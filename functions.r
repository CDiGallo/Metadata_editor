








# The functinos -----------------------------------------------------------



# Metadata_pipeline_function Compares the data ----------------------------
Metadata_Pipeline <- function(df, my_dataset){

  df_untouched <- df
  
  # Harmonisierung von schema.org und DCAT ----------------------------------
  
  df <- df_untouched
  
  
  if(!("http://purl.org/dc/terms/title" %in% df$URI)) { #we want to override schema.name if DCAT is not also available
    df$Property <- ifelse(df$URI=="http://schema.org/name", "http://purl.org/dc/terms/title", df$URI)}
  
  if(!("http://purl.org/dc/terms/description" %in% df$URI)) {
    df$Property <- ifelse(df$URI=="http://schema.org/description", "http://purl.org/dc/terms/description", df$URI)}
  
  
  
  
  
  df_reference <- read_csv2("manual_wanted_properties.csv")
  
  
  
  #df_reference <- df_reference[1:5]
  
  #df_reference$s <- 
  
  df_reference
  df
  
  df_join <- left_join(df_reference,df, by=c("Property"= "URI"))
  
  
  
  
  if(5-sum(df_join$Property=="http://purl.org/dc/terms/title")>0){
    for( i in 1:(5-sum(df_join$Property=="http://purl.org/dc/terms/title"))) { #It is 5-sum() because 1:1 still is 1 and gives one addtional row of "Title". As a NUdge to give a title for every language
      df_join <- add_row(df_join, df_join[df_join$Property=="http://purl.org/dc/terms/title",][1,])
    }
  }
  
  if(5-sum(df_join$Property=="http://purl.org/dc/terms/description")>0){
    for( i in 1:(5-sum(df_join$Property=="http://purl.org/dc/terms/description"))) { #It is 4-sum() because 1:1 still is 1 and gives one addtional row of "description" 
      df_join <- add_row(df_join, df_join[df_join$Property=="http://purl.org/dc/terms/description",][1,])
    }
  }
  
  
  
  df_untouched <- df_untouched %>% mutate(Property = URI) %>% select(-URI)
  
  df_combined <- df_join %>% add_row(df_untouched[!(df_untouched$Property %in% df_join$Property),])
  df_combined
  
  
  df_combined$s <- my_dataset
  df_combined <- unique(df_combined)
  
  
  
  
  
  
  # Downloading BlankNodes --------------------------------------------------
  
  blank_nodes <- df_combined$o
  blank_nodes <- str_match(blank_nodes,"^_:genid.+")
  blank_nodes <- blank_nodes[!(is.na(blank_nodes))]
  
  
  
  query <- paste0("
                PREFIX schema: <http://schema.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT * WHERE {

	<", blank_nodes[1],"> ?Property ?o. 
  
  
}")
  querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
  querymanual
  queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
  queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8")
  blank_nodes_content <-  queryres_csv$content %>% textConnection() %>% read.csv #This downloads the first blank node. And it creates "Blank_nodes_content" to be used in the for loop below
  blank_nodes_content$s <- blank_nodes[1]
  blank_nodes_content
  
  
  
  
  
  for(i in 2:length(blank_nodes)){
    query <- paste0("
                PREFIX schema: <http://schema.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT * WHERE {

	<", blank_nodes[i],"> ?Property ?o.
  
  
}")
    querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
    querymanual
    queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
    queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8")
    queryres_content_csv <-  queryres_csv$content %>% textConnection() %>% read.csv
    queryres_content_csv$s <- blank_nodes[i]
    blank_nodes_content <- add_row(blank_nodes_content, queryres_content_csv) 
    
    
  }
  
  
  
  
  # adding the blank_Nodes to the rest of the df ----------------------------
  
  df_combined <- add_row(df_combined,blank_nodes_content)
  df_combined <- unique(df_combined)
  df_combined
  df_combined$ReqLevel <- ifelse(is.na(df_combined$ReqLevel),"not_DCAT",df_combined$ReqLevel)
  
  df_combined <- df_combined %>% mutate(level= (ifelse(ReqLevel=="Mandatory", 1, ifelse(ReqLevel=="Recommended",2,ifelse(ReqLevel=="not_DCAT",3,ifelse(ReqLevel=="Optional",4, NA) )))))
  
  df_combined <-df_combined[order(df_combined$level),]
  
  df_combined <- df_combined %>% select(-level)
  
  df_combined <- df_combined %>%  relocate(o) %>% relocate(Property) %>%  relocate(s)
  
  df_combined

}


# Get_data_function -------------------------------------------------------


get_data <- function(endpoint, query
                     ,my_dataset                     ){
  querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
  querymanual
  queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
  queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8")
  queryres_content_csv <-  queryres_csv$content %>% textConnection() %>% read.csv 
  df <- as_tibble(queryres_content_csv)
  df$s <- my_dataset
  print(df)
  #I add here the "subject" to later being able to convert it easily to triples
}





# Triple_generator --------------------------------------------------------

triple_generator <- function(df){


metadata <- df %>% select("s","Property","o")
metadata


dataset <- metadata[1,1] #This is the dataset
dataset


endpoint <- "https://lindas.admin.ch/query"
proxy_url <- curl::ie_get_proxy_for_url(endpoint)
proxy_config <- use_proxy(url=proxy_url)
query <- paste0("PREFIX schema: <http://schema.org/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT distinct ?g WHERE {
  graph ?g {<",dataset,"> ?p ?o} .
}") #wwe serach for the graph

querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
querymanual
queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8")
queryres_content_csv <-  queryres_csv$content %>% textConnection() %>% read.csv
df <- as_tibble(queryres_content_csv)
df


graph <- df$g[df$g!="https://lindas.admin.ch/meta"] #we want the actual graph. ./meta has every datset (its about the complete metadata so it's wrong)


graph

# adding Datatype ---------------------------------------------------------
metadata <-  metadata[rowSums(is.na(metadata)) == 0,] #If in the excel a value was not filled in, it shows up as "NA". We don't want to write "NA" as an observation in our Database. So we drop it.
################################################
############################### DAS MUSS NOCH MIT DEN SUNODES GEMACHT WERDEN. fANGEN MIT _GENID AN

###################################################################################################

metadata
metadata$s <- ifelse(grepl("^http", metadata$s), paste0("<",metadata$s,">"),paste0("\"",metadata$s,"\"")) #All entries starting with "http" need to have "<>" at start and finish because of lindas (html-datatype). Otherwise it is now a string (better this later)
metadata$URI <- ifelse(grepl("^http", metadata$Property), paste0("<",metadata$Property,">"),paste0("\"",metadata$Property,"\""))
metadata$o <- ifelse(grepl("^http", metadata$o), paste0("<",metadata$o,">"),paste0("\"",metadata$o,"\""))



# Creating all the triples to be written in -------------------------------
### This will be the body of the query


triples <- c(paste(metadata[1,1],metadata[1,2],metadata[1,3], ". \n")) #metadata's structered is 3 columns "s" "p" "o"


for(i in 2:nrow(metadata)) {

  triples <- c(paste(metadata[i,1],metadata[i,2],metadata[i,3], ". \n"),triples)

}

triples <- ifelse(triples== "\"0\" \"0\" \"0\" . \n", "",triples) #The excelfile, can have more references than actual values. These have an output as "0,0,0"

for(i in 1:length(triples)) {

  triples[i] <- paste(triples[i],triples[(i-1)]) #This combines the first with the seoncd row. And then the combined row with the third until the last one.
}
body <- triples[length(triples)] #we combined all the rows and saved it in the row.
triples





# 
# 
# ##################################
# COMBINING EVERYTHING IN A QUERY -----------------------------------------
# ##################################
# 

sparqlQuery <- paste0(
  "PREFIX schema: <http://schema.org/>
PREFIX admin: <https://schema.ld.admin.ch/>
INSERT DATA {GRAPH <",graph,"> { \n ",body,"}")

sparqlQuery
#write(sparqlQuery, "try_delete_later.txt")
}

# # graveyard ---------------------------------------------------------------
# 
# 
#   
#   
#   
#   
#   
#   df_untouched <- df
#   
#   # Harmonisierung von schema.org und DCAT ----------------------------------
#   
#   df <- df_untouched
#   
#   df$Property <- ifelse(df$URI=="http://schema.org/name", "http://purl.org/dc/terms/title", df$URI)
#   
#   df$Property <- ifelse(df$URI=="http://schema.org/description", "http://purl.org/dc/terms/description", df$URI)
#   
#   df_reference <- read_csv2("manual_wanted_properties.csv")
#   
#   df_reference <- df_reference[1:5]
#   
#   #df_reference$s <- 
#   
#   df_join <- left_join(df_reference,df, by=c("URI"= "URI"))
#   
#   df_join
#   
#   df_combined <- df_join %>% add_row(df_untouched[!(df_untouched$URI %in% df_join$URI),])
#   
#   
#   df_combined$s <- my_dataset
#   df_combined <- unique(df_combined)
#   df_combined$ReqLevel <- ifelse(is.na(df_combined$ReqLevel),"not_DCAT",df_combined$ReqLevel)
#   
#   
#   
#   
#   # Downloading BlankNodes --------------------------------------------------
#   
#   blank_nodes <- df_combined$o
#   blank_nodes <- str_match(blank_nodes,"^_:genid.+")
#   blank_nodes <- blank_nodes[!(is.na(blank_nodes))]
#   
#   
#   
#   query <- paste0("
#                 PREFIX schema: <http://schema.org/>
# PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# SELECT * WHERE {
# 
# 	<", blank_nodes[1],"> ?URI ?o. 
#   
#   
# }")
#   querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
#   querymanual
#   queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
#   queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8")
#   blank_nodes_content <-  queryres_csv$content %>% textConnection() %>% read.csv #This downloads the first blank node. And it creates "Blank_nodes_content" to be used in the for loop below
#   blank_nodes_content$s <- blank_nodes[1]
#   blank_nodes_content
#   
#   
#   
#   
#   
#   for(i in 2:length(blank_nodes)){
#     query <- paste0("
#                 PREFIX schema: <http://schema.org/>
# PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
# PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
# SELECT * WHERE {
# 
# 	<", blank_nodes[i],"> ?URI ?o.
#   
#   
# }")
#     querymanual <- paste(endpoint, "?", "query", "=", gsub("\\+", "%2B", URLencode(query, reserved = TRUE)), "", sep = "")
#     querymanual
#     queryres_csv <- GET(querymanual,proxy_config, timeout(60), add_headers(c(Accept = "text/csv")))
#     queryres_csv$content <- stri_encode(queryres_csv$content, from="UTF-8",to="UTF-8")
#     queryres_content_csv <-  queryres_csv$content %>% textConnection() %>% read.csv
#     queryres_content_csv$s <- blank_nodes[i]
#     blank_nodes_content <- add_row(blank_nodes_content, queryres_content_csv) 
#     
#     
#   }
#   
#   
#   
#   
#   # adding the blank_Nodes to the rest of the df ----------------------------
#   
#   df_combined <- add_row(df_combined,blank_nodes_content)
#   df_combined <- unique(df_combined)
#   
# }
# 

