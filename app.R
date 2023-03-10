library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library(tidyverse)
#library(SPARQLchunks)

library(curl)
library(httr)
library(stringi)


source("functions.r")
endpoint <- "https://lindas.admin.ch/query"
proxy_url <- curl::ie_get_proxy_for_url(endpoint)
proxy_config <- use_proxy(url=proxy_url)
title <- title_generator(endpoint)

dataset_list <- title
title$dataset[title$title=="Staatsrechnungen - Bereich"][1]
title$dataset

name_list <- dataset_list[3]
name_list


dataset_list["title"]

# my_dataset <-    "https://energy.ld.admin.ch/sfoe/bfe_ogd17_fuellungsgrad_speicherseen/5"
# get_data(endpoint,query,my_dataset ="https://energy.ld.admin.ch/sfoe/bfe_ogd17_fuellungsgrad_speicherseen/5")
#####################
######## Define UI 
#####################


header <- dashboardHeader(title = "MetadataEditor_DEMO")


sidebar <- dashboardSidebar(
  sidebarUserPanel("Meta-Data-Editor",
  ),
  radioButtons("radio", label = h3("Endpoint"),choices = list("Prod" = "https://lindas.admin.ch/query", "INT" = "https://int.lindas.admin.ch/query"), 
               selected = "https://lindas.admin.ch/query"),
  #sidebarSearchForm(label = "https://energy.ld.admin.ch/sfoe/bfe_ogd17_fuellungsgrad_speicherseen/5", "caption", "searchButton"),
  sidebarMenu(
    id = "tabs",
    menuItem("select dataset from list", icon= icon("folder-open"), 
             selectInput("drop_down", "Select Dataset", choices = "this_changes_nothing_look_in_server",width = "1600px")),
    menuItem("Add Triple", icon = icon("wand-magic-sparkles"),
             textInput("s", "Subject",""),
             textInput("Property", "Predicate",""),
             textInput("o", "Object",""),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 actionButton("update", "Add triple")),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 actionButton("reset", "Clear"))#, style='padding:6px;width:80px'))
    ),
    menuItem("Download", icon =icon( "download"), startExpanded = TRUE, 
             downloadButton("downloadData", "Download"))
    ))
body <- dashboardBody(DTOutput("x1"))

ui <- dashboardPage(header, 
                    sidebar, 
                    body)
##################$###############
########## SERVER ##############


server <- function(input, output, session)  {

  observe({
    updateSelectInput(
      session,
      "drop_down",
      choices = {
        dataset_list <- title_generator(input$radio)[3]} # For more information how this should work, but does not update the dataset look here: https://shiny.oxshef.io/tutorials_controls-dependent-on-data.html
    )
  })
  
  aniRoi2 <- reactiveVal()
  
  observeEvent(input$searchButton, {
    aniRoi2(  Metadata_Pipeline(get_data(query,my_dataset =input$caption),endpoint = endpoint, my_dataset = input$caption))})
  observeEvent(input$drop_down, {
    aniRoi2(  Metadata_Pipeline(get_data(query,my_dataset =title$dataset[title$title==input$drop_down][1]), my_dataset =title$dataset[title$title==input$drop_down][1],endpoint = endpoint))})

  output[["x1"]] <- renderDT({
    datatable(
      aniRoi2(), 
      selection = "none", 
      editable = TRUE
    )
  })
  proxy <- dataTableProxy("x1")
  
  # Update 
  observeEvent(input[["update"]], {
    to_add <- data.frame( #this makes a dataframe with the same structre as the chosen DF.
      Predicate     = input[["Property"]],
      Datatype    = NA, #input[["URI"]],
      Publisher    = NA, #input[["Range"]],
      ReqLevel   = NA, #input[["ReqLevel"]],
      Cardinality = NA, #input[["Cardinality"]],
      Object = input[["o"]],
      Subject = input[["s"]],
      Explanation  = NA,
      stringsAsFactors = FALSE
    )
    newAniRoi2 <- rbind(aniRoi2(), to_add) # adding new data
    replaceData(proxy, newAniRoi2, resetPaging = FALSE)
    
    saveRDS(newAniRoi2, "data_entry_form.rds") # save rds 
    aniRoi2(newAniRoi2) # updating data
  })
  
  observeEvent(input[["reset"]], {
    updateTextInput(session, "o", "OBJECT","")
    updateTextInput(session, "Property", "Property","")
    
  })
  
  observeEvent(input[["x1_cell_edit"]], { 
    info <- input[["x1_cell_edit"]]
    newAniroi2 <- 
      editData(aniRoi2(), info, proxy, rownames = TRUE, resetPaging = FALSE)
    aniRoi2(newAniroi2)
    saveRDS(newAniroi2, "data_entry_form.rds") # save rds
  })
  
  observeEvent(input[["printer"]], {
    print(newAniRoi2())
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".ttl", sep="")
    },
    content = function(file) {
      write(triple_generator(aniRoi2(),endpoint = endpoint), file)
    }
  )
  
}

shinyApp(ui, server)


