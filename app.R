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

# my_dataset <-    "https://energy.ld.admin.ch/sfoe/bfe_ogd17_fuellungsgrad_speicherseen/5"
# get_data(endpoint,query,my_dataset ="https://energy.ld.admin.ch/sfoe/bfe_ogd17_fuellungsgrad_speicherseen/5")
#####################
######## Define UI 
#####################


header <- dashboardHeader(title = "MetadataEditor_DEMO",
                          dropdownMenu(type = "notifications", badgeStatus = "warning",
                                       notificationItem(icon = icon("users"), status = "info",
                                                        "5 new members joined today"
                                       ),
                                       notificationItem(icon = icon("warning"), status = "danger",
                                                        "Resource usage near limit."
                                       ),
                                       notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                                        status = "success", "25 sales made"
                                       ),
                                       notificationItem(icon = icon("user", lib = "glyphicon"),
                                                        status = "danger", "You changed your username"
                                       )
                          ))


sidebar <- dashboardSidebar(
   sidebarUserPanel("Meta-Data-Editor",
                   # subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                   # # Image file should be in www/ subdir
                   # image = "userimage.png"
  ),
  sidebarSearchForm(label = "https://energy.ld.admin.ch/sfoe/bfe_ogd17_fuellungsgrad_speicherseen/5", "caption", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    menuItem("Add Triple", icon = icon("wand-magic-sparkles"),
             textInput("s", "Subject",""),
             textInput("Property", "Property",""),
            # # textInput("URI", "URI",""),
            #  
            #  # textInput("Range", "RANGE",""),
            #  # textInput("ReqLevel", "REQLEVEL",""),
            #  # textInput("Cardinality", "CARDINALITY",""),
             textInput("o", "OBJECT",""),
            # actionButton("update", "Add Entry") ,
            # actionButton("triplemaker", "prepare for download") ,
            
            # actionButton("printer", "printer") ,
             # actionButton(inputId   = "goButton",
             #              label     = "Run Report"),
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
  aniRoi2 <- reactiveVal()
  
  observeEvent(input$searchButton, {
    aniRoi2(  Metadata_Pipeline(get_data(endpoint,query,my_dataset =input$caption), my_dataset = input$caption))})
  
  
  
  output[["x1"]] <- renderDT({
    datatable(
      aniRoi2(), 
      selection = "none", 
      editable = TRUE
      # ,
      # options = list(pageLength = 100, info = FALSE,
      #                lengthMenu = list(c(100, -1), c("100", "All")) ) 
    )
  })
  
  

  
  proxy <- dataTableProxy("x1")
  
  # Update 
  observeEvent(input[["update"]], {
    

    to_add <- data.frame( #this makes a dataframe with the same structre as the chosen DF.
      Property     = input[["Property"]],
      Datatype    = NA, #input[["URI"]],
      Publisher    = NA, #input[["Range"]],
      ReqLevel   = NA, #input[["ReqLevel"]],
      num_min_max_entries = NA, #input[["Cardinality"]],
      o = input[["o"]],
      s = input[["s"]],
      Explanation  = NA,
      stringsAsFactors = FALSE
    )
    
    newAniRoi2 <- rbind(aniRoi2(), to_add) # adding new data
    replaceData(proxy, newAniRoi2, resetPaging = FALSE)
    
    saveRDS(newAniRoi2, "data_entry_form.rds") # save rds 
    aniRoi2(newAniRoi2) # updating data
  })
  
  
  observeEvent(input[["reset"]], {
    #° Cleaning Inputs
    #   updateTextInput(session, "s","s","")
   # updateTextInput(session, "s", "Subject","")
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
      write(triple_generator(aniRoi2()), file)
    }
  )
}

shinyApp(ui, server)


