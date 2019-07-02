
# Inserting quetions Module -----------------------------------------------


# inputs UI ---------------------------------------------------------------

inputsUI <- function(id) {
  ns = NS(id)
  
  tags$div(id = paste0('input', id),
           fluidPage(
             fluidRow(h3(paste0("Question ", id)),
                      column(12,
                             uiOutput(ns(
                               'company'
                             )))),
             fluidRow(column(12,
                             uiOutput(ns(
                               "description"
                             )))),
             fluidRow(column(12,
                             uiOutput(ns(
                               "hours"
                             )))),
             fluidRow(column(12,
                             uiOutput(ns(
                               "charge"
                             ))))
             
           ))
}


# inputs Server -----------------------------------------------------------

inputsServer <- function(input, output, session) {
  ns <- session$ns
  
  output$company <- renderUI({
    selectInput(
      inputId = ns("company"),
      label = "Choose company that work was done for.",
      choices = list("PK USA (Shelbyville)" = "PK USA (Shelbyville)",
                     "PK TN" = "PK TN",
                     "PK MS" = "PK MS",
                     "Vernet" = "Vernet",
                     "Pacers" = "Pacers",
                     "Shelby Materials" = "Shelby Materials",
                     "Bowen" = "Bowen"),
      width = '100%'
    )
  })
  
  output$description <- renderUI({
    textInput(
      inputId = ns("description"),
      label = "Describe work done for company.",
      width = '100%'
    )
  })
  
  output$hours <- renderUI({
    textInput(
      inputId = ns("hours"),
      label = "How many hours did you work on this job?",
      width = '100%'
    )
  })
  
  output$charge <- renderUI({
    textInput(
      inputId = ns("charge"),
      label = "Hourly rate?",
      width = '100%',
      value = 42
    )
  })
  
  
}
