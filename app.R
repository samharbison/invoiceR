library(shiny)
library(shinyjs)
library(kableExtra)
library(webshot)
#source("editDT.R")

ui = fluidPage(title = "invoiceR",
               sidebarLayout(sidebarPanel(
                 h4("Sam Harbison's InvoiceR"),
                 tabsetPanel(
                    tabPanel(
                      "Title, To, From",
                      textAreaInput("to", "Invoice To:"),
                      textAreaInput("from", "Personal Info", value = "Sam Harbison\n1526 SE Hawthorne Blvd.\nPortland, Oregon 97214\n502-291-4450\nsam.harbison1@gmail.com", height = '105px'),
                      textInput("title_in", "Invoice Title")
                    ),
                   tabPanel(
                     "Line Items",
                     div(
                       shinyjs::useShinyjs(),
                       id = "side-panel",
                       fluidRow(h3(paste0("Add Job")),
                                column(12,
                                       uiOutput((
                                         'company'
                                       )))),
                       fluidRow(column(12,
                                       uiOutput((
                                         "date_completed"
                                       )))),
                       fluidRow(column(12,
                                       uiOutput((
                                         "description"
                                       )))),
                       fluidRow(column(12,
                                       uiOutput((
                                         "hours"
                                       )))),
                       fluidRow(column(12,
                                       uiOutput((
                                         "charge"
                                       )))),
                       fluidRow(column(4,
                                       actionButton(
                                         "add", "Add Question!"
                                       )))
                     )
                   ),
                   tabPanel(
                     "Make",
                     br(),
                     p("Make the R Markdown"),
                     actionButton("make", 'Make')
                   )
                   
                 )
               ),
               mainPanel(br(),
                         tabsetPanel(
                           tabPanel("Datatable",
                                    dataTableOutput("datTab")),
                           tabPanel(
                             "Markdown Readout",
                             br(),
                             downloadButton("download", "Download png"),
                             br(),
                             uiOutput("file")
                           )
                         )
                         )
               ))


server = function(input, output, session) {
    meta = reactiveValues(stuff = list(meta = NULL,
                                       questions = tibble()))
    btn = reactiveValues(value = 1)
    
    observe({
      meta$stuff$meta = list(to=input$to,
                             from=input$from,
                             title_in = input$title_in)
    })
    
    output$company <- renderUI({
        selectInput(
            inputId = ("company"),
            label = "Choose company that work was done for.",
            choices = list(
                "PK USA (Shelbyville)" = "PK USA (Shelbyville)",
                "PK TN" = "PK TN",
                "PK MS" = "PK MS",
                "Vernet" = "Vernet",
                "Pacers" = "Pacers",
                "Shelby Materials" = "Shelby Materials",
                "Bowen" = "Bowen"
            ),
            width = '100%'
        )
    })
    
    output$date_completed <- renderUI({
        dateInput(
            inputId = ("date_completed"),
            label = "Date Completed",
            width = '100%'
        )
    })
    
    output$description <- renderUI({
        textInput(
            inputId = ("description"),
            label = "Describe work done for company.",
            width = '100%'
        )
    })
    
    output$hours <- renderUI({
        textInput(inputId = ("hours"),
                  label = "How many hours did you work on this job?",
                  width = '100%')
    })
    
    output$charge <- renderUI({
        textInput(
            inputId = ("charge"),
            label = "Hourly rate?",
            width = '100%',
            value = 42
        )
    })
    
    
    observeEvent(input$add, {
        meta$stuff$questions = bind_rows(meta$stuff$questions,
                                         bind_cols(
                                             "Date Completed" = as.Date(input$date_completed, "%Y-%m-%d"),
                                             tibble(
                                                 "Company" = input$company,
                                                 "Description" = input$description,
                                                 "Hours" = as.numeric(input$hours),
                                                 "Rate" = dollar(as.numeric(input$charge)),
                                                 "Total" = as.numeric(input$hours) * as.numeric(input$charge)
                                             )
                                         ))
        
        
        btn$value <<- btn$value + 1
        reset("side-panel")
    })
    
    
    output$outLS <- renderPrint({
        print(meta$stuff)
    })
    
    output$datTab <- renderDataTable({
      if (nrow(meta$stuff$questions) >0){
      bind_rows(meta$stuff$questions,
                tibble(
                  "Company" = NA,
                  "Description" = NA,
                  "Hours" = sum(as.numeric(meta$stuff$questions$Hours)),
                  "Rate" = NA,
                  "Total" = sum(as.numeric(meta$stuff$questions$Total))
                ))}
      })
  
    
    # output$totTab <- renderDataTable(
    #   data_frame(sum(meta$stuff$questions$total)),
    # rownames = F)
    
    observeEvent(input$make,{
        
        p = list(set_title = meta$stuff$meta$title_in,
                 to = meta$stuff$meta$to,
                 #to = gsub("\n", "  \n",meta$stuff$meta$to),
                 from = meta$stuff$meta$from,
                 tab =  bind_rows(meta$stuff$questions,
                                  tibble(
                                    "Company" = NA,
                                    "Description" = NA,
                                    "Hours" = sum(as.numeric(meta$stuff$questions$Hours)),
                                    "Rate" = NA,
                                    "Total" = sum(as.numeric(meta$stuff$questions$Total))
                                  )))

        tmp = tempdir()
        filename2 <<- paste(gsub(" ", "", meta$stuff$meta$title_in),Sys.Date(),"invoice", sep="_")
        tempReport = file.path('invoice.Rmd')
        rmarkdown::render(tempReport,
                          output_file = filename2,
                          output_dir = tmp,
                          params = p)
        output$file = renderUI({
          includeHTML(paste(tmp, paste(filename2,".html", sep = ""), sep ="/"))
        })
        
    })
    
    output$download <- downloadHandler(
      filename = function(){
        paste(filename2, ".png", sep = "")
      },
      content = function(file) {
        webshot(url = paste(tmp, paste(filename2,".html", sep = ""), sep ="/"), file = file)
      }
    )
    
    
    
    
    
    
}
shinyApp(ui = ui, server = server)
