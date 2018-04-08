#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(dplyr)
library(DT)
Sys.setenv(TZ='Asia/Taipei')


load("typhoon.tokenized.rda")
tokenized.df %>% str()
df <- tokenized.df %>%
    select(title, content = text, cat, timestamp)



# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Text Search"),

    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "input_content", label = "Content"),
            submitButton(text = "Submit")
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("Results", DT::dataTableOutput("output_table")),
                tabPanel("ShowTitleOnly", DT::dataTableOutput("output_title"))
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output){

    search.criteria1 <- reactive({
        res_content <- c()
        if(input$input_content != ""){
            res_content <- which(grepl(input$input_content, df$content))
        }
    })


    output$output_table <- DT::renderDataTable({
        outdata <- df[search.criteria1(),]
        DT::datatable(outdata)
    })
})

# Run the application
shinyApp(ui = ui, server = server)

