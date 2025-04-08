library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(rio)
library(plotly)

ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  
  uiOutput("all_data_UI")
)

server <- function(input, output){
  de_table_in <- reactive({
    req(input$de_file)
    rio::import(input$de_file) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
  
  output$all_data_UI <- renderUI({ 
    req(de_table_in)
    card(card_header("All genes"), dataTableOutput(outputId = "all_data"))
  }) 
  
  output$all_data = renderDataTable(datatable(de_table_in()))
}

shinyApp(ui = ui, server = server)
