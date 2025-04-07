params <-
list(isSlides = "no")

## ----include=FALSE------------------------------------------------------------
suppressPackageStartupMessages(require(knitr))
knitr::opts_chunk$set(echo = TRUE, tidy = T)


## ----echo=F, eval=T, message = F, warning=F-----------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(rio)
library(plotly)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Session 3 - Improving app experience - dynamic UIs and user feedback

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("#  Session 3 - Improving app experience - dynamic UIs and user feedback

---
"    
  )
  
}



## ----echo=T, eval=T-----------------------------------------------------------

ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), #<<
  
  dataTableOutput(outputId = "all_data"),
)

server <- function(input, output){
  de_table_in <- reactive({
    rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$all_data = renderDataTable({
    datatable(de_table_in(),
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })

}



## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  dataTableOutput(outputId = "all_data"),
)

server <- function(input, output){
  de_table_in <- reactive({
    rio::import(input$de_file$datapath) %>%  #<<
      dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$all_data = renderDataTable({
    datatable(de_table_in(),
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })

}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------

server <- function(input, output){
  
  de_table_in <- reactive({
    req(input$de_file)  #<<
    rio::import(input$de_file$datapath) %>%
      dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$all_data = renderDataTable({
    datatable(de_table_in(),
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=F, eval=T, out.width="75%"------------------------------------------
# grab the custom theme from the previous session
custom_css <- "
  .card-header {
    background-color: #d3dff1;
    border-bottom: 2px solid #273449;
  }
"

# Create theme with custom CSS
custom_theme <- bs_theme(
  version = 5,
  bg = "white",
  fg = "#273449",
  primary = "#5886b2",
  secondary = "#95a5a6",
  success = "#18bc9c",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  preset = "bootstrap",
  "navbar-bg" = "#5886b2"
) |> bs_add_rules(custom_css)



## ----echo=T, eval=T, tidy=FALSE-----------------------------------------------

ui <- page_navbar(
  title = "RNAseq tools",
  theme = custom_theme,
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        # >>>>>>>>>
        fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
        # >>>>>>>>>
        numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005),
        
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
        
        actionButton("de_filter", "Apply filter"),
        br(),
        br(),
        value_box(title = "Number of genes that go up:", value = textOutput("num_up"), 
                  showcase = icon("arrow-up"), 
                  theme = value_box_theme(bg = "#22b430")), 
        value_box(title = "Number of genes that go down:", value = textOutput("num_down"), 
                  showcase = icon("arrow-down"), 
                  theme = value_box_theme(bg ="#c34020" ))
      ),
      
      layout_columns(
        navset_card_tab(
          title = "DE result tables",
          nav_panel(card_header("DEGs"), dataTableOutput(outputId = "de_data")),
          nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))
        ),
        card(card_header("MA plot"),
             plotlyOutput("ma_plot"),
             downloadButton("download_ma_plot", "Download MA plot", style = "width:40%;")), 
        card(card_header("Volcano plot"),
             plotlyOutput("volcano_plot"),
             downloadButton("download_volcano_plot", "Download volcano plot", style = "width:40%;")), 
        col_widths = c(12,6,6), row_heights = c("750px", "500px")
      )
    )
  ),
  nav_panel(title = "Next steps","The next step in our analysis will be..."),
  nav_spacer(),
  nav_menu(title = "Links",
           align = "right",
           nav_item(tags$a(shiny::icon("chart-simple"), "RU BRC - Learn more!", href = "https://rockefelleruniversity.github.io/",target = "_blank"))
  )
)




## ----echo=T, eval=T, out.width="75%"------------------------------------------

server = function(input, output) {
  
  # >>>>>>>>>>>>>>>>>>>>>>>>
  de_table_in <- reactive({ 
    req(input$de_file)
    rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  # >>>>>>>>>>>>>>>>>>>>>>>>
  
  output$all_data = renderDataTable({
    datatable(de_table_in(), # >>>>>>>>>>>>>>>>>>>>>>>>
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  filtered_de <- reactive({
    de_table_in() %>% # >>>>>>>>>>>>>>>>>>>>>>>>
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
  }) %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) # >>>>>>>>>>>>>>>>>>>>>>>>
  
  output$de_data = renderDataTable({
    datatable(filtered_de(), 
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  num_up_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange > 0 & padj < 0.05) %>% nrow) 
  num_down_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange < 0 & padj < 0.05) %>% nrow) 
  output$num_up <- renderText(num_up_genes())
  output$num_down <- renderText(num_down_genes())
  
  ma_plot_reac <- reactive({
    de_table_in() %>% # >>>>>>>>>>>>>>>>>>>>>>>>
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig, label = Symbol)) + geom_point() +
      scale_x_log10() + scale_color_manual(name = "DE status", values = c("red", "grey")) +
      xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) # >>>>>>>>>>>>>>>>>>>>>>>>
  
  output$ma_plot = renderPlotly({
    ggplotly(ma_plot_reac())
  }) 
  
  volcano_plot_reac <- reactive({
    de_table_in() %>% # >>>>>>>>>>>>>>>>>>>>>>>>
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
      ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig)) +
      geom_point() +
      scale_color_manual(name = "DE status", values = c("red","grey")) + theme_bw()  
    
  }) %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) # >>>>>>>>>>>>>>>>>>>>>>>>
  
  output$volcano_plot = renderPlotly({
    ggplotly(volcano_plot_reac())
  }) 
  
  output$download_ma_plot <- downloadHandler(
    filename = function() {
      "maplot.pdf"
    },
    content = function(file) {
      ggsave(filename = file, plot = ma_plot_reac())
    }
  )
  
  output$download_volcano_plot <- downloadHandler(
    filename = function() {
      "volcanoplot.pdf"
    },
    content = function(file) {
      ggsave(filename = file, plot = volcano_plot_reac())
    }
  )
  
  
}


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
   uiOutput("sidebar_filters_UI"), #<<
)

server <- function(input, output){
  de_table_in <- reactive({
    req(input$de_file)
    rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$sidebar_filters_UI <- renderUI({ 
    req(de_table_in())
      div(numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.001),
          numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
          actionButton("de_filter", "Apply filter"))
  })
}



## ----echo=T, eval=T-----------------------------------------------------------
library(rio)

ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
   uiOutput("sidebar_filters_UI"), 
)

server <- function(input, output){
  de_table_in <- reactive({
    req(input$de_file)
    rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$sidebar_filters_UI <- renderUI({ #<<
    req(de_table_in()) #<<
    div(numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.001),#<<
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),#<<
        actionButton("de_filter", "Apply filter"))#<<
  })#<<
}



## ----echo = T, eval=F---------------------------------------------------------
# server <- function(input, output){
#   de_table_in <- reactive({
#     req(input$de_file)
#     rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
# 
#   output$sidebar_filters_UI <- renderUI({
#     req(de_table_in())
#     div( #<<
#       numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.001),
#       numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
#       actionButton("de_filter", "Apply filter")
#     ) #<<
#   })
# }


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  
  uiOutput("all_data_UI")  #<<
)

server <- function(input, output){
  de_table_in <- reactive({
    req(input$de_file)
    rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
  
  output$all_data_UI <- renderUI({ #<<
    if(is.null(input$de_file)) { #<<
      div("You must load data!", style = "color: #273449; font-weight: bold;") #<<
    }else if(!is.null(de_table_in())){ #<<
      navset_card_tab(nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))) #<<
    } #<<
  }) #<<
  
  output$all_data = renderDataTable(datatable(de_table_in()))
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui , server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_fluid(
  checkboxInput("question", "Do you want to use my app?"),
  conditionalPanel(condition = "input.question == '1'",
                   selectInput("experiment", "What kind of experiment is this?",
                                choices = c("", "RNAseq", "ATACseq")),
                   conditionalPanel(condition = "input.experiment == 'RNAseq'",
                                    fileInput("file_in", "Great!, upload your RNAseq file:")),
                   conditionalPanel(condition = "input.experiment == 'ATACseq'",
                                    "Sorry, but this app won't help you"))
  
)

server = function(input, output){}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_navbar(
  title = "RNAseq tools",
  theme = custom_theme,
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
        uiOutput("sidebar_filters_UI") # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ),
      uiOutput("table_plots_UI"), # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    )
  ),
  nav_panel(title = "Next steps","The next step in our analysis will be..."),
  nav_spacer(),
  nav_menu(title = "Links",
           align = "right",
           nav_item(tags$a(shiny::icon("chart-simple"), "RU BRC - Learn more!", href = "https://rockefelleruniversity.github.io/",target = "_blank"))
  )
)



## ----echo=T, eval=T, out.width="75%"------------------------------------------

server = function(input, output) {
  
  de_table_in <- reactive({
    req(input$de_file)
    file_in <- rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  output$table_plots_UI <- renderUI({
    if(is.null(input$de_file)) { 
      layout_columns("No data has been loaded! Upload a DE table with the following columns: 'baseMean', 'log2FoldChange', 'lfcSE', 'stat', 'pvalue', 'padj'", style = "color: #273449; font-weight: bold;")
    }else if(!is.null(de_table_in())){ 
      layout_columns(
        navset_card_tab(
          title = "DE result tables",
          nav_panel(card_header("DEGs"), dataTableOutput(outputId = "de_data")),
          nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))
        ),
        card(card_header("MA plot"),
             plotlyOutput("ma_plot"),
             downloadButton("download_ma_plot", "Download MA plot", style = "width:40%;")), 
        card(card_header("Volcano plot"),
             plotlyOutput("volcano_plot"),
             downloadButton("download_volcano_plot", "Download volcano plot", style = "width:40%;")),
        col_widths = c(12,6,6), row_heights = c("750px", "500px")
      )
      }
  })
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  output$sidebar_filters_UI <- renderUI({
    req(de_table_in())
      div(
        "DE filters",
        numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.001),
        
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
        
        actionButton("de_filter", "Apply filter"),
        br(),
        br(),
        value_box(title = "Number of genes that go up:", value = textOutput("num_up"), 
                  showcase = icon("arrow-up"), 
                  theme = value_box_theme(bg = "#22b430")), 
        value_box(title = "Number of genes that go down:", value = textOutput("num_down"), 
                  showcase = icon("arrow-down"), 
                  theme = value_box_theme(bg ="#c34020" ))
      )
  })
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  output$all_data = renderDataTable({
    datatable(de_table_in(), filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  filtered_de <- reactive({
    de_table_in() %>% 
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
  }) %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE)

  num_up_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange > 0 & padj < 0.05) %>% nrow) 
  num_down_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange < 0 & padj < 0.05) %>% nrow) 
  output$num_up <- renderText(num_up_genes())
  output$num_down <- renderText(num_down_genes())
  
  output$de_data = renderDataTable({
    datatable(filtered_de(),
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
    ma_plot_reac <- reactive({
      de_table_in() %>% 
        dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
         ggplot(aes(x = baseMean, y = log2FoldChange, color = sig, label = Symbol)) + geom_point() +
          scale_x_log10() + scale_color_manual(name = "DE status", values = c("red", "grey")) +
          xlab("baseMean (log scale)") + theme_bw()
  })  %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) 

    output$ma_plot = renderPlotly({
      ggplotly(ma_plot_reac())
    }) 
  
    volcano_plot_reac <- reactive({
        de_table_in() %>% 
          dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
          ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig, label = Symbol)) +
            geom_point() +
            scale_color_manual(name = "DE status", values = c("red","grey"),) + theme_bw() 
      
    }) %>%
      bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) 
  
    output$volcano_plot = renderPlotly({
      ggplotly(volcano_plot_reac())
    })
  
    # download handlers for plots
    output$download_ma_plot <- downloadHandler(
    filename = function() {
      "maplot.pdf"
    },
    content = function(file) {
      ggsave(filename = file, plot = ma_plot_reac())
    }
  )
  
  output$download_volcano_plot <- downloadHandler(
    filename = function() {
      "volcanoplot.pdf"
    },
    content = function(file) {
      ggsave(filename = file, plot = volcano_plot_reac())
    }
  )
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# User Feedback and observers

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# User Feedback and observers

---
"    
  )
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  uiOutput("all_data_UI")
)

server <- function(input, output){
  de_table_in <- reactive({
    req(input$de_file)
    file_in <- rio::import(input$de_file$datapath)
    validate(need(expr = all(c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj") %in% colnames(file_in)), #<<
                  message = "You must have the following columns: 'baseMean', 'log2FoldChange', 'lfcSE', 'stat', 'pvalue', 'padj'")) #<<
    file_in %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$all_data_UI <- renderUI({
    if(is.null(input$de_file)) {
      div("You must load data!", style = "color: #273449; font-weight: bold;")
    }else if(!is.null(de_table_in())){ 
      navset_card_tab(nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data")))
    }
  })
  
  output$all_data = renderDataTable(datatable(de_table_in()))
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  uiOutput("all_data_UI"))

server <- function(input, output){
  de_table_in <- reactive({
    req(input$de_file)
    file_in <- rio::import(input$de_file$datapath)
  })
  
  observe({ #<<
    showNotification("A new table has been loaded into the app!", duration = NULL, type = "message") #<<
  }) %>% #<<
    bindEvent(de_table_in()) #<<
  
  output$all_data_UI <- renderUI({
    if(is.null(input$de_file)) {
      div("Load data!", style = "color: #273449; font-weight: bold;")
    }else{ navset_card_tab(nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))) }
  })
  
  output$all_data = renderDataTable(datatable(de_table_in()))
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, message=F------------------------------------------------

ui <- page_fluid(
  
  actionButton("show_modal", 
               "Click me for modal!")
)

server <- function(input, output){
  observe({
    showModal(
      modalDialog(
        title = "The button was clicked!",
        easyClose = TRUE, 
        footer = modalButton("Dismiss")))
  }) %>% bindEvent(input$show_modal)
}


## ----echo = T, eval=F---------------------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, message=F------------------------------------------------
de_table <- read.csv("data/shP53_vs_control_DEG.csv")
de_table$negLog10_pval <- -log10(de_table$pvalue)

ui <- page_fluid(
  
  plotOutput("volcano_plot"),
  actionButton("volcano_download_modal", "Download volcano plot")
)



## ----echo=T, eval=T, message=F------------------------------------------------
server <- function(input, output){
  volcano_plot_reac <- reactive(ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval, text = Symbol)) + geom_point() + theme_bw())
  
  output$volcano_plot = renderPlot(ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval, text = Symbol)) + geom_point() + theme_bw()) 
  
  observe({
    showModal(
      modalDialog(title = "Are you sure you want to download this plot? If so, provide a name for the file.",
                  textInput("file_name", "Name for file (no extension):", value = "volcano_modal"), #<<
                  downloadButton("confirm_download", "Download plot"), #<<
                  modalButton("Don't download plot"),
                  easyClose = TRUE, footer = NULL))
  }) %>% bindEvent(input$volcano_download_modal)
  
  output$confirm_download <- downloadHandler(
    filename = function() {
      paste0(input$file_name, ".pdf") #<<
    }, content = function(file) { 
      ggsave(filename = file, plot = volcano_plot_reac()) 
    }) 
}


## ----echo = T, eval=F---------------------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
ui <- page_navbar(
  title = "RNAseq tools",
  theme = custom_theme,
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      
      sidebar = sidebar(
        width = 300,
        fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
        uiOutput("sidebar_filters_UI") 
      ),
      uiOutput("table_plots_UI"),
    )
  ),
  nav_panel(title = "Next steps","The next step in our analysis will be..."),
  nav_spacer(),
  nav_menu(title = "Links",
           align = "right",
           nav_item(tags$a(shiny::icon("chart-simple"), "RU BRC - Learn more!", href = "https://rockefelleruniversity.github.io/",target = "_blank"))
  )
)



## ----echo=T, eval=T, out.width="75%"------------------------------------------

server = function(input, output) {
  
  de_table_in <- reactive({
    req(input$de_file)
    file_in <- rio::import(input$de_file$datapath)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    validate(
      need(expr = all(c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj") %in% colnames(file_in)), 
           message = "You must have the following columns: 'baseMean', 'log2FoldChange', 'lfcSE', 'stat', 'pvalue', 'padj'")
    )
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    file_in %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  observe({
      showNotification("A new table has been loaded into the app!", duration = NULL, type = "message")
  }) %>%
    bindEvent(de_table_in())
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  output$sidebar_filters_UI <- renderUI({
    req(de_table_in())
      div(
        "DE filters",
        numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.001),
        
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
        
        actionButton("de_filter", "Apply filter"),
        br(),
        br(),
        value_box(title = "Number of genes that go up:", value = textOutput("num_up"), 
                  showcase = icon("arrow-up"), 
                  theme = value_box_theme(bg = "#22b430")), 
        value_box(title = "Number of genes that go down:", value = textOutput("num_down"), 
                  showcase = icon("arrow-down"), 
                  theme = value_box_theme(bg ="#c34020" ))
      )
  })
  
  output$table_plots_UI <- renderUI({
    if(is.null(input$de_file)) { 
      layout_columns("No data has been loaded! Upload a DE table with the following columns: 'baseMean', 'log2FoldChange', 'lfcSE', 'stat', 'pvalue', 'padj'", style = "color: #273449; font-weight: bold;")
    }else if(!is.null(de_table_in())){ 
      layout_columns(
        navset_card_tab(
          title = "DE result tables",
          nav_panel(card_header("DEGs"), dataTableOutput(outputId = "de_data")),
          nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))
        ),
        card(card_header("MA plot"),
             plotlyOutput("ma_plot"),
             actionButton("ma_download_modal", "Download MA plot", width = "40%")),# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        card(card_header("Volcano plot"),
             plotlyOutput("volcano_plot"),
             actionButton("volcano_download_modal", "Download volcano plot", width = "40%")), # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        col_widths = c(12,6,6), row_heights = c("750px", "500px")
      )
      }
  })
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  observe({
    showModal(
      modalDialog(title = "Are you sure you want to download this plot? If so, provide a name for the file.",
                  textInput("file_name_ma", "Name for file (no extension):", value = "ma_plot"), 
                  downloadButton("download_ma_plot", "Download MA plot", style = "width:40%;"),
                  br(),
                  br(),
                  modalButton("Don't download plot"),
                  easyClose = TRUE, footer = NULL))
  }) %>% 
    bindEvent(input$ma_download_modal)
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  observe({
    showModal(
      modalDialog(title = "Are you sure you want to download this plot? If so, provide a name for the file.",
                  textInput("file_name_volcano", "Name for file (no extension):", value = "volcano_plot"), 
                  downloadButton("download_volcano_plot", "Download plot", style = "width:40%;"),
                  br(), 
                  br(),
                  modalButton("Don't download plot"),
                  easyClose = TRUE, footer = NULL))
  }) %>% 
    bindEvent(input$volcano_download_modal)
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  output$all_data = renderDataTable({
    datatable(de_table_in(), filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  
  filtered_de <- reactive({
    de_table_in() %>% 
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
  }) %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE)

  num_up_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange > 0 & padj < 0.05) %>% nrow) 
  num_down_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange < 0 & padj < 0.05) %>% nrow) 
  output$num_up <- renderText(num_up_genes())
  output$num_down <- renderText(num_down_genes())
  
  output$de_data = renderDataTable({
    datatable(filtered_de(),
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
    ma_plot_reac <- reactive({
      de_table_in() %>% 
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig, label = Symbol)) + geom_point() +
      scale_x_log10() + scale_color_manual(name = "DE status", values = c("red", "grey")) +
      xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) 

    output$ma_plot = renderPlotly({
      ggplotly(ma_plot_reac())
    }) 
  
    volcano_plot_reac <- reactive({
        de_table_in() %>% 
          dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
          ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig, label = Symbol)) +geom_point() +
          scale_color_manual(name = "DE status", values = c("red","grey"),) +theme_bw()
    }) %>%
      bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) 
  
    output$volcano_plot = renderPlotly({
      ggplotly(volcano_plot_reac())
    })
    
    output$download_ma_plot <- downloadHandler(
    filename = function() {
      paste0(input$file_name_ma, ".pdf")
    },
    content = function(file) {
      ggsave(filename = file, plot = ma_plot_reac())
    }
  )
  
  output$download_volcano_plot <- downloadHandler(
    filename = function() {
      paste0(input$file_name_volcano, ".pdf")
    },
    content = function(file) {
      ggsave(filename = file, plot = volcano_plot_reac())
    }
  )
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
de_table <- read.csv("data/shP53_vs_control_DEG.csv")

ui <- page_fluid(
  
  dataTableOutput(outputId = "all_data"),
  actionButton("add_gene", "Add gene to list"),
  actionButton("remove_gene", "Remove gene from list"),
  br(),
  br(),
  textOutput("selected_gene_info")
)



## ----echo=T, eval=T-----------------------------------------------------------

server <- function(input, output){
  # initialize the reactiveValues object
  rv <- reactiveValues(genes = character()) #<< 
  
  output$all_data = renderDataTable(datatable(de_table, selection = "single", filter = 'top'))
  
  observe({
    selected_gene <- de_table[input$all_data_rows_selected, "Symbol"]
    rv$genes <- c(rv$genes, selected_gene) #<<
  }) %>% bindEvent(input$add_gene) # observer when 'add_gene' button is pressed

  observe({
    selected_gene <- de_table[input$all_data_rows_selected, "Symbol"]
    rv$genes <- rv$genes[!rv$genes == selected_gene] #<<
  }) %>% bindEvent(input$remove_gene) # observer when 'remove_gene' button is pressed
  
  output$selected_gene_info <- renderText({
    paste0("The selected genes are ", paste(rv$genes, collapse = ", ")) 
  })
}



## ----echo=T, eval=T-----------------------------------------------------------

server <- function(input, output){
  
  rv <- reactiveValues(genes = character())
  
  output$all_data = renderDataTable(datatable(de_table, selection = "single", filter = 'top'))
  
  observe({
    selected_gene <- de_table[input$all_data_rows_selected, "Symbol"]
    rv$genes <- c(rv$genes, selected_gene) 
  }) %>% bindEvent(input$add_gene)
    
  observe({
    selected_gene <- de_table[input$all_data_rows_selected, "Symbol"]
    rv$genes <- rv$genes[!rv$genes == selected_gene] 
  }) %>% bindEvent(input$remove_gene)
  
  output$selected_gene_info <- renderText({ 
    paste0("The selected genes are ", paste(rv$genes, collapse = ", ")) #<<
  }) 
}



## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui, server)


## ----echo=T, eval=T, message=F, warning=F-------------------------------------
library(rsconnect)


## ----echo=T, eval=F-----------------------------------------------------------
# ui <- page_fluid(
#   fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")),
#   uiOutput("all_data_UI")
# )
# 
# server <- function(input, output){
# 
#   de_table_in <- reactive({
#     req(input$de_file)
#     rio::import(input$de_file) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
# 
#   output$all_data_UI <- renderUI({
#     req(de_table_in)
#     card(card_header("All genes"), dataTableOutput(outputId = "all_data"))
#   })
# 
#   output$all_data = renderDataTable(datatable(de_table_in()))
# }
# 
# shinyApp(ui = ui, server = server)
# 


## ----echo=T, eval=F-----------------------------------------------------------
# # UI object not shown
# server <- function(input, output){
# 
#   observe({
#     print("file_path") #<<
#     print(input$de_file) #<<
#   })
# 
#   de_table_in <- reactive({
#     req(input$de_file)
#     rio::import(input$de_file) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
# 
#   output$all_data_UI <- renderUI({
#     req(de_table_in)
#     card(card_header("All genes"), dataTableOutput(outputId = "all_data"))
#   })
# 
#   output$all_data = renderDataTable(datatable(de_table_in()))
# }
# 
# shinyApp(ui = ui, server = server)
# 


## ----echo=T, eval=F-----------------------------------------------------------
# # UI object not shown
# server <- function(input, output){
# 
#   de_table_in <- reactive({
#     req(input$de_file)
#     print("file_path") #<<
#     print(input$de_file) #<<
#     rio::import(input$de_file) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
# 
#   output$all_data_UI <- renderUI({
#     req(de_table_in)
#     card(card_header("All genes"), dataTableOutput(outputId = "all_data"))
#   })
# 
#   output$all_data = renderDataTable(datatable(de_table_in()))
# }
# 
# shinyApp(ui = ui, server = server)
# 


## ----echo=T, eval=F-----------------------------------------------------------
# # UI object not shown
# server <- function(input, output){
# 
#   de_table_in <- reactive({
#     req(input$de_file)
#     browser() #<<
#     rio::import(input$de_file) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
# 
#   output$all_data_UI <- renderUI({
#     req(de_table_in)
#     card(card_header("All genes"), dataTableOutput(outputId = "all_data"))
#   })
# 
#   output$all_data = renderDataTable(datatable(de_table_in()))
# }
# 
# shinyApp(ui = ui, server = server)
# 


## ----echo=T, eval=F-----------------------------------------------------------
# # UI object not shown
# server <- function(input, output){
# 
#   de_table_in <- reactive({
#     req(input$de_file)
#     rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))})
# 
#   output$all_data_UI <- renderUI({
#     req(de_table_in)
#     browser() #<<
#     card(card_header("All genes"), dataTableOutput(outputId = "all_data"))
#   })
# 
#   output$all_data = renderDataTable(datatable(de_table_in()))
# }
# 
# shinyApp(ui = ui, server = server)
# 

