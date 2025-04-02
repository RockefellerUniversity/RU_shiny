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

ui_upload <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), #<<
  
  dataTableOutput(outputId = "all_data"),
)

server_upload <- function(input, output){
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
ui_upload <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  dataTableOutput(outputId = "all_data"),
)

server_upload <- function(input, output){
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
# shinyApp(ui = ui_upload, server = server_upload)


## ----echo=T, eval=T-----------------------------------------------------------

server_uploadReq <- function(input, output){
  
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
# shinyApp(ui = ui_upload, server = server_uploadReq)


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

ui_fileInput <- page_navbar(
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
        
        actionButton("de_filter", "Apply filter")
      ),
      
      layout_columns(
        navset_card_tab(
          title = "DE result tables",
          nav_panel(card_header("DEGs"), dataTableOutput(outputId = "de_data")),
          nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))
        ),
        card(card_header("MA plot"),
             plotOutput("ma_plot"),
             downloadButton("download_ma_plot", "Download MA plot", style = "width:40%;")), 
        card(card_header("Volcano plot"),
             plotOutput("volcano_plot"),
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


## ----echo=T, eval=F-----------------------------------------------------------
# # part of server function, not run in isolation...
# filtered_de <- reactive({
#   req(input$de_file)
#     de_table_in() %>% #<<
#       dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
#   }) %>%
#     bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) #<<
# 


## ----echo=T, eval=F-----------------------------------------------------------
# # part of server function, not run in isolation...
# ma_plot_reac <- reactive({
#     de_table_in() %>% #
#       dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
#       ggplot(aes(x = baseMean, y = log2FoldChange, color = sig, label = Symbol)) + geom_point() +
#       scale_x_log10() + scale_color_manual(name = "DE status", values = c("red", "grey")) +
#       xlab("baseMean (log scale)") + theme_bw()
#   })  %>%
#     bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE)  #<<
# 


## ----echo=T, eval=T, out.width="75%"------------------------------------------

server_fileInput = function(input, output) {

  # >>>>>>>>>>>>>>>>>>>>>>>>
  de_table_in <- reactive({
    req(input$de_file)
    rio::import(input$de_file$datapath) %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  # >>>>>>>>>>>>>>>>>>>>>>>>
  
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
  
  ma_plot_reac <- reactive({
      de_table_in() %>% # >>>>>>>>>>>>>>>>>>>>>>>>
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig, label = Symbol)) + geom_point() +
      scale_x_log10() + scale_color_manual(name = "DE status", values = c("red", "grey")) +
      xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) # >>>>>>>>>>>>>>>>>>>>>>>>

    output$ma_plot = renderPlot({
      ma_plot_reac()
    }) 
  
    volcano_plot_reac <- reactive({
        de_table_in() %>% # >>>>>>>>>>>>>>>>>>>>>>>>
          dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%
          ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig)) +
          geom_point() +
          scale_color_manual(name = "DE status", values = c("red","grey")) + theme_bw()  
      
    }) %>%
      bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) # >>>>>>>>>>>>>>>>>>>>>>>>
  
    output$volcano_plot = renderPlot({
      volcano_plot_reac()
    }) 
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_fileInput, server = server_fileInput)


## ----echo=T, eval=T-----------------------------------------------------------
ui_renderUI <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
   uiOutput("sidebar_filters_UI"), #<<
)

server_renderUI <- function(input, output){
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

ui_renderUI <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
   uiOutput("sidebar_filters_UI"), 
)

server_renderUI <- function(input, output){
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



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_renderUI, server = server_renderUI)


## ----echo=T, eval=T-----------------------------------------------------------
ui_renderUI_table <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  
  uiOutput("all_data_UI")  #<<
)

server_renderUI_table <- function(input, output){
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
# shinyApp(ui = ui_renderUI_table , server = server_renderUI_table)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_renderUI_table , server = server_renderUI_table)


## ----echo=T, eval=T-----------------------------------------------------------
ui_validate_small <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  uiOutput("all_data_UI")
)

server_validate_small <- function(input, output){
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
# shinyApp(ui = ui_validate_small, server = server_validate_small)


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



## ----echo=T, eval=T-----------------------------------------------------------
ui_renderUIall <- page_navbar(
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

server_renderUIall = function(input, output) {
  
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
  
  output$all_data = renderDataTable({
    datatable(de_table_in(), filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
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
        
        actionButton("de_filter", "Apply filter")
      )
  })
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  filtered_de <- reactive({
    de_table_in() %>% 
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
  }) %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE)

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
          ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig)) +
          geom_point() +
          scale_color_manual(name = "DE status", values = c("red","grey"),) + theme_bw() 
          ggtitle("Volcano plot")
      
    }) %>%
      bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) 
  
    output$volcano_plot = renderPlotly({
      ggplotly(volcano_plot_reac())
    })
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_renderUIall, server = server_renderUIall)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Observers

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Observers

---
"    
  )
  
}



## ----echo=T, eval=T-----------------------------------------------------------
ui_notify <- page_fluid(
  fileInput("de_file", "Upload a DE file", accept = c(".csv", ".tsv", "xlsx", "xls")), 
  uiOutput("all_data_UI"))

server_notify <- function(input, output){
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
# shinyApp(ui = ui_notify, server = server_notify)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

server_notify = function(input, output) {
  
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  observe({
      showNotification("A new table has been loaded into the app!", duration = NULL, type = "message")
  }) %>%
    bindEvent(de_table_in())
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  de_table_in <- reactive({
    req(input$de_file)
    file_in <- rio::import(input$de_file$datapath)
    validate(
      need(expr = all(c("baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj") %in% colnames(file_in)), 
           message = "You must have the following columns: 'baseMean', 'log2FoldChange', 'lfcSE', 'stat', 'pvalue', 'padj'")
    )
    file_in %>% dplyr::mutate(negLog10_pval = -log10(pvalue))
  })
  
  output$all_data = renderDataTable({
    datatable(de_table_in(), filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
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
             downloadButton("download_ma_plot", "Download MA plot", style = "width:40%;")), 
        card(card_header("Volcano plot"),
             plotlyOutput("volcano_plot"),
             downloadButton("download_volcano_plot", "Download volcano plot", style = "width:40%;")),
        col_widths = c(12,6,6), row_heights = c("750px", "500px")
      )
      }
  })
  
  output$sidebar_filters_UI <- renderUI({
    req(de_table_in())
      div(
        "DE filters",
        numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.001),
        
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
        
        actionButton("de_filter", "Apply filter")
      )
  })
  
  filtered_de <- reactive({
    de_table_in() %>% 
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
  }) %>%
    bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE)

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
          ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig)) +geom_point() +
          scale_color_manual(name = "DE status", values = c("red","grey"),) +theme_bw()
    }) %>%
      bindEvent(input$de_filter, de_table_in(), ignoreNULL = FALSE) 
  
    output$volcano_plot = renderPlotly({
      ggplotly(volcano_plot_reac())
    })
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_renderUIall, server = server_notify)


## ----echo=T, eval=T-----------------------------------------------------------
library(rsconnect)

