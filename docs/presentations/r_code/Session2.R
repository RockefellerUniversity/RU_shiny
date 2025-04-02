params <-
list(isSlides = "no")

## ----setup, include=FALSE-----------------------------------------------------
suppressPackageStartupMessages(require(knitr))
knitr::opts_chunk$set(echo = TRUE, tidy = T)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides != "yes"){
  cat("# Introduction to Shiny (part 2)

---
"    
  )
  
}



## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Session 2 - Input types and reactivity

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Session 2 - Input types and reactivity

---
"    
  )
  
}



## ----text_boxes, echo=T, eval=F-----------------------------------------------
# ui_textInputs = page_fluid(
#   textInput(inputId = "text_box", label = "Experiment name:"),
#   textAreaInput(inputId = "big_text_box", "Describe your experiment:", rows = 3)
# )
# 


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_textInputs, server = function(input, output){})


## ----echo=F, eval=T, message = F----------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(rio)


## ----dropdowns, echo=T, eval=T------------------------------------------------
ui_selInputs <- page_fluid(
  selectInput("dropdown", "Select a gene:", 
              choices = c("TP53", "PTEN", "HRAS", "PI3K")),
  
  selectInput("dropdown", "Select a gene from this really wide box!", 
              choices = c("TP53", "PTEN", "HRAS", "PI3K"),
              width = "100%"),
  
  selectInput("dropdown2", "Select more than one gene if you want:", 
              choices = c("TP53", "PTEN", "HRAS", "PI3K"), 
              selected = c("PTEN", "HRAS"), multiple = T),
)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_selInputs, server = function(input, output){})


## ----pickInputs, echo=T, eval=T-----------------------------------------------
ui_pickInputs <- page_fluid(

  "If you want the user to only select one option from a list, radioButtons work well",
  radioButtons("radio", "Select only one gene from the radio selections:", 
              choices = c("TP53", "PTEN", "HRAS", "PI3K"), 
              selected = "HRAS"),
  
  "To allow the user to select multiple options, use checkboxGroupInput",
  checkboxGroupInput("checkbox_group", "Check one or more boxes next to a gene:", 
                     choices = c("TP53", "PTEN", "HRAS", "PI3K")),

  "OR if you only want a binary yes/no, you can use checkboxInput",
  checkboxInput("checkbox", "Do you agree to the terms and conditions?"),
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_pickInputs, server = function(input, output){})


## ----numInputs, echo=T, eval=T------------------------------------------------
ui_numInputs <- page_fluid(
  
  numericInput("numeric", "Number of samples", value = 1, min = 0, max = 100),
  
  sliderInput("num_slider", "Number of samples", value = 10, min = 0, max = 25),
  
  sliderInput("num_slider", "Range of sample numbers", value = c(10,20), min = 0, max = 25)
)




## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_numInputs, server = function(input, output){})


## ----dateInputs, echo=T, eval=T-----------------------------------------------
ui_dateInputs <- page_fluid(
  dateInput("date", "Choose a date:"),
  
  dateRangeInput("date_range", "Choose a range of dates:")
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_dateInputs, server = function(input, output){})


## ----echo=T, eval=T-----------------------------------------------------------
ui_button <- page_fluid(
  actionButton("button", "Click me!"),
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_button, server = function(input, output){})


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Reactivity

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Reactivity

---
"    
  )
  
}



## ----echo=T, eval=T-----------------------------------------------------------
ui_gene <- page_fluid(
  radioButtons("gene", "Select only one gene from the radio selections:", 
              choices = c("TP53", "PTEN", "HRAS", "PI3K"), 
              selected = "HRAS"),
  
  textOutput("gene_text")
)

server_gene = function(input, output){
   output$gene_text <- renderText({
     paste0("We will study ", input$gene)
   })
 }



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# shinyApp(ui = ui_gene, server = server_gene)


## ----echo=T, eval=F-----------------------------------------------------------
# 
# server_geneBad = function(input, output){
#     print(paste0("We will study ", input$gene))
#  }
# 


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_gene, server = server_geneBad)


## ----echo=T, eval=T-----------------------------------------------------------
ui_gene2 <- page_fluid(
  radioButtons("gene", "Select only one gene from the radio selections:", 
              choices = c("TP53", "PTEN", "HRAS", "PI3K"), 
              selected = "HRAS"),
  
  sliderInput("conditions", "Number of samples", value = 10, min = 0, max = 25),
  
  numericInput("replicates", "Number of replicates", value = 1, min = 0, max = 100),
  
  textOutput("study_summary")
)

server_gene2 = function(input, output){
   output$study_summary <- renderText({
     paste0("We will study ", input$gene, " and use ", input$conditions, " samples, with ", input$replicates, " replicates of each. This will give ", input$conditions*input$replicates, " total samples.")
   })
 }



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_gene2, server = server_gene2)


## ----echo=T, eval=T-----------------------------------------------------------
server_geneGood = function(input, output){
  
  total_samples <- reactive({
    input$conditions*input$replicates
  })
  
   output$study_summary <- renderText({
     paste0("We will study ", input$gene, " and use ", input$conditions, " samples, with ", input$replicates, " replicates of each. This will give ", total_samples(), " total samples.")
   })
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_gene2, server = server_geneGood)


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



## ----echo=F, eval=T, out.width="75%"------------------------------------------
# read in table
de_table <- read.csv("data/shP53_vs_control_DEG.csv")
de_table$negLog10_pval <- -log10(de_table$pvalue)

ui_custom <- page_navbar(title = "RNAseq tools", theme = custom_theme, 
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar( "This is a sidebar",width = 300,),
      
      layout_columns(
        card(card_header("Table of DE results"), dataTableOutput(outputId = "de_data")),
        card(card_header("MA plot"),plotOutput("ma_plot")),
        card(card_header("Volcano plot"),plotOutput("volcano_plot")),
        col_widths = c(12,6,6), row_heights = c("750px", "500px")
      )
    )
  ),
  nav_panel(title = "Next steps","The next step in our analysis will be..."),
  nav_spacer(),
  nav_menu(title = "Links",align = "right",
           nav_item(tags$a(shiny::icon("chart-simple"), "RU BRC - Learn more!", href = "https://rockefelleruniversity.github.io/",target = "_blank"))
  )
)

server_data = function(input, output) {
  output$app_info = renderText("This is an app showing differential gene expression data")
  
  output$de_data = renderDataTable({
    datatable(de_table,filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  output$ma_plot = renderPlot({ 
    ggplot(de_table, aes(x = baseMean, y = log2FoldChange)) +  geom_point() + scale_x_log10() + xlab("baseMean (log scale)") + theme_bw()
  }) 
  
  output$volcano_plot = renderPlot({ 
    ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval)) + geom_point() + theme_bw() 
  }) 
} 



## ----echo=T, eval=T-----------------------------------------------------------

ui_filter <- page_fluid(
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), #<<
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), #<<
  
  dataTableOutput(outputId = "de_data")
)

server_filter = function(input, output){ 
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter))

  output$de_data = renderDataTable(datatable(filtered_de()))
}



## ----echo=T, eval=T-----------------------------------------------------------

ui_filter <- page_fluid(
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  
  dataTableOutput(outputId = "de_data")
)

server_filter = function(input, output){ 
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)) #<< 

  output$de_data = renderDataTable(datatable(filtered_de())) #<< 
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_filter, server = server_filter)


## ----echo=T, eval=T-----------------------------------------------------------

ui_filter_value <- page_fluid(
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  
  layout_columns(
      value_box(title = "Number of genes that go up:", value = textOutput("num_up"), showcase = icon("arrow-up"), theme = value_box_theme(bg = "#22b430")), #<<
      value_box(title = "Number of genes that go down:", value = textOutput("num_down"), showcase = icon("arrow-down"), theme = value_box_theme(bg ="#c34020" )), #<<
      col_widths = c(2,2)))

server_filter_value = function(input, output){ 
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter))
  
  num_up_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange > 0 & padj < 0.05) %>% nrow) 
  num_down_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange < 0 & padj < 0.05) %>% nrow) 
  output$num_up <- renderText(num_up_genes())
  output$num_down <- renderText(num_down_genes()) 
}



## ----echo=T, eval=T-----------------------------------------------------------

ui_filter_value <- page_fluid(
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  
  layout_columns(
      value_box(title = "Number of genes that go up:", value = textOutput("num_up"), showcase = icon("arrow-up"), theme = value_box_theme(bg = "#22b430")), 
      value_box(title = "Number of genes that go down:", value = textOutput("num_down"), showcase = icon("arrow-down"), theme = value_box_theme(bg ="#c34020" )), 
      col_widths = c(2,2)))

server_filter_value = function(input, output){ 
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter))
  
  num_up_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange > 0 & padj < 0.05) %>% nrow) #<<
  num_down_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange < 0 & padj < 0.05) %>% nrow) #<<
  output$num_up <- renderText(num_up_genes()) #<<
  output$num_down <- renderText(num_down_genes()) #<<
}


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_filter_value, server = server_filter_value)


## ----echo=T, eval=T-----------------------------------------------------------

ui_filterButton <- page_fluid(theme = custom_theme,
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  actionButton("de_filter", "Apply filter"), #<<
  card(card_header("Filtered data"), dataTableOutput(outputId = "de_data"), min_height = "750px")
)

server_filterButton = function(input, output){ 
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter))  %>%
    bindEvent(input$de_filter) #<<

  output$de_data = renderDataTable(datatable(filtered_de())) 
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_filterButton, server = server_filterButton)


## ----echo=T, eval=T-----------------------------------------------------------

ui_filterButton <- page_fluid(theme = custom_theme,
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  actionButton("de_filter", "Apply filter"), 
  card(card_header("Filtered data"), dataTableOutput(outputId = "de_data"), min_height = "750px")
)

server_filterButton2 = function(input, output){ 
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter))  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE) #<<

  output$de_data = renderDataTable(datatable(filtered_de())) 
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_filterButton, server = server_filterButton2)


## ----echo=T, eval=T-----------------------------------------------------------

ui_tab <- page_fluid(theme = custom_theme,
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  actionButton("de_filter", "Apply filter"), 
  navset_card_tab(title = "DE result tables", height = "750px", #<<
                  nav_panel(card_header("DEGs"), dataTableOutput(outputId = "de_data")), #<<
                  nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data"))), #<<
)

server_tab = function(input, output){ 
  output$all_data = renderDataTable(datatable(de_table)) #<<
  
  filtered_de <- reactive(de_table %>% dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter))  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE)

  output$de_data = renderDataTable(datatable(filtered_de())) 
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_tab, server = server_tab)


## ----echo=T, eval=T-----------------------------------------------------------

ui_colorDE <- page_fluid(theme = custom_theme,
  numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), 
  numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 0, min = 0, step = 0.1), 
  actionButton("de_filter", "Apply filter"), 
  card(card_header("MA plot"), plotOutput("ma_plot")) #<<
)



## ----echo=T, eval=T-----------------------------------------------------------

server_colorDE = function(input, output){ 
  ma_plot_reac <- reactive({ #<<
    de_table %>% #<<
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>% #<< 
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig)) + geom_point() + scale_x_log10() + #<<
        scale_color_manual(name = "DE status", values = c("red", "grey")) + #<<
        xlab("baseMean (log scale)") + theme_bw() #<<
  })  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE) 
  
    output$ma_plot = renderPlot(ma_plot_reac())
}



## ----echo=T, eval=T-----------------------------------------------------------

server_colorDE = function(input, output){ 
  ma_plot_reac <- reactive({ 
    de_table %>% 
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>% 
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig)) + geom_point() + scale_x_log10() +
        scale_color_manual(name = "DE status", values = c("red", "grey")) + #
        xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE) #<<
  
    output$ma_plot = renderPlot(ma_plot_reac()) 
}



## ----echo=T, eval=T-----------------------------------------------------------

server_colorDE = function(input, output){ 
  ma_plot_reac <- reactive({ #<<
    de_table %>% 
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>% 
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig)) + geom_point() + scale_x_log10() +
        scale_color_manual(name = "DE status", values = c("red", "grey")) + 
        xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE) 
  
    output$ma_plot = renderPlot(ma_plot_reac()) #<<
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_colorDE, server = server_colorDE)


## -----------------------------------------------------------------------------
ui_filterMain <- page_navbar(
  title = "RNAseq tools",
  theme = custom_theme,
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005), # >>>>>>>>>>>>>>>
  
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1), # >>>>>>>>>>>>>>>
        actionButton("de_filter", "Apply filter") # >>>>>>>>>>>>>>>
      ),
      
      layout_columns(
        navset_card_tab( # >>>>>>>>>>>>>>>
          title = "DE result tables",
          nav_panel(card_header("DEGs"), dataTableOutput(outputId = "de_data")), # >>>>>>>>>>>>>>>
          nav_panel(card_header("All genes"), dataTableOutput(outputId = "all_data")) # >>>>>>>>>>>>>>>
        ),
        card(card_header("MA plot"),plotOutput("ma_plot")),
        card(card_header("Volcano plot"),plotOutput("volcano_plot")),
        col_widths = c(12,6,6), row_heights = c("750px", "500px")
      )
    )
  ),
  nav_panel(
    title = "Next steps",
    "The next step in our analysis will be..."
  ),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(
      tags$a(
        shiny::icon("chart-simple"), "RU BRC - Learn more!",
        href = "https://rockefelleruniversity.github.io/",
        target = "_blank"
      )
    )
  )
)


## ----echo=T, eval=T, out.width="75%"------------------------------------------


server_filterMain = function(input, output) {
  output$all_data = renderDataTable({ # >>>>>>>>>>>>>>>
    datatable(de_table,
              selection = "none",
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  filtered_de <- reactive({
    de_table %>%
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter) # >>>>>>>>>>>>>>>
  }) %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE) # >>>>>>>>>>>>>>>

  output$de_data = renderDataTable({
    datatable(filtered_de(), # >>>>>>>>>>>>>>>
              selection = "none",
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  
  ma_plot_reac <- reactive({
    de_table %>%
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%  # >>>>>>>>>>>>>>>
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig)) + geom_point() + scale_x_log10() +
      scale_color_manual(name = "DE status", values = c("red", "grey")) +
      xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE) # >>>>>>>>>>>>>>>
  
    output$ma_plot = renderPlot({
      ma_plot_reac() 
    }) 
  
    volcano_plot_reac <- reactive({
      de_table %>%
        dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>% # >>>>>>>>>>>>>>>
        ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig)) + geom_point() +
        scale_color_manual(name = "DE status", values = c("red", "grey")) + theme_bw()
    }) %>%
      bindEvent(input$de_filter, ignoreNULL = FALSE) # >>>>>>>>>>>>>>>
    
    
  output$volcano_plot = renderPlot({
    volcano_plot_reac() # >>>>>>>>>>>>>>>
  }) 
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_filterMain, server = server_filterMain)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Advanced interactivity with tables and plots

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Advanced interactivity with tables and plots

---
"    
  )
  
}



## ----echo=T, eval=F-----------------------------------------------------------
# ui_rowSelect <- page_fluid(
#   dataTableOutput(outputId = "all_data"),
# 
#   textOutput("selected_row_info")
# )
# 
# server_rowSelect <- function(input, output){
#   output$all_data = renderDataTable({
#     datatable(de_table,
#               selection = "single", #<<
#               filter = 'top')
#   })
# 
#   selected_row <- reactive({
#     row_index <- input$all_data_rows_selected #<<
#     de_table[row_index, ]
#   })
# 
#   output$selected_row_info <- renderText({
#     print(paste0("The selected gene is ", selected_row()$Symbol, " and the index of the selected row is ", input$all_data_rows_selected))
#   })
# }
# 


## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui_rowSelect, server_rowSelect)


## ----echo=T, eval=T-----------------------------------------------------------
ui_pointClick <- page_fluid(
  plotOutput("volcano_plot", click = "volcano_click"), #<<
  
  tableOutput("selected_point_table"),
)

server_pointClick <- function(input, output){
  volcano_plot_reac <- reactive({
        ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval)) +
          geom_point() +
          theme_bw() 
    })
    
  output$volcano_plot = renderPlot(volcano_plot_reac()) 
  
  output$selected_point_table <- renderTable({
    nearPoints(de_table, input$volcano_click, threshold = 20, maxpoints = 1) #<<
  })
}



## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui_pointClick, server_pointClick)


## ----echo=T, eval=T-----------------------------------------------------------
ui_pointBrush <- page_fluid(
  plotOutput("volcano_plot", brush = "volcano_brush"), #<<
  
  tableOutput("selected_brush_table")
)

server_pointBrush <- function(input, output){
  
  volcano_plot_reac <- reactive({
        ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval)) +
          geom_point() +
          theme_bw() 
    })
    
  output$volcano_plot = renderPlot(volcano_plot_reac()) 
  
  output$selected_brush_table <- renderTable({
    brushedPoints(de_table, input$volcano_brush) #<<
  })
}



## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui_pointBrush, server_pointBrush)


## ----echo=T, eval=T, message = F----------------------------------------------
library(plotly)
ui_plotly <- page_fluid(
  plotlyOutput("volcano_plotly"), #<<
)

server_plotly <- function(input, output){
  volcano_plot_reac <- reactive({
        ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval, text = Symbol)) +
          geom_point() +
          theme_bw() 
    })
    
  output$volcano_plotly = renderPlotly(ggplotly(volcano_plot_reac())) #<< 

}



## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui_plotly, server_plotly)


## ----echo=T, eval=T, message = F----------------------------------------------
library(plotly)
ui_plotly <- page_fluid(
  plotlyOutput("volcano_plotly"),
  
  tableOutput("plotly_click_row")
)

server_plotly <- function(input, output){
  volcano_plot_reac <- reactive({
    ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval, text = Symbol)) + geom_point() + theme_bw() 
  })
  
  output$volcano_plotly = renderPlotly(ggplotly(volcano_plot_reac(), source = "volcano_plot"))  #<<
  
  clicked_row <- reactive({
    event <- event_data(event = "plotly_click", source = "volcano_plot") #<<
    if(!is.null(event) > 0){
      de_table %>% filter(log2FoldChange == event$x & negLog10_pval == event$y)
    }
  })
  
  output$plotly_click_row <- renderTable({
    clicked_row()
  })
}



## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui_plotly, server_plotly)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Downloading and uploading files

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Downloading and uploading files

---
"    
  )
  
}



## ----echo=T, eval=T, message=F------------------------------------------------
library(plotly)
ui_download <- page_fluid(
  plotlyOutput("volcano_plotly"),
  
  downloadButton("download_volcano_plot", "Download volcano plot", style = "width:40%;") #<<
)



## ----echo=T, eval=T, message=F------------------------------------------------
library(plotly)
ui_download <- page_fluid(
  plotlyOutput("volcano_plotly"),
  
  downloadButton("download_volcano_plot", "Download volcano plot", style = "width:40%;") #<<
)



## ----echo=T, eval=T, message=F------------------------------------------------
server_download <- function(input, output){
  volcano_plot_reac <- reactive(ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval, text = Symbol)) + geom_point() + theme_bw())
  
  output$volcano_plotly = renderPlotly(ggplotly(volcano_plot_reac(), source = "volcano_plot")) 
  
  output$download_volcano_plot <- downloadHandler( #<<
    filename = function() { #<<
      "volcanoplot.pdf" #<<
    }, content = function(file) { #<<
      ggsave(filename = file, plot = volcano_plot_reac()) #<<
    } #<<
  ) #<<
}


## ----echo=T, eval=F-----------------------------------------------------------
# shinyApp(ui_download, server_download)


## ----echo=T, eval=T-----------------------------------------------------------
ui_newPlots <- page_navbar(
  title = "RNAseq tools",
  theme = custom_theme,
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
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
        # >>>>>>>>>>>>>>>>>>>>>>>>
        # change to plotly and add download buttons to each card
        card(card_header("MA plot"),
             plotlyOutput("ma_plot"), 
             downloadButton("download_ma_plot", "Download MA plot", style = "width:40%;")), 
        card(card_header("Volcano plot"),
             plotlyOutput("volcano_plot"),
             downloadButton("download_volcano_plot", "Download volcano plot", style = "width:40%;")), 
        # >>>>>>>>>>>>>>>>>>>>>>>>
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

server_newPlots = function(input, output) {
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
  # >>>>>>>>>>>>>>>>>>>>>>>>
  output$all_data = renderDataTable({
    datatable(de_table,
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  filtered_de <- reactive({
    de_table %>%
      dplyr::filter(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter)
  }) %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE)

  output$de_data = renderDataTable({
    datatable(filtered_de(),
              selection = "single",
              filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
ma_plot_reac <- reactive({
    de_table %>%
      dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>%  
      ggplot(aes(x = baseMean, y = log2FoldChange, color = sig, text = Symbol)) + geom_point() + scale_x_log10() +
      scale_color_manual(name = "DE status", values = c("red", "grey")) +
      xlab("baseMean (log scale)") + theme_bw() 
  })  %>%
    bindEvent(input$de_filter, ignoreNULL = FALSE)
  
    # >>>>>>>>>>>>>>>>>>>>>>>>
    # use 'renderPlotly' and wrap plot in 'ggplotly'
    output$ma_plot = renderPlotly({
      ggplotly(ma_plot_reac())
    }) 
    # >>>>>>>>>>>>>>>>>>>>>>>>
    
    volcano_plot_reac <- reactive({
      de_table %>%
        dplyr::mutate(sig = ifelse(padj < input$padj_filter & abs(log2FoldChange) > input$lfc_filter, "DE", "Not_DE")) %>% 
        ggplot(aes(x = log2FoldChange, y = negLog10_pval, color = sig, text = Symbol)) + geom_point() +
        scale_color_manual(name = "DE status", values = c("red", "grey")) + theme_bw()
    })
    
  # >>>>>>>>>>>>>>>>>>>>>>>>
  output$volcano_plot = renderPlotly({
    ggplotly(volcano_plot_reac())
  }) 
  # >>>>>>>>>>>>>>>>>>>>>>>>
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui_newPlots, server = server_newPlots)

