library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)

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

# read in table
de_table <- read.csv("shP53_vs_control_DEG.csv")
de_table$negLog10_pval <- -log10(de_table$pvalue)

ui <- page_navbar(
  title = "RNAseq tools",
  theme = custom_theme,
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        numericInput("padj_filter", label = "Cutoff for padj:", value = 0.05, min = 0, max = 1, step = 0.005),
        
        numericInput("lfc_filter", label = "Cutoff for log2 FC:", value = 1, min = 0, step = 0.1),
        
        actionButton("de_filter", "Apply filter"),
        # >>>>>>>>>>>>>>>>>>>>>>>>
        # add value boxes (and new line spaces with br()) to the sidebar
        br(),
        br(),
        value_box(title = "Number of genes that go up:", value = textOutput("num_up"), 
                  showcase = icon("arrow-up"), 
                  theme = value_box_theme(bg = "#22b430")), 
        value_box(title = "Number of genes that go down:", value = textOutput("num_down"), 
                  showcase = icon("arrow-down"), 
                  theme = value_box_theme(bg ="#c34020" ))
        # >>>>>>>>>>>>>>>>>>>>>>>>
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

server = function(input, output) {
  
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
  
  # >>>>>>>>>>>>>>>>>>>>>>>>
  # make reactives that return number of genes that go up or down based on user provided inputs
  num_up_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange > 0 & padj < 0.05) %>% nrow) 
  num_down_genes <- reactive(filtered_de() %>% dplyr::filter(log2FoldChange < 0 & padj < 0.05) %>% nrow) 
  # >>>>>>>>>>>>>>>>>>>>>>>>
  # make outputs that display the above reactives in the valueboxes
  output$num_up <- renderText(num_up_genes())
  output$num_down <- renderText(num_down_genes()) 
  
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
  # use 'renderPlotly' and wrap plot in 'ggplotly' to make ma plot interactive
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
  # use 'renderPlotly' and wrap plot in 'ggplotly' to make volcano plot interactive
  output$volcano_plot = renderPlotly({
    ggplotly(volcano_plot_reac())
  }) 
  # >>>>>>>>>>>>>>>>>>>>>>>>
  
  # >>>>>>>>>>>>>>>>>>>>>>>>
  # download hanlders for each download button below each plot
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
  
}


shinyApp(ui, server)