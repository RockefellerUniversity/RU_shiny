library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)

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
  theme = custom_theme, #<<
  nav_panel(
    title = "DE Analysis",
    layout_sidebar(
      sidebar = sidebar("This is a sidebar", width = 300),
      
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
  nav_menu(title = "Links",
           align = "right",
           nav_item(tags$a(shiny::icon("chart-simple"), "RU BRC - Learn more!", href = "https://rockefelleruniversity.github.io/",target = "_blank"))
  )
)


server = function(input, output) {
  output$app_info = renderText("This is an app showing differential gene expression data")
  
  output$de_data = renderDataTable({
    datatable(de_table,filter = 'top') %>%
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>%
      formatSignif(columns = c("pvalue", "padj"), digits = 3)
  })
  
  output$ma_plot = renderPlot({ #<<
    ggplot(de_table, aes(x = baseMean, y = log2FoldChange)) +  geom_point() + #<<
      scale_x_log10() + xlab("baseMean (log scale)") + theme_bw()#<<
  }) #<<
  
  output$volcano_plot = renderPlot({ #<<
    ggplot(de_table, aes(x = log2FoldChange, y = negLog10_pval)) + geom_point() + theme_bw() #<<
  }) #<<
} 

shinyApp(ui, server)