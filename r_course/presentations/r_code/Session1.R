params <-
list(isSlides = "no")

## ----include=FALSE------------------------------------------------------------
suppressPackageStartupMessages(require(knitr))
knitr::opts_chunk$set(echo = TRUE, tidy = T)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Set Up

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Set Up

---
"    
  )
  
}



## ----setwd_introtoR,eval=F----------------------------------------------------
# setwd("/PathToMyDownload/RU_shiny-master/r_course")
# # e.g. setwd("~/Downloads/RU_shiny-master/r_course")


## ----echo=F, eval=T, message = F----------------------------------------------
library(shiny)
library(bslib)
library(ggplot2)
library(DT)



## ----first_app, echo=T, eval=F------------------------------------------------
# library(shiny)
# library(bslib)
# 
# ui = page_fluid(
#   textOutput(outputId = "app_info")
# )
# 
# server = function(input, output) {
#   output$app_info = renderText("Our first app!")
# }
# 
# shinyApp(ui = ui, server = server)
# 


## ----template, eval=F, echo=T-------------------------------------------------
# library(shiny)
# 
# ui <- page_fluid()
# 
# server <- function(input, output, session) {
# 
# }
# 
# shinyApp(ui, server)


## ----basic, echo=T, eval=T----------------------------------------------------
library(shiny)
library(bslib)

data <- data.frame(col1 = 1:3, 
                   col2 = 4:6)

ui = page_fluid(
  textOutput(outputId = "app_info"),
  
  br(),
  tags$b("Bold text in UI object"),
  
  br(),
  tableOutput(outputId = "table_in")
)

server = function(input, output) {
  output$app_info <- renderText("Rendered in server!")
  
  output$table_in <- renderTable(data)
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
library(shiny) #<<
library(bslib) #<<

data <- data.frame(col1 = 1:3, #<<
                   col2 = 4:6) #<<

ui = page_fluid(
  textOutput(outputId = "app_info"),
  
  br(),
  tags$b("Bold text in UI object"),
  
  br(),
  tableOutput(outputId = "table_in")
)

server = function(input, output) {
  output$app_info <- renderText("Rendered in server!")
  
  output$table_in <- renderTable(data)
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T-----------------------------------------------------------
library(shiny) 
library(bslib) 

data <- data.frame(col1 = 1:3, 
                   col2 = 4:6) 

ui = page_fluid( #<<
  textOutput(outputId = "app_info"), #<<
  #<<
  br(), #<<
  tags$b("Bold text in UI object"), #<<
  #<<
  br(), #<<
  tableOutput(outputId = "table_in") #<<
) #<<

server = function(input, output) {
  output$app_info <- renderText("Rendered in server!")
  
  output$table_in <- renderTable(data)
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

print(as.character(ui))


## ----echo=T, eval=T-----------------------------------------------------------
library(shiny) 
library(bslib) 

data <- data.frame(col1 = 1:3, 
                   col2 = 4:6) 

ui = page_fluid(
  textOutput(outputId = "app_info"), #<<
  
  br(), 
  tags$b("Bold text in UI object"),
  
  br(),
  tableOutput(outputId = "table_in") 
)

server = function(input, output) {
  output$app_info <- renderText("Rendered in server!") #<<
  
  output$table_in <- renderTable(data)
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, message=F, warning=F-------------------------------------
library(shiny) 
library(bslib) 

data <- data.frame(col1 = 1:3, 
                   col2 = 4:6) 

ui = page_fluid(
  textOutput(outputId = "app_info"), 
  
  br(), #<<
  tags$b("Bold text in UI object"), #<<
   #<<
  br(), #<<
  tableOutput(outputId = "table_in") 
)

server = function(input, output) {
  output$app_info <- renderText("Rendered in server!") 
  
  output$table_in <- renderTable(data)
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, message=F, warning=F-------------------------------------
library(shiny) 
library(bslib) 

data <- data.frame(col1 = 1:3, 
                   col2 = 4:6) 

ui = page_fluid(
  textOutput(outputId = "app_info"), 
  
  br(), 
  tags$b("Bold text in UI object"), 
   
  br(), 
  tableOutput(outputId = "table_in") 
)

server = function(input, output) { #<<
  output$app_info <- renderText("Rendered in server!")#<<
  #<<
  output$table_in <- renderTable(data)#<<
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, message=F, warning=F-------------------------------------
library(shiny) 
library(bslib) 

data <- data.frame(col1 = 1:3, 
                   col2 = 4:6) 

ui = page_fluid(
  textOutput(outputId = "app_info"), 
  
  br(), 
  tags$b("Bold text in UI object"), 
   
  br(), 
  tableOutput(outputId = "table_in") 
)

server = function(input, output) { 
  output$app_info <- renderText("Rendered in server!")
  
  output$table_in <- renderTable(data)
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# shinyApp(ui = ui, server = server) #<<


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Build our own RNAseq app

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Build our own RNAseq app

---
"    
  )
  
}



## ----echo=T, eval=T, out.width="75%", message=F, warning=F--------------------
library(shiny) 
library(bslib) 
library(dplyr)

# read in table
de_table <- read.csv("data/shP53_vs_control_DEG.csv")
de_table$negLog10_pval <- -log10(de_table$pvalue)

# view table (would not be part of shiny script)
head(de_table, 3)


## ----echo=T, eval=T, out.width="75%", message=F, warning=F--------------------
library(DT) #<<

ui = page_fluid(
  
  textOutput(outputId = "app_info"),
  DT::dataTableOutput(outputId = "de_data") #<<
)

server = function(input, output) {
  
  output$app_info = renderText("This is an app showing differential gene expression data")
  output$de_data = renderDataTable(datatable(de_table)) #<<
}


## ----addDT, echo=T, eval=F, out.width="75%"-----------------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------


server = function(input, output) {
  output$app_info = renderText("This is an app showing differential gene expression data")
  
  output$de_data = renderDataTable({
    datatable(de_table, #<<
              filter = 'top') %>% #<<
      formatRound(columns = c("baseMean", "log2FoldChange", "lfcSE", "stat"), digits = 3) %>% #<<
      formatSignif(columns = c("pvalue", "padj"), digits = 3) #<<
  })
}



## ----customizeDT, echo=T, eval=F, out.width="75%"-----------------------------
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------


ui = page_fluid(
  textOutput(outputId = "app_info"),
  
  dataTableOutput(outputId = "de_data"),
  
  plotOutput("ma_plot"), #<<
  
  plotOutput("volcano_plot") #<<
)



## ----echo=T, eval=T, out.width="75%"------------------------------------------
library(ggplot2)

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



## ----addPlots, echo=T, eval=F, out.width="75%"--------------------------------
# shinyApp(ui = ui, server = server)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# App layouts

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# App layouts

---
"    
  )
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fillable(
# 
#   card(card_header("Table of DE results"),
#        dataTableOutput(outputId = "de_data")),
#   card(card_header("MA plot"),
#        plotOutput("ma_plot")),
#   card(card_header("Volcano plot"),
#        plotOutput("volcano_plot"))
# )
# 


## ----addCards, echo=T, eval=F, out.width="75%"--------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fillable(
#   accordion(
#     accordion_panel(title = "Table of DE results",
#                     dataTableOutput(outputId = "de_data")),
# 
#     accordion_panel(title = "MA plot",
#                     plotOutput("ma_plot")),
# 
#     accordion_panel(title = "Volcano plot",
#                     plotOutput("volcano_plot"))
#   )
# )
# 


## ----accordion, echo=T, eval=F, out.width="75%"-------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fillable(
#   layout_columns(
#     card(card_header("Table of DE results", dataTableOutput(outputId = "de_data"))),
# 
#     card(card_header("MA plot",plotOutput("ma_plot"))),
# 
#     card(card_header("Volcano plot",plotOutput("volcano_plot"))),
# 
#     col_widths = c(12,6,6)
#   ),
# )
# 


## ----multiple_rows, echo=T, eval=F, out.width="75%"---------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fillable(
#   layout_columns(
#     col_widths = 12,
#     card(card_header("Table of DE results", dataTableOutput(outputId = "de_data")))),
# 
#   layout_columns(
#     col_widths = c(6, 6),
#     card(card_header("This is a tall box")),
# 
#     layout_columns(
#       col_widths = c(12,12),
#       card(card_header("MA plot",plotOutput("ma_plot"))),
# 
#       card(card_header("Volcano plot",plotOutput("volcano_plot")))
#     )
#   ),
# )
# 


## ----nested, echo=T, eval=F, out.width="75%"----------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fillable( #<<
#   layout_columns(
#     card(card_header("Table of DE results", dataTableOutput(outputId = "de_data"))),
# 
#     card(card_header("MA plot",plotOutput("ma_plot"))),
# 
#     card(card_header("Volcano plot",plotOutput("volcano_plot"))),
# 
#     col_widths = c(12,6,6)
#   )
# )
# 


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fixed( #<<
#   layout_columns(
#     card(card_header("Table of DE results", dataTableOutput(outputId = "de_data"))),
# 
#     card(card_header("MA plot",plotOutput("ma_plot"))),
# 
#     card(card_header("Volcano plot",plotOutput("volcano_plot"))),
# 
#     col_widths = c(12,6,6)
#   )
# )
# 


## ----fixed_page, echo=T, eval=F, out.width="75%"------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui_fluid <- page_fluid( #<<
#   layout_columns(
#     card(card_header("Table of DE results", dataTableOutput(outputId = "de_data"))),
# 
#     card(card_header("MA plot",plotOutput("ma_plot"))),
# 
#     card(card_header("Volcano plot",plotOutput("volcano_plot"))),
# 
#     col_widths = c(12,6,6)
#   )
# )
# 


## ----fluid_page, echo=T, eval=F, out.width="75%"------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Value boxes and sidebars

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Value boxes and sidebars

---
"    
  )
  
}



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# 
# ui <- page_fluid(
#     layout_columns(
#       layout_columns(
#         value_box(title = "Number of genes that go up:", value = 2000, "Adjusted p-value < 0.05", showcase = icon("arrow-up"), theme = value_box_theme(bg = "#22b430")), #<<
#       value_box(title = "Number of genes that go down:", value = 1000, "Adjusted p-value < 0.05", showcase = icon("arrow-down"), theme = value_box_theme(bg ="#c34020" )), #<<
#         col_widths = c(12,12)
#       ),
#       card(card_header("Table of DE results", dataTableOutput(outputId = "de_data"))),
#       col_widths = c(3,9))
# )
# 
# 


## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <-page_fluid( 
  layout_columns(
    card(card_header("Table of DE results"),
         layout_sidebar(sidebar = sidebar("Sidebar ONLY for table", width = 400), #<<
                        dataTableOutput(outputId = "de_data"), #<<
                        fillable = FALSE)), #<<
    card(card_header("MA plot",plotOutput("ma_plot"))),
    card(card_header("Volcano plot",plotOutput("volcano_plot"))),
    col_widths = c(12,6,6)
  )
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <- page_sidebar(
  title = "RNAseq tools",
  
  sidebar = sidebar( #<<
    "This is a sidebar for the whole page", #<<
    width = 300, #<<
  ), #<<
  
  layout_columns(
    card(card_header("Table of DE results"), dataTableOutput(outputId = "de_data")),
    card(card_header("MA plot"),plotOutput("ma_plot")),
    card(card_header("Volcano plot"),plotOutput("volcano_plot")),
    col_widths = c(12,6,6), row_heights = c("750px", "500px")
  )
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Mutli-page apps

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Mutli-page apps

---
"    
  )
  
}



## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <- page_navbar( #<<
  title = "RNAseq tools",
  nav_panel(title = "DE Analysis",  #<<
            layout_columns(
              card(card_header("Table of DE results"), dataTableOutput(outputId = "de_data")),
              card(card_header("MA plot"),plotOutput("ma_plot")),
              card(card_header("Volcano plot"),plotOutput("volcano_plot")),
              col_widths = c(12,6,6), row_heights = c("750px", "500px")
            )),
  nav_panel(title = "Next steps",  "The next step in our analysis will be...") #<<
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <- page_navbar(
  title = "RNAseq tools",
  nav_panel(title = "DE Analysis",  
            layout_columns(
              card(card_header("Table of DE results"), dataTableOutput(outputId = "de_data")),
              card(card_header("MA plot"),plotOutput("ma_plot")),
              card(card_header("Volcano plot"),plotOutput("volcano_plot")),
              col_widths = c(12,6,6), row_heights = c("750px", "500px")
            )),
  nav_panel(title = "Next steps",   "The next step in our analysis will be..."),
  nav_spacer(), #<<
  nav_menu( #<<
    title = "Links", align = "right", #<<
    nav_item( #<<
      tags$a(shiny::icon("chart-simple"), "RU BRC - Learn more!",href = "https://rockefelleruniversity.github.io/", target = "_blank")), #<<
    nav_item( #<<
      tags$a(shiny::icon("arrow-right"), "More about posit",href = "https://posit.co/", target = "_blank"))#<<
  ) #<<
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <- page_navbar( 
  title = "RNAseq tools",
  sidebar = sidebar("The same sidebar on every page...", width = 500), #<<
  nav_panel(title = "DE Analysis",  
            layout_columns(
              card(card_header("Table of DE results"), dataTableOutput(outputId = "de_data")),
              card(card_header("MA plot"),plotOutput("ma_plot")),
              card(card_header("Volcano plot"),plotOutput("volcano_plot")),
              col_widths = c(12,6,6), row_heights = c("750px", "500px")
            )),
  nav_panel(title = "Next steps",  "The next step in our analysis will be...") 
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <- page_navbar( 
  title = "RNAseq tools",
  nav_panel(title = "DE Analysis",  
            layout_sidebar(#<<
              sidebar = sidebar("The side bar is only on this page...", width = 500),  #<<
              layout_columns(#<<
                card(card_header("Table of DE results"), dataTableOutput(outputId = "de_data")), #<<
                card(card_header("MA plot"),plotOutput("ma_plot")), #<<
                card(card_header("Volcano plot"),plotOutput("volcano_plot")), #<<
                col_widths = c(12,6,6), row_heights = c("750px", "500px")) #<<
            ),  #<<
  ),
  nav_panel(title = "Next steps",  "The next step in our analysis will be...") 
)



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----results='asis',include=TRUE,echo=FALSE-----------------------------------
if(params$isSlides == "yes"){
  cat("class: inverse, center, middle

# Themes

<html><div style='float:left'></div><hr color='#EB811B' size=1px width=720px></html> 

---
"    
  )
}else{
  cat("# Themes

---
"    
  )
  
}



## -----------------------------------------------------------------------------
bootswatch_themes()


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui <- page_navbar(
  title = "RNAseq tools",
  theme = bs_theme(version = 5, bootswatch = "cerulean"), #<<
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



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

ui_darkly <- page_navbar(
  title = "RNAseq tools",
  theme = bs_theme(version = 5, bootswatch = "darkly"), #<<
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



## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)


## ----echo=T, eval=T, out.width="75%"------------------------------------------

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



## ----echo=T, eval=T, out.width="75%"------------------------------------------

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




## ----echo=T, eval=F, out.width="75%"------------------------------------------
# #same server function as previous
# shinyApp(ui = ui, server = server)

