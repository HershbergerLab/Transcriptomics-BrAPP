source("R/mod_data_input.R")
source("R/mod_filtering.R")
source("R/mod_pca_compute.R")
source("R/mod_pca_plot.R")

library(DT)
library(dplyr)
library(ggplot2)
library(tibble)
library(rlang)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      mod_data_input_ui("data"),
      mod_filter_ui("filter")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("PCA Plot", mod_pca_plot_ui("pca")),
        tabPanel("Scree Plot", mod_scree_plot_ui("pca")),
        tabPanel("PC Scores", DT::DTOutput("pc_table")),
        tabPanel("Loadings", DT::DTOutput("loading_table"))
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- mod_data_input_server(
    "data",
    example_expression,
    example_metadata
  )
  
  expr_filt <- mod_filter_server("filter", data$expr_raw)
  
  pca <- mod_pca_server("pca_compute", expr_filt)
  
  mod_pca_plot_server(
    "pca",
    expr_filt,
    pca$pca_res,
    pca$variance,
    phenotypes = data$phenotypes
  )
  
  # PC SCORES TABLE (samples x PCs)
  output$pc_table <- renderDT({
    req(pca$pca_res(), expr_filt())
    
    scores <- as.data.frame(pca$pca_res()$x)
    
    scores <- scores |>
      mutate(sample = expr_filt()$sample_name) |>
      select(sample, everything())
    
    datatable(
      scores,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # LOADINGS TABLE (genes x PCs)
  output$loading_table <- renderDT({
    req(pca$pca_res())
    
    loadings <- as.data.frame(pca$pca_res()$rotation)
    loadings$gene <- rownames(loadings)
    
    
    loadings <- loadings[, c("gene", colnames(loadings))]
    
    datatable(
      loadings,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

shinyApp(ui, server)
