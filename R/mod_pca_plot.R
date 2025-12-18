mod_pca_plot_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("pc_x"), "PC X", NULL),
    selectInput(ns("pc_y"), "PC Y", NULL),
    selectInput(ns("color_by"), "Color by", NULL),
    selectInput(ns("shape_by"), "Shape by", NULL),
    sliderInput(ns("pt_size"), "Point size", 1, 6, 3),
    checkboxInput(ns("labels"), "Show labels", FALSE),
    plotOutput(ns("pca_plot")),
  )
}

mod_scree_plot_ui <- function(id) {
  ns <- NS(id)
  plotOutput(ns("scree_plot"))
}

mod_pca_plot_server <- function(id, expr_filt, pca, variance) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      pcs <- paste0("PC", seq_along(variance()))
      updateSelectInput(session, "pc_x", choices = pcs, selected = pcs[1])
      updateSelectInput(session, "pc_y", choices = pcs, selected = pcs[2])
    })
    
    observe({
      req(expr_filt())
      df <- expr_filt()
      pheno <- setdiff(
        colnames(expr_filt()),
        grep("^Zeam", colnames(expr_filt()), value = TRUE)
      )
      
      updateSelectInput(session, "color_by", choices = c("None", pheno))
      updateSelectInput(session, "shape_by", choices = c("None", pheno))
    })
    
    output$pca_plot <- renderPlot({
      df <- bind_cols(expr_filt(), as.data.frame(pca()$x))
      
      pc_x_num <- as.integer(sub("PC", "", input$pc_x))
      pc_y_num <- as.integer(sub("PC", "", input$pc_y))
      
      var_pc1 <- round(variance()[pc_x_num] * 100, 1)
      var_pc2 <- round(variance()[pc_y_num] * 100, 1)
      
      g <- ggplot(df, aes_string(input$pc_x, input$pc_y)) +
        geom_point(size = input$pt_size) +
        theme_minimal() +
        labs(
          x = paste0(input$pc_x, "(", var_pc1, "% variance)"),
          y = paste0(input$pc_y, "(", var_pc2, "% variance)"),
        )
      
      if (input$color_by != "None") {
        g <- g + aes_string(colour = input$color_by)
      }
      if (input$shape_by != "None") {
        g <- g + aes_string(shape = input$shape_by)
      }
      if (input$labels) {
        g <- g + geom_text(aes(label = sample_name), vjust = -1)
      }
      g
    })
    
    output$scree_plot <- renderPlot({
      tibble(
        PC = paste0("PC", seq_along(variance())),
        Var = variance()
      ) |>
        ggplot(aes(PC, Var)) +
        geom_col() +
        geom_line(aes(group = 1)) +
        theme_minimal()
    })
  })
}
