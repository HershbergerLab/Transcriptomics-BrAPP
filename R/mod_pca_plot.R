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

mod_pca_plot_server <- function(id, expr_filt, pca, variance, phenotypes = NULL) {
  moduleServer(id, function(input, output, session) {
    
    plot_df <- reactive({
      req(expr_filt(), pca())
      
      df <- bind_cols(
        expr_filt(),
        as.data.frame(pca()$x)
      )
      
      # Optional phenotype join
      if (!is.null(phenotypes) && !is.null(phenotypes())) {
        df <- dplyr::left_join(
          df,
          phenotypes(),
          by = "sample_name"
        )
      }
      
      df
    })
    
    observe({
      pcs <- paste0("PC", seq_along(variance()))
      updateSelectInput(session, "pc_x", choices = pcs, selected = pcs[1])
      updateSelectInput(session, "pc_y", choices = pcs, selected = pcs[2])
    })
    
    observe({
      df <- plot_df()
      
      # Default: no phenotype columns
      pheno_cols <- character(0)
      
      if (!is.null(phenotypes) &&
          !is.null(phenotypes()) &&
          ncol(phenotypes()) > 1) {
        
        pheno_cols <- setdiff(
          colnames(phenotypes()),
          "sample_name"
        )
      }
      
      # Update color dropdown
      updateSelectInput(
        session,
        "color_by",
        choices = c("None", pheno_cols),
        selected = "None"
      )
      
      # If no phenotypes, shape should only be None
      if (length(pheno_cols) == 0) {
        updateSelectInput(
          session,
          "shape_by",
          choices = "None",
          selected = "None"
        )
        return()
      }
      
      # Only allow categorical columns for shape
      categorical_cols <- pheno_cols[
        vapply(
          df[, pheno_cols, drop = FALSE],
          function(x) is.factor(x) || is.character(x),
          logical(1)
        )
      ]
      
      updateSelectInput(
        session,
        "shape_by",
        choices = c("None", categorical_cols),
        selected = "None"
      )
    })
    
    
    output$pca_plot <- renderPlot({
      req(plot_df(), variance(), input$pc_x, input$pc_y)
      
      df <- plot_df()
      
      pc_x_num <- as.integer(sub("PC", "", input$pc_x))
      pc_y_num <- as.integer(sub("PC", "", input$pc_y))
      
      var_x <- round(variance()[pc_x_num] * 100, 1)
      var_y <- round(variance()[pc_y_num] * 100, 1)
      
      aes_args <- list(
        x = rlang::sym(input$pc_x),
        y = rlang::sym(input$pc_y)
      )
      
      if (input$color_by != "None" && input$color_by %in% colnames(df)) {
        aes_args$color <- rlang::sym(input$color_by)
      }
      
      if (input$shape_by != "None" &&
          input$shape_by %in% colnames(df)) {
        aes_args$shape <- rlang::sym(input$shape_by)
      }
      
      g <- ggplot(df, do.call(aes, aes_args)) +
        geom_point(size = input$pt_size, alpha = 0.8) +
        theme_minimal() +
        labs(
          x = paste0(input$pc_x, " (", var_x, "% variance)"),
          y = paste0(input$pc_y, " (", var_y, "% variance)")
        )
      
      if (input$labels) {
        g <- g + geom_text(
          aes(label = sample_name),
          vjust = -1,
          size = 3
        )
      }
      
      g
    })
    
    output$scree_plot <- renderPlot({
      tibble(
        PC = seq_along(variance()),
        Var = variance()
      ) |>
        ggplot(aes(x = PC, y = Var)) +
        geom_col() +
        geom_line() +
        scale_x_continuous(
          breaks = seq_along(variance())
        ) +
        labs(
          x = "Principal Component",
          y = "Proportion of Variance Explained"
        ) +
        theme_minimal()
    })
  })
}
