mod_filter_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Filtering"),
    textAreaInput(ns("keep_genes"), "Keep Gene IDs (one per line)"),
    textAreaInput(ns("drop_genes"), "Drop Gene IDs (one per line)"),
    numericInput(ns("min_expr"), "Min mean expression", 0),
    numericInput(ns("min_var"), "Min variance", 0),
    actionButton(ns("apply_filter"), "Apply Filters")
  )
}

mod_filter_server <- function(id, expr_raw) {
  moduleServer(id, function(input, output, session) {
    
    expr_filt <- reactiveVal(NULL)
    
    observeEvent(input$apply_filter, {
      req(expr_raw())
      df <- expr_raw()
      
      gene_cols <- setdiff(
        colnames(df),
        c("sample_name", "device_id", "comments")
      )
      
      mat <- df[, gene_cols, drop = FALSE]
      
      keep <- strsplit(input$keep_genes, "\n")[[1]]
      drop <- strsplit(input$drop_genes, "\n")[[1]]
      
      if (any(nzchar(keep))) {
        mat <- mat[, intersect(colnames(mat), keep), drop = FALSE]
      }
      if (any(nzchar(drop))) {
        mat <- mat[, setdiff(colnames(mat), drop), drop = FALSE]
      }
      
      if (input$min_expr > 0) {
        mat <- mat[, colMeans(mat) >= input$min_expr, drop = FALSE]
      }
      if (input$min_var > 0) {
        mat <- mat[, apply(mat, 2, var) >= input$min_var, drop = FALSE]
      }
      
      expr_filt(bind_cols(df[, c("sample_name", "device_id", "comments")], mat))
    })
    
    expr_filt
  })
}
