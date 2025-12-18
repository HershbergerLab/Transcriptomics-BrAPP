mod_pca_server <- function(id, expr_filt) {
  moduleServer(id, function(input, output, session) {
    
    pca_res <- reactive({
      req(expr_filt())
      
      gene_cols <- setdiff(
        colnames(expr_filt()),
        c("sample_name", "device_id", "comments")
      )
      
      mat <- expr_filt()[, gene_cols, drop = FALSE]
      
      vars <- apply(mat, 2, var, na.rm = TRUE)
      print(colnames(expr_filt()))
      print(gene_cols)
      mat <- mat[, vars > 0, drop = FALSE]
      
      validate(
        need(ncol(mat) >= 2, "Not enough variable genes for PCA")
      )
      
      prcomp(mat, center = TRUE, scale. = TRUE)
    })
    
    scores <- reactive({
      req(pca_res())
      as.data.frame(pca_res()$x)
    })
    
    loadings <- reactive({
      req(pca_res())
      as.data.frame(pca_res()$rotation)
    })
    
    variance <- reactive({
      req(pca_res())
      v <- pca_res()$sdev^2
      v / sum(v)
    })
    
    return(list(
      pca_res  = pca_res,
      scores   = scores,
      loadings = loadings,
      variance = variance
    ))
  })
}