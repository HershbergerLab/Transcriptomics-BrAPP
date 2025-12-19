mod_data_input_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Data Input"),
    actionButton(ns("load_example"), "Load Example Data"),
    fileInput(ns("expr_file"), "Upload Expression Matrix (CSV)", accept = ".csv"),
    fileInput(ns("meta_file"), "Upload Gene Metadata (CSV)", accept = ".csv"),
    fileInput(ns("pheno_file"), "Upload phenotype file (optional)", accept = ".csv")
  )
}

mod_data_input_server <- function(id, example_expression, example_metadata) {
  moduleServer(id, function(input, output, session) {
    
    expr_raw  <- reactiveVal(NULL)
    gene_meta <- reactiveVal(NULL)
    
    observeEvent(input$load_example, {
      expr_raw(example_expression())
      gene_meta(example_metadata())
    })
    
    observeEvent(input$expr_file, {
      expr_raw(read.csv(input$expr_file$datapath, check.names = FALSE))
    })
    
    observeEvent(input$meta_file, {
      gene_meta(read.csv(input$meta_file$datapath, check.names = FALSE))
    })
    
    phenotypes <- reactive({
      req(input$pheno_file)
      
      ext <- tools::file_ext(input$pheno_file$name)
      
      df <- switch(
        ext,
        csv = read.csv(input$pheno_file$datapath, stringsAsFactors = FALSE),
        valaidate("Unsupported file type")
      )
      
      validate(
        need("sample_name" %in% colnames(df),
             "Phenotype file must contain a 'sample_name' column")
      )
      
      df
    })
    
    list(
      expr_raw  = expr_raw,
      gene_meta = gene_meta,
      phenotypes = phenotypes
    )
  })
}
