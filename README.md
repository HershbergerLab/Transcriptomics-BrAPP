# Transcriptomics-BrAPP

A Shiny application for transcriptomics analysis, including:

- Data upload and filtering  
- Principal Component Analysis (PCA)  
- PCA visualization with selectable PCs  
- Scree plots  
- PC scores and loadings tables  
- Optional phenotype-based coloring and shaping  

---

## Requirements

### R
- **R ≥ 4.2** recommended

Check your R version:
```r

R.version.string
```
Install necessary packages
```r
install.packages(c(
  "shiny",
  "dplyr",
  "ggplot2",
  "DT",
  "tibble",
  "rlang"
))
```
## How to run the app locally

### Download or clone the repository

If using Git:

```bash
git clone https://github.com/yourusername/Transcriptomics-BrAPP.git
```
In R or R studio
```r
setwd("path/to/Transcriptomics-BrAPP")
```
Run the app
```r
shiny::runApp()
```
---

### TODO
- Port to Python web app
- Implement other transcriptomics analyses
