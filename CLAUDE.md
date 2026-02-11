# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a teaching materials repository for the CTLGH-ILRI Genomics Training Course (February 2026). It contains R Markdown documents, R analysis scripts, and presentation materials for a 5-6 day intensive training on genomics and quantitative genetics.

## Working with Materials

### Render R Markdown to HTML
```r
# In R console
rmarkdown::render("intro_to_r_styled.Rmd")

# Or from command line
Rscript -e "rmarkdown::render('filename.Rmd')"
```

### Run Analysis Scripts
```r
# In R console
source("DataFiltering.R")
source("PCA_in_R.R")
```

## Architecture

### Directory Structure
- `materials/day1-6/` - Day-by-day teaching materials (R scripts, Rmd files)
- `Day1/filesv3/` - Latest version of Day 1 intro document with clean styling
- `ssGWAS_materials/` - GWAS data files for practical exercises
- `genomicDatamgnt_TrainingMaterials/` - Genomic data management scripts

### Key Technologies
- **R/RStudio** - Primary development environment
- **R Markdown** - Document generation (Rmd → HTML)
- **PLINK** - Genetics software for QC (called via `system()` in R)

### Required R Packages
```r
install.packages(c("rmarkdown", "knitr", "ggplot2", "tidyverse", "lme4"))
```

## R Script Conventions

Scripts follow this structure:
1. `rm(list = ls())` - Clear workspace
2. `setwd()` - Set working directory explicitly
3. Load libraries
4. Read data files
5. Analysis with inline comments
6. Outputs/exports

## R Markdown Styling

- Custom CSS in `styles.css` linked via YAML header
- Self-contained HTML with embedded data download buttons
- Color scheme: Light backgrounds (#F8F9FA for code), professional palette
- Mobile-responsive and print-friendly

## Data Files

- `CT_traits_724_pc_res.csv` - Primary sheep carcass traits dataset (724 animals)
- GWAS text files in `ssGWAS_materials/`
- Data files must be in same directory as corresponding Rmd files
