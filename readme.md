# Reliability and Agreement Analysis Pipeline (R & Quarto)

This repository contains a comprehensive statistical pipeline implemented in **R** and **Quarto** for evaluating **inter-rater** and **intra-rater reliability** using continuous and categorical clinical/experimental measurements.

------------------------------------------------------------------------

## Project Structure

``` text
├── data/
│   └── dati.xlsx          # Synthetic dataset (AI-generated for demonstration purposes)
├── export/                # Automatically generated tables (.rds, .xlsx) and plots (.svg, .pdf)
├── analysis.R             # Main R script for statistical calculations and visualization
├── report.qmd             # Quarto report script for compiling the final document
└── README.md              # Project documentation
```

------------------------------------------------------------------------

## Important Data Disclaimer

> **⚠️ NOTE ON THE DATASET:** The dataset contained in `data/dati.xlsx` is **purely synthetic and randomly generated using artificial intelligence (AI)**. It does not contain any real patient, clinical, or confidential data. This file is provided exclusively for testing, demonstration, and educational purposes to showcase the automated analysis and reporting pipeline.

------------------------------------------------------------------------

## Key Features of the Analysis

The pipeline automatically computes and visualizes:

1.  **Sample Size Estimation:** Based on Bonett’s (2002) approach for Intraclass Correlation Coefficients (ICC).

2.  **Continuous Agreement (ICC):** Two-way agreement ICC models with 95% Confidence Intervals, F-values, and p-values.

3.  **Categorical Agreement (Cohen's Kappa):** Confusion matrices and unweighted/weighted Kappa coefficients across multiple operators and rounds.

4.   **Bland-Altman Analysis:** Bias, Standard Deviation of differences, Limits of Agreement (LoA), and **Shapiro-Wilk normality tests** on the residuals.

5.  **High-Quality Visualizations:** Custom publication-ready `ggplot2` charts including Bland-Altman plots and confusion matrix heatmaps.

6.  **Automated Exports:** Batch saving of summary tables (`.rds` and multi-sheet `.xlsx`) and plots (`.svg` and `.pdf`).

------------------------------------------------------------------------

## Prerequisites & Installation

To run this script, ensure you have **R** (version 4.0+) and **RStudio** installed. The script will automatically check, install, and load any missing required packages:

``` r
install.packages(c("dplyr", "ggplot2", "ICC", "irr", "janitor", "openxlsx", "readxl", "svglite", "tidyr"))
```

------------------------------------------------------------------------

## Usage

1.  Place your input Excel file inside the `data/` folder (ensure it matches the expected column structure or replace `dati.xlsx` with your own file).
2.  Run the main analysis script (`analysis.R`).
3.  Check the `export/` folder for generated tabular outputs and graphic assets.
4.  Compile the Quarto report (`report.qmd`) to generate a fully formatted PDF/HTML report.

------------------------------------------------------------------------

## 📄 License

This project is open-source and available under the [MIT License](LICENSE).
