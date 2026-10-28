# Brazil Unemployment Dynamics: Structural Breaks, Seasonality, and Regional Heterogeneity (2012–2026)

[![Language: R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![Framework: Quarto](https://img.shields.io/badge/Rendered_with-Quarto-447099.svg)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

An econometric investigation decomposing aggregate and subnational labor market trajectories in Brazil using **X-13ARIMA-SEATS** and robust **STL filtering** on micro-aggregated Continuous PNAD data from IBGE.

---

## Summary

* **Historical Structural Floor:** Brazil's purified trend-cycle operating at **5.3%–5.4%** in 2026 represents the lowest unemployment baseline since the beginning of the PNAD Contínua survey (breaking the 2013–2014 threshold).
* **The Year-End Calendar Trap:** Aggregate nationwide unemployment exhibits an intra-annual peak-to-trough mechanical swing of **~1.1 p.p.** (troughing in December at -0.5 to -0.6 p.p. below trend and rebounding by March/April).
* **Extreme Subnational Asymmetry:** Regional heterogeneity is sharp. While Santa Catarina operates with minimal seasonal volatility (0.69 p.p. quarterly swing), Bahia exhibits the highest seasonal volatility in Brazil (**1.94 p.p.** quarterly swing), making unadjusted readings highly misleading.

---

## Executive Reports (PDFs)

Full technical and executive documentation is available in both languages:

* 🇧🇷 **[Relatório Completo em Português (PDF)](reports/brazil_unemployment_report_pt.pdf)**
* 🇺🇸 **[Full Executive Report in English (PDF)](reports/brazil_unemployment_report_en.pdf)**

---

## Econometric Methodology

1. **Stochastic Properties & Unit Root Diagnostics:**
   * Augmented Dickey-Fuller (ADF) and KPSS tests confirm an $I(1)$ process in levels.
   * **Zivot-Andrews Test ($t = -3.404$ vs. 5% critical value of $-5.08$):** Does not reject the unit root null hypothesis, proving that non-stationarity is robust and stochastic rather than an artifact of an exogenous deterministic break. Endogenous minimum locates May 2021 as the cyclical inflection of the pandemic shock, validating the stochastic double differencing ($d=1, D=1$) applied.
2. **Signal Extraction (X-13ARIMA-SEATS):**
   * Multiplicative decomposition with SARIMA $(1,1,0)(0,1,1)_{12}$ in log scale.
   * Automated outlier filtering (SEATS) absorbing the Covid-19 structural disturbance without manual overfitting.
3. **Regional Panel STL Decomposition:**
   * LOESS-based robust decomposition applied to quarterly state series comparing structural extremes (**Santa Catarina, São Paulo, Rio de Janeiro, and Bahia**).

---

## Repository Structure

```text
├── data/              # Raw and processed PNADC data (IBGE SIDRA API)
├── scripts/           # Modular, documented R scripts (Tidyverse style)
│   ├── 01_data_extraction_sidra.R
│   ├── 02_macro_econometrics_brazil.R
│   ├── 03_regional_analysis_states.R
│   └── 04_visualizations_and_exports.R
├── outputs/
│   ├── figures/       # High-resolution econometric charts (Figures 01 to 07)
│   └── tables/        # Exported CSVs (Unit root tests, residual diagnostics, rankings)
├── reports/           # Bilingual Quarto/LaTeX-PDF executive reports
│   ├── brazil_unemployment_report_pt.qmd
│   ├── brazil_unemployment_report_pt.pdf
│   ├── brazil_unemployment_report_en.qmd
│   └── brazil_unemployment_report_en.pdf
└── README.md

```
## Reproducibility

To replicate the data pipeline and compile the reports locally:

### 1. Install & Load R Dependencies

```r
# Install pacman if not present
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

# Restore required packages
pacman::p_load(tidyverse, sidrar, seasonal, urca, tseries, fpp3, knitr, here)
```

### 2. Compile Reports via Quarto CLI

```bash
# Render Brazilian Portuguese report
quarto render reports/brazil_unemployment_report_pt.qmd --to pdf

# Render English executive report
quarto render reports/brazil_unemployment_report_en.qmd --to pdf
```
Brgds,
YAN
