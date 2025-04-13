# 📊 Peak Electricity Demand Modeling Project

This repository contains the full analysis and modeling workflow for predicting peak electricity demand using weather, calendar, and renewable generation data. The project was developed as part of a university statistical computing course and showcases advanced modeling techniques for operational and long-term energy planning.

---

## 🔍 Overview

The project explores how daily peak electricity demand can be modeled and forecasted using:

- Weather data (temperature, wind, solar)
- Calendar features (day of week, month, weekend)
- Historical structural trends (e.g., pre/post 2006 shifts)

We focus particularly on accurately estimating demand during **extreme weather days** and testing model robustness under different historical weather scenarios.

---

## 📁 Project Structure

- `code.R`: All data preparation, feature engineering, modeling, and visualization logic.
- `Report_project02.Rmd`: R Markdown report including full results, tables, and visualizations.
- `SCS_demand_modelling.csv` & `SCS_hourly_temp.csv`: Datasets used in the project (not included here).

---

## 🧠 Key Features

### ✅ Data Processing & Feature Engineering
- Merging daily and hourly datasets
- Cleaning, handling missing values, deduplication
- Polynomial and segmented trend features (e.g., `TE²`, `trend_post`)
- Encoding calendar effects (weekday, month, weekend)

### 📈 Modeling
- Linear models (baseline, interaction terms, segmented trends)
- Polynomial regression
- Regularized models: Ridge and Lasso
- Quantile regression for renewable effects
- Cross-validation using stratified (monthly) folds

### 🌡️ Weather Scenario Simulation
- Replaces 2013–14 winter weather with historical winters (1991, 1992, 1993, 2005)
- Analyzes sensitivity of maximum demand to varying weather

---

## 📊 Model Comparison

| Model | Adjusted R² | RMSE | MAE | Extreme MAE |
|-------|-------------|------|-----|--------------|
| M0 (Baseline) -Basic features only | 0.464 | 3739.9 | 2960.7 | 5023.7 |
| M1 -Adds temperature & trends | 0.780 | 2394.9 | 1512.7 | 1916.8 |
| M2 (Final) -Adds non-linearities & interactions | 0.795 | 2307.3 | 1420.2 | 1917.7 |

Results are also evaluated using:
- **Mean Squared Error (MSE)**
- **Dawid-Sebastiani Score (DS)**

---

## 🧪 How to Run

1. Clone the repo:
   ```bash
   git clone https://github.com/yourusername/peak-demand-modeling.git
   cd peak-demand-modeling
   ```
2. Open `Report_project02.Rmd` in RStudio.
3. Run the code chunks sequentially, or click **Knit to HTML** to generate the full report.

---

## 📦 Required R Packages

To install all necessary packages, run the following in your R console:

```r
install.packages(c(
  "ggplot2", "patchwork", "tidyverse", "lubridate", "lme4", "knitr",
  "kableExtra", "car", "quantreg", "caret", "glmnet", "broom"
))
```

## 📘 Authors
- Project by: Sulixiang Lu, Shiyuan Feng, Yichong Gao
- Course: Statistical Computing - Year 3 Project

---

## 📄 License
This project is for academic use only.
Please do not redistribute without permission.
```vbnet
Let me know if you want badges (like CRAN/R version, knit status, etc.) added at the top!
```
