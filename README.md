# Regression Analysis

This is my solution to Homework 3 (Tarea 3) of my regression analysis course. It has three problems, and each one lives in its own folder with the data, the Quarto notebook, the final PDF report and a plain R script.

All the code is written in R. The reports were built with Quarto inside Positron.

## What's inside

```
12 - Regression Analysis/
├── 01 - First Regression Analysis/    Multiple linear regression
├── 02 - Second Regression Analysis/   Logistic regression
├── 03 - Third Regression Analysis/    Poisson regression
├── LICENSE
└── README.md
```

Every folder has the same files:

| File | What it is |
|---|---|
| `notebook.qmd` | The Quarto notebook with the code, the results and my comments |
| `notebook.pdf` | The report I hand in |
| `notebook.html` | The same report as a web page |
| `regression_analysis_N.R` | Only the code, so it can be run without Quarto |
| `data/` | The Excel file used in that problem (folders 02 and 03) |

### 1. Prostate cancer (multiple linear regression)

Uses the `prostate` dataset from the `genridge` package to predict the level of prostate specific antigen (`lpsa`) from eight clinical variables.

What I did:

- Described every variable with summary statistics, histograms and Shapiro-Wilk tests.
- Made a correlation plot of all the variables.
- Split the data into 70% for training and 30% for testing, with `set.seed(123)`.
- Fitted a full linear model and checked multicollinearity with the VIF.
- Selected variables with forward, backward and stepwise selection, plus Lasso, Ridge and Elastic Net.
- Compared all the models on the test set with RMSE and MAE.
- Took the best method (backward selection) and checked its ANOVA table, its F statistic, and the normality, homoscedasticity and independence of its residuals.

### 2. Seed germination (logistic regression)

Uses `germ.xlsx`, where 831 seeds of two types (OA73 and OA75) were planted on two rootstocks (bean and cucumber). The question is whether the rootstock and the seed type decide if a plant germinates.

I compared a model with only the main effects against one with an interaction. The interaction turned out to be significant, so I interpreted that model using odds ratios. The best combination was cucumber with OA75, with about a 68% chance of germinating.

### 3. Cervical cancer deaths (Poisson regression)

Uses `cervical.xlsx`, with the number of deaths from cervical cancer between 1969 and 1973 in four countries (Belgium, England and Wales, France and Italy), split into four age groups.

I fitted a Poisson model with age and country as explanatory variables and read the coefficients as rate ratios. Deaths go up sharply with age in every country. The model showed overdispersion, so I also fitted a quasi-Poisson model to make sure the conclusions still held, and they did. One thing to keep in mind: the data doesn't include the population of each country, so the differences between countries mostly reflect how big each country is, not how risky it is.

## How to run it

You'll need R (I used version 4.6.1). Positron or RStudio make it easier, but they're optional.

**Just the R scripts.** Open the folder of the problem you want, set it as your working directory and run the script:

```r
setwd("path/to/02 - Second Regression Analysis")
source("regression_analysis_2.R", echo = TRUE)
```

Each script installs any package it's missing the first time you run it, so you don't need to install anything by hand.

**The full reports.** Open `notebook.qmd` in Positron and press **Preview**, or run this from a terminal inside the folder:

```bash
quarto render notebook.qmd
```

This rebuilds both the HTML and the PDF. Positron already comes with Quarto, so there's nothing else to set up.

## Packages used

`tidyverse`, `readxl`, `knitr`, `genridge`, `skimr`, `psych`, `corrplot`, `car`, `lmtest`, `glmnet`, `MASS`, `performance`, `see` and `gt`.

## License

This project is under the MIT License. See [LICENSE](LICENSE) for the details.
