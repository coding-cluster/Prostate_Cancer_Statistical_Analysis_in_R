# ==========================================================================
# Tarea 3: Regression Analysis
# Problem 2: Logistic regression (seed germination, data/germ.xlsx)
# Author: Alexis Alberto Zúñiga Alonso
#
# Run this script with the working directory set to this folder
# (in RStudio/Positron: Session > Set Working Directory > To Source File Location).
# Missing packages are installed automatically.
# ==========================================================================

# --------------------------------------------------------------------------
# 1. Introduction
# --------------------------------------------------------------------------

# 1.1. Project Libraries and Installation ---------------------------------

# Project libraries
libs <- c("tidyverse", "readxl", "knitr")

# Install only the missing ones
missing <- setdiff(libs, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# Load all of them
invisible(lapply(libs, library, character.only = TRUE))

# --------------------------------------------------------------------------
# 2. Problem 2: Seed Germination (Logistic Regression)
# --------------------------------------------------------------------------

# 2.1. Initial Description of the Data ------------------------------------

germ_raw <- read_excel("data/germ.xlsx")
str(germ_raw)
colSums(is.na(germ_raw))

germ <- germ_raw |>
  mutate(
    germinated = if_else(Resultado == "Germ", 1, 0),
    Portain    = factor(Portain),
    Semilla    = factor(Semilla)
  )

# 2.1.1. Frequency Tables -------------------------------------------------

germ |> count(Portain) |> kable(caption = "Seeds per rootstock")
germ |> count(Semilla) |> kable(caption = "Seeds per seed type")
germ |> count(Resultado) |> kable(caption = "Germination result")

# 2.1.2. Germination Proportion by Group ----------------------------------

germ_summary <- germ |>
  group_by(Portain, Semilla) |>
  summarise(
    n            = n(),
    n_germinated = sum(germinated),
    proportion   = mean(germinated),
    .groups = "drop"
  )

germ_summary |> kable(digits = 3, caption = "Germination by rootstock and seed")

germ_summary |>
  ggplot(aes(x = Semilla, y = proportion, fill = Portain)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            position = position_dodge(width = 0.9), vjust = -0.4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8)) +
  labs(title = "Germination proportion by rootstock and seed",
       x = "Seed type", y = "Germinated", fill = "Rootstock") +
  theme_minimal()

# 2.2. Logistic Regression Model ------------------------------------------

# 2.2.1. Main Effects Model -----------------------------------------------

logit_main <- glm(germinated ~ Portain + Semilla, family = binomial, data = germ)
summary(logit_main)

# 2.2.2. Model with Interaction -------------------------------------------

logit_int <- glm(germinated ~ Portain * Semilla, family = binomial, data = germ)
summary(logit_int)

# 2.2.3. Model Comparison (Likelihood Ratio Test) -------------------------

anova(logit_main, logit_int, test = "Chisq")

# 2.3. Interpretation of the Coefficients ---------------------------------

odds_ratios <- exp(cbind(OR = coef(logit_int), confint.default(logit_int)))
odds_ratios |> kable(digits = 3, caption = "Odds ratios with 95% confidence intervals")

b <- coef(logit_int)

# 2.3.1. Predicted Probabilities ------------------------------------------

germ_summary |>
  select(Portain, Semilla) |>
  mutate(predicted = predict(logit_int, newdata = pick(everything()), type = "response")) |>
  kable(digits = 3, caption = "Predicted germination probability")
