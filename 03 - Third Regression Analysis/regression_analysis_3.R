# ==========================================================================
# Tarea 3: Regression Analysis
# Problem 3: Poisson regression (cervical cancer deaths, data/cervical.xlsx)
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
# 2. Problem 3: Cervical Cancer Deaths (Poisson Regression)
# --------------------------------------------------------------------------

# 2.1. Initial Description of the Data ------------------------------------

cervical_raw <- read_excel("data/cervical.xlsx")
str(cervical_raw)
colSums(is.na(cervical_raw))

cervical <- cervical_raw |>
  mutate(
    Pais = factor(Pais),
    Edad = factor(Edad, levels = c("25to34", "35to44", "45to54", "55to64"))
  )

# 2.1.1. Deaths by Country and Age Group ----------------------------------

cervical |>
  pivot_wider(names_from = Edad, values_from = Muertes) |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  kable(caption = "Deaths by country and age group")

summary(cervical$Muertes)

cervical |>
  ggplot(aes(x = Edad, y = Muertes, color = Pais, group = Pais)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Cervical cancer deaths by age group and country",
       x = "Age group", y = "Deaths", color = "Country") +
  theme_minimal()

# 2.2. Poisson Regression Model -------------------------------------------

poisson_model <- glm(Muertes ~ Edad + Pais, family = poisson, data = cervical)
summary(poisson_model)

# 2.2.1. Significance of the Variables ------------------------------------

anova(poisson_model, test = "Chisq")

# 2.2.2. Goodness of Fit and Overdispersion -------------------------------

dispersion <- sum(residuals(poisson_model, type = "pearson")^2) / df.residual(poisson_model)
dispersion

# Goodness-of-fit test based on the residual deviance
pchisq(deviance(poisson_model), df.residual(poisson_model), lower.tail = FALSE)

quasi_model <- glm(Muertes ~ Edad + Pais, family = quasipoisson, data = cervical)
summary(quasi_model)

# 2.3. Interpretation of the Coefficients ---------------------------------

rate_ratios <- exp(cbind(RR = coef(poisson_model), confint.default(quasi_model)))
rate_ratios |> kable(digits = 3, caption = "Rate ratios with 95% confidence intervals (quasi-Poisson)")

rr <- exp(coef(poisson_model))

# 2.3.1. Observed vs Fitted Deaths ----------------------------------------

cervical |>
  mutate(fitted = fitted(poisson_model)) |>
  ggplot(aes(x = Muertes, y = fitted, color = Pais)) +
  geom_abline(linetype = "dashed") +
  geom_point(size = 3) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Observed vs fitted deaths (log scale)",
       x = "Observed deaths", y = "Fitted deaths", color = "Country") +
  theme_minimal()
