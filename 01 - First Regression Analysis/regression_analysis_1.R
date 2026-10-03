# ==========================================================================
# Tarea 3: Regression Analysis
# Problem 1: Multiple linear regression (prostate dataset, genridge)
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
libs <- c("genridge", "tidyverse", "skimr", "corrplot", "car",
          "lmtest", "glmnet", "MASS", "performance", "see", "gt", "psych")

# Install only the missing ones
missing <- setdiff(libs, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

# Load all of them
invisible(lapply(libs, library, character.only = TRUE))

# 1.2. Dataset Analysis ---------------------------------------------------

# 1.2.1. Importing the dataset --------------------------------------------

# (Not run) # Information about the library
# (Not run) library(genridge)
# (Not run) ?prostate

# 1.2.2. Dataset ----------------------------------------------------------

# Dataframe creation
data(prostate, package="genridge")
df <- prostate |> dplyr::select(-train)
# Stringify dataset
str(df)
# Dataset summary
print(summary(df))
# Skim through the data
skimr::skim(df)
# Head of the dataset
print(head(df))
# Tail of the dataset
print(tail(df))
# Shape / Dimensions of the Dataset
print(dim(df))

# 1.3. Descriptive Statistics ---------------------------------------------

# Descriptive statistics with psych
psych::describe(df) |>
  as.data.frame() |>
  dplyr::select(n, mean, sd, median, min, max, range, skew, kurtosis, se) |>
  round(2) |>
  print()

# 1.3.1. Histograms -------------------------------------------------------

df |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20, fill = "coral", color = "red") +
  facet_wrap(~ variable, scales = "free") +
  labs(title = "Distribution of every variable", x = NULL, y = "Frequency") +
  theme_minimal()

# 1.3.2 Normality Test ----------------------------------------------------

# Shapiro-Wilk on lcavol
shapiro.test(df$lcavol)
# Shapiro-Wilk on lweight
shapiro.test(df$lweight)
# Shapiro-Wilk on age
shapiro.test(df$age)
# Shapiro-Wilk on lbph
shapiro.test(df$lbph)
# Shapiro-Wilk on svi
shapiro.test(df$svi)
# Shapiro-Wilk on lcp
shapiro.test(df$lcp)
# Shapiro-Wilk on gleason
shapiro.test(df$gleason)
# Shapiro-Wilk on pgg45
shapiro.test(df$pgg45)
# Shapiro-Wilk on lpsa
shapiro.test(df$lpsa)

# 1.4. Correlation Matrix -------------------------------------------------

corrplot(cor(df), method = "color", addCoef.col = "black",
         number.cex = 0.75, tl.col = "black")

# --------------------------------------------------------------------------
# 2. Train / Test Split
# --------------------------------------------------------------------------

set.seed(123)
idx   <- sample(nrow(df), 0.7 * nrow(df))
train <- df[idx, ]
test  <- df[-idx, ]

cat("Train:", nrow(train), "observations\n")
cat("Test: ", nrow(test), "observations\n")

# --------------------------------------------------------------------------
# 3. Multiple Linear Regression (Full Model)
# --------------------------------------------------------------------------

full_model <- lm(lpsa ~ ., data = train)
summary(full_model)

# 3.1. Multicollinearity (VIF) --------------------------------------------

vif(full_model)

# --------------------------------------------------------------------------
# 4. Variable Selection Methods
# --------------------------------------------------------------------------

# 4.1. Forward, Backward and Stepwise -------------------------------------

null_model <- lm(lpsa ~ 1, data = train)

forward_model  <- step(null_model, scope = formula(full_model),
                       direction = "forward", trace = 0)
backward_model <- step(full_model, direction = "backward", trace = 0)
stepwise_model <- step(null_model, scope = formula(full_model),
                       direction = "both", trace = 0)

formula(forward_model)
formula(backward_model)
formula(stepwise_model)

# 4.2. Lasso, Ridge and Elastic Net ---------------------------------------

x_train <- model.matrix(lpsa ~ ., data = train)[, -1]
y_train <- train$lpsa
x_test  <- model.matrix(lpsa ~ ., data = test)[, -1]

set.seed(123)
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1)
set.seed(123)
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0)
set.seed(123)
enet_cv  <- cv.glmnet(x_train, y_train, alpha = 0.5)

par(mfrow = c(1, 3))
plot(lasso_cv, main = "Lasso")
plot(ridge_cv, main = "Ridge")
plot(enet_cv,  main = "Elastic Net")
par(mfrow = c(1, 1))

penalized_coefs <- cbind(
  as.matrix(coef(lasso_cv, s = "lambda.min")),
  as.matrix(coef(ridge_cv, s = "lambda.min")),
  as.matrix(coef(enet_cv,  s = "lambda.min"))
)
colnames(penalized_coefs) <- c("Lasso", "Ridge", "Elastic Net")
round(penalized_coefs, 4)

# --------------------------------------------------------------------------
# 5. Model Comparison (RMSE and MAE)
# --------------------------------------------------------------------------

rmse <- function(y, pred) sqrt(mean((y - pred)^2))
mae  <- function(y, pred) mean(abs(y - pred))

predictions <- list(
  "Full model"  = predict(full_model, test),
  "Forward"     = predict(forward_model, test),
  "Backward"    = predict(backward_model, test),
  "Stepwise"    = predict(stepwise_model, test),
  "Lasso"       = as.numeric(predict(lasso_cv, x_test, s = "lambda.min")),
  "Ridge"       = as.numeric(predict(ridge_cv, x_test, s = "lambda.min")),
  "Elastic Net" = as.numeric(predict(enet_cv,  x_test, s = "lambda.min"))
)

metrics <- tibble(
  Model = names(predictions),
  RMSE  = map_dbl(predictions, function(p) rmse(test$lpsa, p)),
  MAE   = map_dbl(predictions, function(p) mae(test$lpsa, p))
)

metrics |>
  arrange(RMSE) |>
  mutate(across(c(RMSE, MAE), \(x) round(x, 4))) |>
  as.data.frame() |>
  print()

# --------------------------------------------------------------------------
# 6. Analysis of the Best Model (Backward)
# --------------------------------------------------------------------------

best_model <- backward_model
summary(best_model)

# 6.1. Significance of the Model (ANOVA and F Statistic) ------------------

anova(best_model)

f <- summary(best_model)$fstatistic
p_value <- pf(f["value"], f["numdf"], f["dendf"], lower.tail = FALSE)
cat("F =", round(f["value"], 3), "on", f["numdf"], "and", f["dendf"],
    "DF,  p-value =", signif(p_value, 3), "\n")

# 6.2. Assumption Checks --------------------------------------------------

par(mfrow = c(2, 2))
plot(best_model)
par(mfrow = c(1, 1))

# 6.2.1. Normality of the Residuals ---------------------------------------

qqnorm(residuals(best_model), main = "Normal Q-Q Plot of the Residuals")
qqline(residuals(best_model), col = "red")
shapiro.test(residuals(best_model))

# 6.2.2. Homoscedasticity -------------------------------------------------

plot(fitted(best_model), residuals(best_model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted", pch = 19, col = "coral")
abline(h = 0, col = "red", lty = 2)
bptest(best_model)

# 6.2.3. Independence -----------------------------------------------------

plot(residuals(best_model), type = "b",
     xlab = "Observation order", ylab = "Residuals",
     main = "Residuals vs Order", pch = 19, col = "coral")
abline(h = 0, col = "red", lty = 2)
dwtest(best_model)

# --------------------------------------------------------------------------
# 7. Conclusions
# --------------------------------------------------------------------------
