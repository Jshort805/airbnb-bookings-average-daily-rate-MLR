# 03_model.R -- Multiple linear regression for nightly price (outcome: log of price)
# Input : Data/clean/airbnb_analytic.csv  (made by 01_clean.R)
# Output: Outputs/model_comparison.csv, Outputs/model_test_results.csv, Outputs/model_coefficients.csv
# Run from the project root:  Rscript Scripts/03_model.R
# Plan: split the data, compare candidate models on the training data only (cross-validation),
#       pick one, then score the test data once.

set.seed(2026)

## Part 1: Read the data and set up the variables
df <- read.csv("Data/clean/airbnb_analytic.csv")
df$log_price <- log(df$price)

# Reference categories (the group every other category is compared against): the biggest group
df$room_type <- relevel(factor(df$room_type), ref = "Entire home/apt")
df$neighborhood_grp <- relevel(factor(df$neighborhood_grp), ref = "Other")
common_type <- names(which.max(table(df$property_type_grp)))
df$property_type_grp <- relevel(factor(df$property_type_grp), ref = common_type)

# Candidate predictors. Left out on purpose: id (just a label), price (the outcome),
# latitude and longitude (dist_lax_mi and neighborhood_grp already describe location).
predictors <- c("room_type", "property_type_grp", "neighborhood_grp", "accommodates",
                "bedrooms", "beds", "bathrooms", "shared_bath", "n_amenities",
                "minimum_nights", "has_license", "dist_lax_mi", "host_is_superhost",
                "host_identity_verified", "hosts_time_as_host_years",
                "calculated_host_listings_count")

## Part 2: Split into training (80%) and test (20%) data. The test data stays untouched until Part 5.
train_rows <- sample(nrow(df), size = round(0.8 * nrow(df)))
train <- df[train_rows, ]
test <- df[-train_rows, ]
cat("Training rows:", nrow(train), " Test rows:", nrow(test), "\n")

# Error measures
rmse <- function(actual, predicted) sqrt(mean((actual - predicted)^2))
mae <- function(actual, predicted) mean(abs(actual - predicted))
r2 <- function(actual, predicted, baseline) 1 - sum((actual - predicted)^2) / sum((actual - baseline)^2)

## Part 3: Candidate models (fit on the training data only)
f_small <- log_price ~ accommodates + room_type + neighborhood_grp
f_full <- as.formula(paste("log_price ~", paste(predictors, collapse = " + ")))
full_fit <- lm(f_full, data = train)
reduced_fit <- step(full_fit, trace = 0)  # drops predictors that do not improve the fit (AIC)
models <- list(Baseline = log_price ~ 1, Small = f_small, Full = f_full,
               Reduced = formula(reduced_fit))
cat("Predictors dropped by step():",
    setdiff(predictors, attr(terms(reduced_fit), "term.labels")), "\n")

## Part 4: Compare the candidates with 5-fold cross-validation on the training data
folds <- sample(rep(1:5, length.out = nrow(train)))
comparison <- NULL
for (name in names(models)) {
  errors <- numeric(5)
  for (k in 1:5) {
    fit <- lm(models[[name]], data = train[folds != k, ])
    pred <- predict(fit, newdata = train[folds == k, ])
    errors[k] <- rmse(train$log_price[folds == k], pred)
  }
  adj_r2 <- summary(lm(models[[name]], data = train))$adj.r.squared
  comparison <- rbind(comparison,
    data.frame(model = name, n_terms = length(coef(lm(models[[name]], data = train))) - 1,
               adj_r2 = round(adj_r2, 3), cv_rmse_log = round(mean(errors), 4)))
}
write.csv(comparison, "Outputs/model_comparison.csv", row.names = FALSE)
print(comparison)

# Pick the model with the lowest cross-validated error
ok <- comparison$cv_rmse_log <= min(comparison$cv_rmse_log) + 0.001
best <- comparison$model[ok & comparison$n_terms == min(comparison$n_terms[ok])]
cat("Chosen model:", best, "\n")
final_fit <- lm(models[[best]], data = train)

## Part 5: Score the test data once (errors in dollars: convert log predictions back with exp)
pred_final <- predict(final_fit, newdata = test)
pred_base <- rep(mean(train$log_price), nrow(test))
test_results <- data.frame(
  model = c("Baseline (average only)", paste("Final:", best)),
  rmse_usd = round(c(rmse(test$price, exp(pred_base)), rmse(test$price, exp(pred_final))), 2),
  mae_usd = round(c(mae(test$price, exp(pred_base)), mae(test$price, exp(pred_final))), 2),
  r2_log_price = round(c(r2(test$log_price, pred_base, mean(train$log_price)),
                         r2(test$log_price, pred_final, mean(train$log_price))), 3)
)
write.csv(test_results, "Outputs/model_test_results.csv", row.names = FALSE)
print(test_results)

## Part 6: Coefficients of the final model
# pct_effect: approximate percent change in price for a one-unit increase (or versus the
# reference category), holding the other variables fixed
coefs <- summary(final_fit)$coefficients
coef_table <- data.frame(term = rownames(coefs), estimate = round(coefs[, 1], 4),
                         std_error = round(coefs[, 2], 4), p_value = signif(coefs[, 4], 3),
                         pct_effect = round((exp(coefs[, 1]) - 1) * 100, 1), row.names = NULL)
coef_table$pct_effect[coef_table$term == "(Intercept)"] <- NA
write.csv(coef_table, "Outputs/model_coefficients.csv", row.names = FALSE)
print(coef_table)
cat("Final model adjusted R-squared (training):", round(summary(final_fit)$adj.r.squared, 3), "\n")