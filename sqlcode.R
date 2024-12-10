# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(scales)
library(readr)
library(rsample)
library(car)
library(lm.beta)

# Load dataset
project <- read_xlsx("/Users/benfraser/Desktop/410 Final Dataset.xlsx")
project %>% glimpse()

# Rename columns
colnames(project) <- c(
  "bene_count", "total_claim_count", "total_30_day_fill_count", "total_day_supply", 
  "total_drug_cost", "nppes_provider_first_name", "nppes_provider_state", "speciality_description", 
  "description_flag", "claim_per_bene", "avg_days_per_claim", 
  "employer", "nongroup", "medicaid", "medicare", "military", "uninsured",
  "retailrx", "avggrowth"
)


# Clean dataset: Remove columns with all NA
dataset_clean <- project %>%
  dplyr::select(all_of(colnames(project))) %>%  
  select_if(~ !all(is.na(.)))    


dataset_clean %>% glimpse()
# Specify columns to check for outliers (exclude total_drug_cost)
numeric_columns <- c("bene_count", "total_claim_count", "total_30_day_fill_count", "total_day_supply",
                     "claim_per_bene", "avg_days_per_claim", "employer", "nongroup", "uninsured", 
                     "retailrx", "avggrowth")

# Calculate probability for log odds transformation
# Normalize total_drug_cost to a probability between 0 and 1
dataset_clean$prob_drug_cost <- (dataset_clean$total_drug_cost - min(dataset_clean$total_drug_cost)) / 
  (max(dataset_clean$total_drug_cost) - min(dataset_clean$total_drug_cost))

# Apply logit transformation (log odds) with a small constant epsilon to avoid infinities
epsilon <- 0.000001
dataset_clean$logit_drug_cost <- log((dataset_clean$prob_drug_cost + epsilon) / 
                                       (1 - dataset_clean$prob_drug_cost + epsilon))

# Split the data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(dataset_clean$logit_drug_cost, p = 0.8, list = FALSE)
train_data <- dataset_clean[train_indices, ]
test_data <- dataset_clean[-train_indices, ]

# Fit regression model using numeric predictors
predictor_vars <- paste(numeric_columns, collapse = " + ")
formula <- as.formula(paste("logit_drug_cost ~", predictor_vars))
log_odds_model <- lm(formula, data = train_data)

# Summary of the regression model
summary(log_odds_model)





# Make predictions on training data (keep them in the logit space)
log_odds_predictions_train <- predict(log_odds_model, newdata = train_data)

# Make predictions on test data (keep them in the logit space)
log_odds_predictions_test <- predict(log_odds_model, newdata = test_data)

# Calculate performance metrics for the training data
train_rmse <- sqrt(mean((log_odds_predictions_train - train_data$logit_drug_cost)^2))
train_mae <- mean(abs(log_odds_predictions_train - train_data$logit_drug_cost))
train_r_squared <- cor(log_odds_predictions_train, train_data$logit_drug_cost)^2

# Calculate performance metrics for the test data
test_rmse <- sqrt(mean((log_odds_predictions_test - test_data$logit_drug_cost)^2))
test_mae <- mean(abs(log_odds_predictions_test - test_data$logit_drug_cost))
test_r_squared <- cor(log_odds_predictions_test, test_data$logit_drug_cost)^2

# Print performance metrics
cat("Train RMSE (logit space):", train_rmse, "\n")
cat("Train MAE (logit space):", train_mae, "\n")
cat("Train R-squared (logit space):", train_r_squared, "\n\n")

cat("Test RMSE (logit space):", test_rmse, "\n")
cat("Test MAE (logit space):", test_mae, "\n")
cat("Test R-squared (logit space):", test_r_squared, "\n")




# Standardize coefficients
# Calculate standardized coefficients
std_coef <- lm.beta(log_odds_model)

# Create importance dataframe
var_imp_df <- data.frame(
  Feature = names(std_coef$standardized.coefficients)[-1],  # Exclude intercept
  Importance = abs(std_coef$standardized.coefficients[-1])
)

# Plot standardized variable importance
ggplot(var_imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Variable Importance (Standardized Coefficients)", 
       x = "Variables", 
       y = "Absolute Standardized Coefficient")


# Calculate residuals
train_data$residuals <- train_data$logit_drug_cost - log_odds_predictions_train



# Random Forest model using log_total_drug_cost as the target
rf_model <- randomForest(log_total_drug_cost ~ bene_count + total_claim_count + total_30_day_fill_count + total_day_supply +
                           claim_per_bene + avg_days_per_claim + employer + nongroup + uninsured + 
                           retailrx + avggrowth,
                         data = train_data, ntree = 100, mtry = 4)

# Print Random Forest model summary
print(rf_model)

# Make predictions on the training data and convert back to original scale
rf_predictions_train <- exp(predict(rf_model, newdata = train_data))

# Make predictions on the test data and convert back to original scale
rf_predictions_test <- exp(predict(rf_model, newdata = test_data))

# Calculate performance metrics for the training data
train_rmse <- sqrt(mean((rf_predictions_train - train_data$total_drug_cost)^2))
train_mae <- mean(abs(rf_predictions_train - train_data$total_drug_cost))
train_r_squared <- cor(rf_predictions_train, train_data$total_drug_cost)^2

# Calculate performance metrics for the test data
test_rmse <- sqrt(mean((rf_predictions_test - test_data$total_drug_cost)^2))
test_mae <- mean(abs(rf_predictions_test - test_data$total_drug_cost))
test_r_squared <- cor(rf_predictions_test, test_data$total_drug_cost)^2

# Print performance metrics
cat("Train RMSE:", train_rmse, "\n")
cat("Train MAE:", train_mae, "\n")
cat("Train R-squared:", train_r_squared, "\n\n")

cat("Test RMSE:", test_rmse, "\n")
cat("Test MAE:", test_mae, "\n")
cat("Test R-squared:", test_r_squared, "\n\n")



# Extract feature importance from the model
importance_data <- data.frame(
  Variable = rownames(rf_model$importance),
  Importance = rf_model$importance[, "IncNodePurity"]
)

# Plot
ggplot(importance_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance (Random Forest)",
    x = "Features",
    y = "Importance (Increase in Node Purity)"
  ) +
  theme_minimal()

