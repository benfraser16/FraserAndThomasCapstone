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
library(randomForest)

# ATTENTION: Please download the dataset called "dataset_state_000" on your local device.
# The link for this is in out Github Repository: FraserAndThomasCapstone. 
# The link to downaload is located in our ReadMe.

# Load dataset
project <- read.csv("/Users/benfraser/Desktop/dataset_state_000") # change this to your file path
project %>% glimpse()

# Rename columns
colnames(project) <- c(
  "bene_count", "total_claim_count", "total_30_day_fill_count", "total_day_supply", 
  "total_drug_cost", "nppes_provider_first_name", "nppes_provider_state", "speciality_description", 
  "description_flag", "pills_per_day", "claim_per_bene", "avg_days_per_claim", 
  "employer", "nongroup", "medicaid", "medicare", "military", "uninsured", "health_spending_per_capita",
  "retailrx", "avggrowth"
)

# Remove unnecessary columns
project <- project %>%
  select(-health_spending_per_capita, -pills_per_day)

# Clean dataset: Remove columns with all NA
dataset_clean <- project %>%
  dplyr::select(all_of(colnames(project))) %>%  
  select_if(~ !all(is.na(.)))    

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






memory.limit(size = 16000)
# Random Forest model using total_drug_cost as the target
rf_model <- randomForest(total_drug_cost ~ bene_count + total_claim_count + total_30_day_fill_count + total_day_supply +
                           claim_per_bene + avg_days_per_claim + employer + nongroup + uninsured + 
                           retailrx + avggrowth,
                         data = train_data, ntree = 100, mtry = 4)

# Print Random Forest model summary
print(rf_model)

# Make predictions on the training data
rf_predictions_train <- predict(rf_model, newdata = train_data)

# Make predictions on the test data
rf_predictions_test <- predict(rf_model, newdata = test_data)

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
cat("Test R-squared:", test_r_squared, "\n")

