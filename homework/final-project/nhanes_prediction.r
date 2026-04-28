################################################################################
# NHANES Blood Pressure Prediction Analysis
# Stats 521 Final Project
################################################################################

# 1. SETUP & PACKAGE LOADING --------------------------------------------------
# Install packages if needed (run once)
# install.packages(c("HDSinRdata", "tidyverse", "caret", "broom", "gridExtra"))

library(HDSinRdata)
library(tidyverse)
library(caret)
library(broom)
library(gridExtra)

# Set seed for reproducibility
set.seed(806)

# 2. DATA LOADING & INITIAL EXPLORATION ---------------------------------------
data("NHANESsample")
df <- NHANESsample

# View structure
cat("Dataset Structure:\n")
glimpse(df)

cat("\n\nSummary Statistics:\n")
summary(df)

# Check missing data
cat("\n\nMissing Data Count:\n")
missing_summary <- colSums(is.na(df))
print(missing_summary)
cat("\nPercentage Missing:\n")
print(round(100 * missing_summary / nrow(df), 2))

# 3. EXPLORATORY DATA ANALYSIS ------------------------------------------------

# Create EDA plots
p1 <- ggplot(df, aes(x = LEAD, y = SBP1)) + 
  geom_point(alpha = 0.2, color = "steelblue") + 
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Blood Lead vs Systolic BP",
       x = "Blood Lead Level (μg/dL)",
       y = "Systolic BP (mmHg)") +
  theme_minimal()

p2 <- ggplot(df, aes(x = SBP1)) + 
  geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of Systolic BP",
       x = "Systolic BP (mmHg)",
       y = "Count") +
  theme_minimal()

p3 <- ggplot(df, aes(x = SEX, y = SBP1, fill = SEX)) + 
  geom_boxplot(alpha = 0.7) +
  labs(title = "BP by Sex",
       x = "Sex",
       y = "Systolic BP (mmHg)") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(df, aes(x = AGE, y = SBP1)) + 
  geom_point(alpha = 0.2, color = "darkgreen") + 
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Age vs Systolic BP",
       x = "Age (years)",
       y = "Systolic BP (mmHg)") +
  theme_minimal()

# Display EDA plots
cat("\n\nGenerating EDA plots...\n")
grid.arrange(p1, p2, p3, p4, ncol = 2)

# 4. DATA PREPROCESSING -------------------------------------------------------

# Select relevant variables and handle missing data
# Using SBP1 (first systolic BP measurement) as outcome
df_model <- df %>%
  select(SBP1, LEAD, AGE, SEX, RACE) %>%
  drop_na()  # Remove rows with any missing values

cat("\n\nData after cleaning:\n")
cat("Original rows:", nrow(df), "\n")
cat("Cleaned rows:", nrow(df_model), "\n")
cat("Rows removed:", nrow(df) - nrow(df_model), "\n")

# Check for any remaining issues
cat("\nFinal missing data check:\n")
print(colSums(is.na(df_model)))

# 5. TRAIN/TEST SPLIT ---------------------------------------------------------

# Create 80/20 split
train_index <- createDataPartition(df_model$SBP1, 
                                   p = 0.8, 
                                   list = FALSE)
train_data <- df_model[train_index, ]
test_data <- df_model[-train_index, ]

cat("\n\nTrain/Test Split:\n")
cat("Training set:", nrow(train_data), "observations\n")
cat("Test set:", nrow(test_data), "observations\n")

# 6. MODEL BUILDING -----------------------------------------------------------

# Model 1: Lead only (baseline)
model1 <- lm(SBP1 ~ LEAD, 
             data = train_data)

# Model 2: Lead + Demographics
model2 <- lm(SBP1 ~ LEAD + AGE + SEX + RACE, 
             data = train_data)

# Model 3: With interaction (lead * age)
model3 <- lm(SBP1 ~ LEAD * AGE + SEX + RACE, 
             data = train_data)

cat("\n\nModels built successfully!\n")

# 7. MODEL EVALUATION ---------------------------------------------------------

# Function to calculate metrics
evaluate_model <- function(model, data, set_name) {
  predictions <- predict(model, newdata = data)
  actual <- data$SBP1
  
  tibble(
    Set = set_name,
    RMSE = sqrt(mean((actual - predictions)^2)),
    MAE = mean(abs(actual - predictions)),
    R_squared = cor(actual, predictions)^2,
    Adj_R_squared = summary(model)$adj.r.squared
  )
}

# Evaluate all models
results <- bind_rows(
  evaluate_model(model1, train_data, "Train") %>% mutate(Model = "1: Lead Only"),
  evaluate_model(model1, test_data, "Test") %>% mutate(Model = "1: Lead Only"),
  evaluate_model(model2, train_data, "Train") %>% mutate(Model = "2: + Demographics"),
  evaluate_model(model2, test_data, "Test") %>% mutate(Model = "2: + Demographics"),
  evaluate_model(model3, train_data, "Train") %>% mutate(Model = "3: + Interaction"),
  evaluate_model(model3, test_data, "Test") %>% mutate(Model = "3: + Interaction")
) %>%
  select(Model, Set, everything())

cat("\n\nMODEL PERFORMANCE COMPARISON:\n")
print(results, n = Inf)

# Visualize model comparison
results_plot <- results %>%
  filter(Set == "Test") %>%
  select(Model, RMSE, R_squared) %>%
  pivot_longer(cols = c(RMSE, R_squared), names_to = "Metric", values_to = "Value")

p_compare <- ggplot(results_plot, aes(x = Model, y = Value, fill = Model)) +
  geom_col(alpha = 0.7) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Model Comparison on Test Set",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(p_compare)

# 8. FEATURE IMPORTANCE (Best Model) ------------------------------------------

# Use standardized coefficients for Model 2
train_scaled <- train_data %>%
  mutate(across(c(SBP1, LEAD, AGE), scale))

model2_std <- lm(SBP1 ~ LEAD + AGE + SEX + RACE,
                 data = train_scaled)

# Extract and visualize coefficients
coef_plot_data <- tidy(model2_std, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = fct_reorder(term, estimate))

p_importance <- ggplot(coef_plot_data, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Feature Importance (Standardized Coefficients)",
       subtitle = "Effect size on systolic blood pressure",
       x = "Feature",
       y = "Standardized Coefficient (95% CI)") +
  theme_minimal()

print(p_importance)

# 9. MODEL DIAGNOSTICS --------------------------------------------------------

# Diagnostic plots for best model (model2)
cat("\n\nGenerating diagnostic plots for Model 2...\n")
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# Residual plot using ggplot
resid_data <- augment(model2)

p_resid <- ggplot(resid_data, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(se = TRUE, color = "darkred") +
  labs(title = "Residual Plot (Model 2)",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

print(p_resid)

# 10. PREDICTIONS VISUALIZATION -----------------------------------------------

# Actual vs Predicted for test set
test_predictions <- test_data %>%
  mutate(predicted = predict(model2, newdata = test_data))

p_pred <- ggplot(test_predictions, aes(x = SBP1, y = predicted)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted (Test Set)",
       x = "Actual Systolic BP (mmHg)",
       y = "Predicted Systolic BP (mmHg)") +
  theme_minimal()

print(p_pred)

# 11. FINAL MODEL SUMMARY -----------------------------------------------------

cat("\n\n" , rep("=", 80), "\n", sep = "")
cat("FINAL MODEL SUMMARY (Model 2: Lead + Demographics)\n")
cat(rep("=", 80), "\n\n", sep = "")
print(summary(model2))

cat("\n\nKEY FINDINGS:\n")
cat("- Best model: Model 2 (Lead + Demographics)\n")
cat("- Test R²:", round(results %>% filter(Model == "2: + Demographics", Set == "Test") %>% pull(R_squared), 4), "\n")
cat("- Test RMSE:", round(results %>% filter(Model == "2: + Demographics", Set == "Test") %>% pull(RMSE), 2), "mmHg\n")
cat("\nInterpretation:\n")
cat("- Age is the strongest predictor of blood pressure\n")
cat("- Blood lead shows a statistically significant but modest association\n")
cat("- Gender and race/ethnicity also contribute to predictions\n")

cat("\n\nAnalysis complete! Review the plots and model summary above.\n")

