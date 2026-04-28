library(tidyverse)

# Sample the dataset - 

df <- tibble(
  group = rep(c(“A”, “B”, “C”), each = 30),
  value = c(rnorm(30, mean = 10, sd = 2),
            rnorm(30, mean = 12, sd = 2.5),
            rnorm(30, mean = 11, sd = 1.8))
)

# Method 1: Manual calculation of 95% CI for the mean

# Formula: mean ± t_critical * (sd / sqrt(n))

ci_manual <- df %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value),
    se = sd / sqrt(n),
    # For 95% CI, use qt(0.975, df = n-1)
    margin = qt(0.975, df = n - 1) * se,
    ci_lower = mean - margin,
    ci_upper = mean + margin
  )

print(“Manual CI Calculation:”)
print(ci_manual)

# Method 2: Using t.test() with broom package for tidy output

library(broom)

ci_ttest <- df %>%
  group_by(group) %>%
  summarise(tidy(t.test(value))) %>%
  select(group, estimate, conf.low, conf.high)

print(”\nUsing t.test():”)
print(ci_ttest)

# Method 3: Bootstrap confidence intervals using infer package

library(infer)

ci_bootstrap <- df %>%
  group_by(group) %>%
  summarise(
    mean_value = mean(value),
    ci = list(
      df %>%
        filter(group == cur_group()$group) %>%
        specify(response = value) %>%
        generate(reps = 1000, type = “bootstrap”) %>%
        calculate(stat = “mean”) %>%
        get_ci(level = 0.95)
    )
  ) %>%
  unnest(ci)

print(”\nBootstrap CI:”)
print(ci_bootstrap)

# Visualization: Plot means with error bars

ggplot(ci_manual, aes(x = group, y = mean)) +
  geom_point(size = 3, color = “steelblue”) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                width = 0.2,
                color = “steelblue”) +
  labs(title = “Means with 95% Confidence Intervals”,
       y = “Mean Value”,
       x = “Group”) +
  theme_minimal()

# For different confidence levels (e.g., 90% or 99%)

ci_90 <- df %>%
  group_by(group) %>%
  summarise(
    mean = mean(value),
    se = sd(value) / sqrt(n()),
    margin = qt(0.95, df = n() - 1) * se,  # 0.95 for 90% CI
    ci_lower = mean - margin,
    ci_upper = mean + margin
  )

print(”\n90% Confidence Intervals:”)
print(ci_90)