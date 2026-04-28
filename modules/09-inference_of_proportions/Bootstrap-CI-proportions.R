## ---- Bootstrap Percentile CI for proportion ----
set.seed(123)

# GSS result
n  <- 670
y  <- 571
p_hat <- y / n

# Construct the observed 0/1 data vector: 1 = correct, 0 = incorrect
# (571 ones, 99 zeros)
x <- c(rep(1, y), rep(0, n - y))

## ---- Bootstrap  ----
B <- 10000  # number of bootstrap resamples

# Resample indices with replacement and compute p-hat each time
p_star <- replicate(B, {
  xb <- sample(x, size = n, replace = TRUE)
  mean(xb)
})

# 95% percentile CI (change probs for a different confidence level)
alpha <- 0.05
ci_percentile <- quantile(p_star, probs = c(alpha/2, 1 - alpha/2))

# Report results
p_hat
ci_percentile
