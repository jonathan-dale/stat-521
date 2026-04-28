# Load some data
load("ames.RData")

# Set a random seed
set.seed(335)

# Create a population vector variable and view its summary
population <- ames$Gr.Liv.Area
summary(population)


# Take a sample from our population of size n = 60
samp <- sample(population, 60)

# View some of the data points from our sample and see summary
head(samp)
summary(samp)


# Create a histogram from sample and populations
hist(samp)
hist(population)


# Calculate mean, standard error, and lower/upper confidence interval.
sample_mean <- mean(samp)
se <- sd(samp) / sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se
c(lower, upper)




samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60

for(i in 1:50){
  # obtain a sample of size n = 60 from the population
  samp <- sample(population, n) 
  # save sample mean in ith element of samp_mean
  samp_mean[i] <- mean(samp) 
  # save sample sd in ith element of samp_sd
  samp_sd[i] <- sd(samp)        
}

lower_vector <- samp_mean - 1.96 * samp_sd / sqrt(n) 
upper_vector <- samp_mean + 1.96 * samp_sd / sqrt(n)

c(lower_vector[1], upper_vector[1])

source("plot_ci_v2.R")
plot_ci(lower_vector, upper_vector, mean(population))

