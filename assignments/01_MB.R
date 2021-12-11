#-----------------------------------------------------------------------------------------------------------------------

# Assignment 1 - Statistical Rethinking w/ Chris & Brandon
# December 2021

#-----------------------------------------------------------------------------------------------------------------------

# Q1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution,
# grid approximation. Use the same flat prior as before.

# Define grid
p_grid <- seq(0, 1, length.out = 1000)

# Define prior
prior <- rep(1, 1000) # uniform

# Compute likelihood at each value in grid
likelihood <- dbinom(x = 8, size = 15, prob = p_grid)

# Compute product of prior and likelihood
unstandardized_posterior <- prior * likelihood

# Standardize posterior
posterior <- unstandardized_posterior / sum(unstandardized_posterior)

# Plot
plot(p_grid, posterior)

#-----------------------------------------------------------------------------------------------------------------------

# Q2. New prior: 0 below p = 0.5 and a constant above p = 0.5, i.e. the majority of the surface of earth is water.

new_prior <- ifelse(p_grid < 0.5, 0, 1)

# Compute product of prior and likelihood
new_unstandardized_posterior <- new_prior * likelihood

# Standardize posterior
new_posterior <- new_unstandardized_posterior / sum(new_unstandardized_posterior)

# Plot
plot(p_grid, new_posterior)

#-----------------------------------------------------------------------------------------------------------------------

# Q3. How many times do you need to toss the glob to have an answer that is very precise? e.g. 99% percentile interval
# of the posterior distribution of p to be only 0.05 wide.

library(rethinking)
library(dplyr)
library(purrr)
library(ggplot2)

# The 'guts' of it:
true_water_p  <- 0.7
N             <- 20 # initial sample size
W             <- rbinom(n = 1, size = N, prob = true_water_p)
p_grid        <- seq(0, 1, length.out = 1000)
prior         <- rep(1, 1000)
prob_data     <- dbinom(x = W, size = N, prob = p_grid)
posterior     <- prob_data * prior
std.posterior <- posterior / sum(posterior)
samples       <- sample(x = p_grid, size = 1000, prob = std.posterior, replace = TRUE)
PI99          <- rethinking::PI(samples, prob = 0.99)
PI_range      <- as.numeric(PI99[2] - PI99[1])

# Now, test at different sample sizes (N's)
df <- data.frame(
  # Choose some different sample sizes
  sample_size = c(20, 50, 100, 200, 500, 1000, 2000, 4000)) %>%
  mutate(waters = map_dbl(.x = sample_size, .f = ~ rbinom(1, size = .x, prob = true_water_p)),
         # Following columns are all list columns
         prob_data = map2(.x = waters, .y = sample_size, .f = ~ dbinom(x = .x, size = .y, prob = p_grid)),
         posterior = map(.x = prob_data, .f = ~ .x * prior),
         std.posterior = map(.x = posterior, .f = ~ .x / sum(.x)),
         sample = map(.x = std.posterior, .f = ~ sample(x = p_grid, size = 1000, replace = TRUE, prob = .x)),
         # Calculate percentile intervals
         pi = map(.x = sample, .f = ~ PI(.x, prob = 0.99)),
         # Low end of range
         pi_lower = map_dbl(.x = pi, .f = ~ pluck(.x, '1%')),
         # High end of range
         pi_higher = map_dbl(.x = pi, .f = ~ pluck(.x, '100%')),
         # Range
         pi_range = pi_higher - pi_lower) %>%
  select(sample_size, pi, pi_range)

# Probably would have been easier to just make a function, ha.
# Answer: close to 2000, slightly over that?

# Plot
df %>%
  ggplot(mapping = aes(x = sample_size, y = pi_range)) +
  geom_point(colour = "steelblue", size = 4) +
  geom_line(colour = "steelblue") +
  geom_hline(yintercept = 0.05, color = "red") +
  labs(y = "width", x = "sample size")

