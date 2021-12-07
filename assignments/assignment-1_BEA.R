#
# Title: Assignment 1
# Created: December 1st, 2021
# Last Updated: December 7th, 2021
# Author: Brandon Allen
# Objectives: Create examples illustrating how dispersal distances will impact the indicator
# Keywords: Setup, Question 1, Question 2, Question 3
# Notes:
#

#########
# Setup # Completed December 1st, 2021
#########~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Clear memory
rm(list=ls())
gc()

# install.packages(c("coda","mvtnorm","devtools","loo","dagitty"))
# devtools::install_github("stan-dev/cmdstanr")
# devtools::install_github("rmcelreath/rethinking")

library(rstan)
library(rethinking)

# Failed to install libraries
# curl, rlang, ellipsis, glue, ps

##############
# Question 1 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution, using grid approximaton.
# Use the same flat prior as before.

# Clear memory
rm(list=ls())
gc()

p.grid <- seq(from = 0, to = 1, length.out = 1000) # Create grid
prior <- rep(1, 1000) # Create flat prior distribution
prob.data <- dbinom(x = 8, size = 15, prob = p.grid) # Create probablility data based on 15 tosses
posterior <- prob.data * prior # Multiply prior by data
posterior <- posterior / sum(posterior) # Standardize back to probabilities

plot(posterior) # Visualize

##############
# Question 2 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Start over in 1, but now use a prior that is zero below p = 0.5 and constant above p = 0.5.
# This corresponds to prior information that a majority of the Earth's surface is water.
# What difference does the better prior make?
# If it helps, compare posterior distributions (using both priors) to the true value p = 0.7.

# Clear memory
rm(list=ls())
gc()

p.grid <- seq(from = 0, to = 1, length.out = 1000) # Create grid
prior <- c(rep(0, 500), rep(1, 500)) # Create non-flat prior distribution
prob.data <- dbinom(x = 8, size = 15, prob = p.grid) # Create probablility data based on 15 tosses
posterior <- prob.data * prior # Multiply prior by data
posterior <- posterior / sum(posterior) # Standardize back to probabilities

plot(posterior) # Visualize

# The improved prior removes values we know are impossible from the posterior distribution

##############
# Question 3 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This problem is more open-ended than the others. Feel free to collaborate onthe solution.
# Suppose you want to estimate the Earth's proportion of water very precisely.
# Specifically, you want the 99% percentile interval of the posterior distributions of p to be only 0.05 wide.
# This means the distance between the upper and lower bound of the interval should be 0.05.
# How many times will you have to toss the globe to do this?
# I won't require a precise answer. I'm honestly more interested in your approach.

# Clear memory
rm(list=ls())
gc()

# Define parameter values
threshold <- 0.5
n.water <- 2
n.sample <- 2*n.water

while (threshold >= 0.05) {

  p.grid <- seq(from = 0, to = 1, length.out = 1000) # Create grid
  prior <- rep(1, 1000) # Create non-flat prior distribution
  prob.data <- dbinom(x = n.water, size = n.sample, prob = p.grid) # Create probablility data based on 15 tosses
  posterior <- prob.data * prior # Multiply prior by data
  posterior <- posterior / sum(posterior) # Standardize back to probabilities

  samples <- sample(x = p.grid, prob = posterior, size = 1000, replace= TRUE) # Sample the distribution

  threshold <- as.numeric(PI(samples, prob = 0.5)[2] - PI(samples, prob = 0.5)[1])

  # Update sample
  n.water <- n.water + 2
  n.sample <- 2*n.water

  print(threshold)

}

print(n.sample)
