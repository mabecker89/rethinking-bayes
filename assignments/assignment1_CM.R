# Assignment 1
# Chris Mallon
# December 9, 2021


# libraries ---------------------------------------------------------------

library(renv)
library(rethinking)


# Preparatory Exercises ---------------------------------------------------

# recall that to compute the probability of w W's in n tosses, we can use
# the binomial distribution (just like a coin toss).

p = dbinom(6, size = 9, prob = 0.5) # 6 Ws in 9 tosses, assuning 50/50 odds
p # 0.1640625
rm(p)

p = dbinom(6, size = 9, prob = seq(0, 1, 0.1))
plot(p) # um, neat. Is this grid approximation?
rm(p)

# Problem 1 ---------------------------------------------------------------

# Suppose the globe tossing data had turned out to be 8 water in 15 tosses.
# Construct the posterior distribution, using grid approximation. Use the
# same flat prior as before.

# There are 8 waters found in 15 globe tosses.

W <- 8
n <- 15
grid_length <- 100

## Data story
# 1) The true proportion of water on earth is p.
# 2) Given the real earth, a single toss has probability p of producing W
#    and 1-p of producing L, assuming no bias in measurement.
# 3) Samples are assumed to be independent (no autocorrelation)


## Grid approximation to determine p

# define grid
p_grid <- seq(0, 1, length.out = grid_length)

# define prior
prior <- rep(1,grid_length) # uniform distribution

# compute likelihood at each value in grid
likelihood <- dbinom(W, n, prob=p_grid)

# product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize posterior
posterior <- unstd.posterior / sum(unstd.posterior)

par(mfrow=c(1,4)) 
plot(prior)
plot(likelihood)
plot(unstd.posterior) # this is the same b/c prior = 1
plot(posterior) # standardized but still basically the same; technically now 
                # probabilities
dev.off()

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "8 W out of 15 tosses" )



# Problem 2 ---------------------------------------------------------------

# Start over in 1, but now use a prior that is zero below p = 0.5 and a 
# constant above p = 0.5. This corresponds to prior information that a majority
# of the Earth’s surface is water. What difference does the better prior make?
# If it helps, compare posterior distributions (using both priors) to the true
# value p = 0.7.


W <- 8
n <- 15
grid_length <- 100

## Grid approximation to determine p

# define grid
p_grid <- seq(0, 1, length.out = grid_length)

# define prior - accounting for the fact that we know at least half is water
prior <- ifelse(p_grid > 0.5, 1, 0) # uniform distribution

# compute likelihood at each value in grid
likelihood <- dbinom(W, n, prob=p_grid)

# product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize posterior
posterior <- unstd.posterior / sum(unstd.posterior)

par(mfrow=c(1,4)) 
plot(prior)
plot(likelihood)
plot(unstd.posterior) # this is the same b/c prior = 1
plot(posterior) # standardized but still basically the same; technically now 
# probabilities
dev.off()

plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )
mtext( "8 W out of 15 tosses" )



# Problem 3 ---------------------------------------------------------------

# This problem is more open-ended than the others. Feel free to collabo-
# rate on the solution. Suppose you want to estimate the Earth’s proportion of
# water very precisely. Specifically, you want the 99% percentile interval of the
# posterior distribution of p to be only 0.05 wide. This means the distance 
# between the upper and lower bound of the interval should be 0.05. How many
# times will you have to toss the globe to do this? I won’t require a precise
# answer. I’m honestly more interested in your approach.


get_interval_width <- function(n) { # defining interval width as a function of sample intensity
  
  s = sample(c(0, 1), size = n, replace = T, prob = c(0.3, 0.7)) # count of 'W'
  W <- sum(s)
  
  grid_length <- 100
  
  # define grid
  p_grid <- seq(0, 1, length.out = grid_length)
  prior <- ifelse(p_grid > 0.5, 1, 0) # uniform distribution
  likelihood <- dbinom(W, n, prob=p_grid)
  unstd.posterior <- likelihood * prior
  posterior <- unstd.posterior / sum(unstd.posterior)
  samples <- sample(p_grid, size=1e4, replace=T, prob=posterior)
  interval <- PI(samples, prob=0.99)
  p_lower <- interval[[1]]
  p_upper <- interval[[2]]
  return(p_upper - p_lower) # interval width
  }

widths <- seq(10, 3000, 10) 
out <- lapply(widths, get_interval_width)
min(widths[out <= 0.05])

# It seems that just over 2000 samples (approx 2090) are required to get 
# a 99% interval of less than 0.05.

