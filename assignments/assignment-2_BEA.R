#
# Title: Assignment 1
# Created: December 15th, 2021
# Last Updated: December 16th, 2021
# Author: Brandon Allen
# Objectives: Create examples illustrating how dispersal distances will impact the indicator
# Keywords: Question 1, Question 2, Question 3
# Notes:
#

##############
# Question 1 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The weights listed below were recorded in the !Kung census, but heights were not recorded for these individuals.
# Provide predicted heights and 89% compatibility intervals for each of these individuals. That is, fill in the table
# below, using model-based predictions

# Clear memory
rm(list=ls())
gc()

# Load library
library(rethinking)

# Load the Dobe !Kung data
data(Howell1)

# Assuming we are only using adults since question 2 specifies all individuals
data.in <- Howell1[Howell1$age >= 18, ]

# Define the unknown individuals
unknown.heights <- data.frame(individual = c(1:5),
                              weight = c(45, 40, 65, 31, 53),
                              exp_height = NA,
                              lower_ci = NA,
                              upper_ci = NA)

# Define average weight
xbar <- mean(data.in$weight)

# Create the model based on the quap framework
bayes.model <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ a + b*(weight - xbar),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = data.in
)

# Predict individuals using sim
height.sim <- sim(bayes.model, data = unknown.heights)

# Calculate mean and ci
mu <- apply(X = height.sim, MARGIN = 2, FUN = mean)
ci.intervals <- apply(X = height.sim, MARGIN = 2, FUN = function(x) {PI(x, prob = 0.89)})

# Add values to unknown.heights
unknown.heights$exp_height <- mu
unknown.heights$lower_ci <- ci.intervals[1, ]
unknown.heights$upper_ci <- ci.intervals[2, ]

##############
# Question 2 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Model the relationship between height (cm) and the natural logarithm of weight (log-kg): log(weight).
# Use the entire Howell1 data frame, all 544 rows, adults and non-adults. Use any model type from Chapter 4 that you
# think useful: an ordinary linear regression, a polynomial or a spline. Plot the posterior predictions against the raw data.

#######################
# Polynomial approach #
#######################

# Clear memory
rm(list=ls())
gc()

# Load library
library(rethinking)

# Load the Dobe !Kung data
data(Howell1)

# Use the complete dataset
data.in <- Howell1

# Standardize weights (log)
data.in$weight_stand <- log(data.in$weight)
data.in$weight_stand2 <- data.in$weight_stand^2

# Create the model based on the quap framework
bayes.model <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ a + b1*weight_stand + b2*weight_stand2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = data.in
)

# Check results
precis(bayes.model)

# Create weight sequence on standardized scale
weight.seq <- seq(from = min(data.in$weight_stand),
                  to = max(data.in$weight_stand),
                  length.out = 50)

# Create data frame of new weights
pred.data <- list(weight_stand = weight.seq,
                  weight_stand2 = weight.seq^2)

# Extract samples
mu <- link(bayes.model, data = pred.data)

# Calculate mean and intervals
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

# Simulate the heights and calculate intervales
height.sim <- sim(bayes.model, data = pred.data)
height.PI <- apply(height.sim ,2 , PI, prob=0.89)

# Visualize
plot(height ~ weight_stand , data.in, col=  col.alpha(rangi2,0.5))
lines(weight.seq, mu.mean)
shade(height.PI, weight.seq)

###################
# Spline approach # Shade function only seems to work if the x axis is ordered. Otherwise everything turns into jumble
###################

# Clear memory
rm(list=ls())
gc()

# Load library
library(splines)
library(rethinking)

# Load the Dobe !Kung data
data(Howell1)

# Use the complete dataset
data.in <- Howell1

# Standardize weights (log)
data.in$weight_stand <- log(data.in$weight)

# Sort data frame by weight
data.in <- data.in[order(data.in$weight_stand), ]

# Create knots for the spline
knot.list <- quantile(data.in$weight_stand, probs=seq(0, 1,length.out = 5) )
B <- bs(data.in$weight_stand,
        knots = knot.list[-c(1,5)],
        degree = 3,
        intercept = TRUE)

# Check knots are as expect
plot(NULL,
     xlim = range(data.in$weight_stand),
     ylim = c(0,1),
     xlab = "weight_stand",
     ylab = "basis")

for (i in 1:ncol(B)) {

  lines(data.in$weight_stand, B[,i])

}

# Create the model based on the quap framework
bayes.model <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + B %*% w,
    a ~ dnorm(178, 20),
    w ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = list(height = data.in$height, B = B),
  start = list(w = rep(0, ncol(B)))
)

# Check results
precis(bayes.model, depth = 2)

# Check the knots
posterior <- extract.samples(bayes.model)
w <- apply(posterior$w, 2, mean)

plot(NULL,
     xlim=range(data.in$weight_stand),
     ylim=c(min(w), max(w)),
     xlab="weight_stand",
     ylab="basis * weight")

for (i in 1:ncol(B)) {

  lines(data.in$weight_stand, w[i]*B[,i])

}

# Calculate mean and intervals
mu <- link(bayes.model)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

# Visualize
plot(height ~ weight_stand , data.in, col=  col.alpha(rangi2,0.5))
shade(mu.PI, data.in$weight_stand, col=col.alpha("black",0.5))

##############
# Question 3 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot the prior predictive distribution for the polynomial regression model in Chapter 4.
# You can modify the the code that plots the linear regression prior predictive distribution.
# 20 or 30 parabolas from the prior should suffice to show where the prior probability resides.
# Can you modify the prior distributions of alpha, beta1, and beta2 so that the prior predictions stay within the
# biologically reasonable outcome space? That is to say: Do not try to fit the data by hand.
# But do try to keep the curves consistent with what you know about height and weight, before seeing these exact data.

#######################
# Polynomial approach #
#######################

# Clear memory
rm(list=ls())
gc()

# Load library
library(rethinking)

# Load the Dobe !Kung data
data(Howell1)

# Use the complete dataset
data.in <- Howell1

# Standardize weights
data.in$weight_stand <- (data.in$weight - mean(data.in$weight)) / sd(data.in$weight)
data.in$weight_stand2 <- data.in$weight_stand^2

# Create the model based on the quap framework
bayes.model <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ a + b1*weight_stand + b2*weight_stand2,
    a ~ dnorm(178, 20),
    b1 ~ dnorm(0, 100),
    b2 ~ dnorm(0, 100),
    sigma ~ dunif(0, 50)
  ),
  data = data.in
)

# Check results
precis(bayes.model)

# Create weight sequence on standardized scale
weight.seq <- seq(from = min(data.in$weight_stand),
                  to = max(data.in$weight_stand),
                  length.out = 50)

# Create data frame of new weights
pred.data <- list(weight_stand = weight.seq,
                  weight_stand2 = weight.seq^2)

# Calculate mean and intervals
mu <- link(bayes.model, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob = 0.89)

# Visualize
plot(height ~ weight_stand , data.in, col=  col.alpha(rangi2,0.5))
shade(mu.PI, pred.data$weight_stand, col=col.alpha("black",0.5))

# Extract prior
prior.in <- extract.prior(bayes.model)
mu <- link(bayes.model,
           post = prior.in,
           data = pred.data)

plot(NULL,
     xlim=range(data.in$weight_stand),
     ylim=c(50,300),
     xlab="weight_stand",
     ylab="height")

for (i in 1:50) {

  lines(pred.data$weight_stand, mu[i,], col=col.alpha("black",0.5) )

}

# Moral of the story, the polynomials can be bonkers and still
# result in a model that fits. However, this should be a warning
# as it this could lead to incorret results in more complex models!


