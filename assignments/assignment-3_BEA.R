#
# Title: Assignment 3
# Created: December 21st, 2021
# Last Updated: December December 21st, 2021
# Author: Brandon Allen
# Objectives:
# Keywords: Question 1, Question 2, Question 3
# Notes:
#

##############
# Question 1 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Use a model to infer the total causal influence of area on weight.
# Would increasing the area available to each fox make it heavier (healthier)? You might want to standardize the variables.
# Regardless, use prior predictive simulation to show that your model’s prior predictions stay within the possible outcome range.

# Clear memory
rm(list=ls())
gc()

# Load library
library(rethinking)

# Load the foxes data. This is used for all three questions
data(foxes)

# Standardize variables (x - mean) / SD
data.in <- foxes

for (col.id in colnames(data.in)[2:5]) {

  data.in[, col.id] <- (data.in[, col.id] - mean(data.in[, col.id])) / sd(data.in[, col.id])

}

# Sort the variable of interest (weight)
data.in <- data.in[order(data.in$weight), ]

# Build our model
# Since we want to total effect, we don't need to include the other relationships

bayes.model <- quap(
  alist(

    # Area -> Weight

    weight ~ dnorm(mu, sigma),
    mu ~ a + b.area * area,
    a ~ dnorm(0, 0.2),
    b.area ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),
  data = data.in
)

# Get model summary
precis(bayes.model)

# Extract the priors
set.seed(123)

prior <- extract.prior(bayes.model)

# Get estiamtes of mu
mu <- link(bayes.model,
           post = prior,
           data = list(area = c(-2,2)))

# Weight
plot( NULL, xlim=c(-2,2) , ylim=c(-2,2))

for (i in 1:50) {

  lines(c(-2,2),
        mu[i,],
        col=col.alpha("black",0.4))

}

##############
# Question 2 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now infer the causal impact of adding food to a territory.
# Would this make foxes heavier? Which covariates do you need to adjust for to estimate the total causal influence of food?

# Build our model
# Since we want to total effect, we don't need to include the other relationships

bayes.model <- quap(
  alist(

    # Area -> Weight

    weight ~ dnorm(mu, sigma),
    mu ~ a + b.avgfood * avgfood,
    a ~ dnorm(0, 0.2),
    b.avgfood ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),
  data = data.in
)

# Get model summary
precis(bayes.model)

# Extract the priors
set.seed(123)

prior <- extract.prior(bayes.model)

# Get estiamtes of mu
mu <- link(bayes.model,
           post = prior,
           data = list(avgfood = c(-2,2)))

# Weight
plot( NULL, xlim=c(-2,2) , ylim=c(-2,2))

for (i in 1:50) {

  lines(c(-2,2),
        mu[i,],
        col=col.alpha("black",0.4))

}

##############
# Question 3 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now infer the causal impact of group size. Which covariates do you need to adjust for?
# Looking at the posterior distribution of the resulting model, what do you think explains these data?
# That is, can you explain the estimates for all three problems? How do they go together?

# Build our model

bayes.model <- quap(
  alist(

    # avgfood -> weight <- groupsize

    weight ~ dnorm(mu, sigma),
    mu ~ a + b.avgfood * avgfood + b.groupsize * groupsize,
    a ~ dnorm(0, 0.2),
    b.avgfood ~ dnorm(0, 0.5),
    b.groupsize ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),
  data = data.in
)

# Get model summary
precis(bayes.model)

# Extract the priors
set.seed(123)

prior <- extract.prior(bayes.model)

# Get estiamtes of mu
mu <- link(bayes.model,
           post = prior,
           data = list(avgfood = c(-2,2),
                       area = c(-2,2),
                       groupsize = c(-2,2)))
