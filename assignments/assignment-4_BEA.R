#
# Title: Assignment 4
# Created: December 22nd, 2021
# Last Updated: January 28th, 2021
# Author: Brandon Allen
# Objectives:
# Keywords: Question 1, Question 2, Question 3
# Notes:
#

# Clear memory
rm(list=ls())
gc()

# Load library
library(rethinking)

##############
# Question 1 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Consider three fictional Polynesian islands.
# On each there is a Royal Ornithologist charged by the king with surveying the birb population.
# They have each found the following proportions of 5 important birb species:

bird.species <- data.frame(Birb_A = c(0.2, 0.8, 0.05),
                           Birb_B = c(0.2, 0.1, 0.15),
                           Birb_C = c(0.2, 0.05, 0.7),
                           Birb_D = c(0.2, 0.025, 0.05),
                           Birb_E = c(0.2, 0.025, 0.05),
                           row.names = c("Island1", "Island2", "Island3"))

# Notice that each row sums to 1, all the birbs. This problem has two parts.
# It is not computationally complicated. But it is conceptually tricky.
# First, compute the entropy of each island’s birb distribution. Interpret these entropy values.
# Second, use each island’s birb distribution to predict the other two.
# This means to compute the K-L Divergence of each island from the others, treating
# each island as if it were a statistical model of the other islands.
# You should end up with 6 different K-L Divergence values.
# Which island predicts the others best? Why?

# Define entropy function and Dkl functions
entropy <- function(p) {

  sum(p*log(p))

}

dkl <- function(p, q) {

  sum(p*(log(p) - log(q)))

}

# Island 1
entropy(bird.species["Island1",])

# Island 2
entropy(bird.species["Island2",])

# Island 3
entropy(bird.species["Island2",])

# Pairwise DKL

dkl(bird.species["Island1",], bird.species["Island2",])
dkl(bird.species["Island1",], bird.species["Island3",])

dkl(bird.species["Island2",], bird.species["Island1",])
dkl(bird.species["Island2",], bird.species["Island3",])

dkl(bird.species["Island3",], bird.species["Island1",])
dkl(bird.species["Island3",], bird.species["Island2",])

# Island 1 has the lowest divergence


##############
# Question 2 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Recall the marriage, age, and happiness collider bias example from Chapter 6.
# Run models m6.9 and m6.10 again. Compare these two models using WAIC (or LOO, they will produce identical results).
# Which model is expected to make better predictions? Which model provides the correct causal inference about the
# influence of age on happiness? Can you explain why the answers to these two questions disagree?

# Copy example m6.9 and m6.10

# Load data and clean
d <- sim_happiness( seed=1977 , N_years=1000 )
d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
d2$mid <- d2$married + 1

m6.9 <- quap(

  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a[mid] + bA*A,
    a[mid] ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 2 ) ,
    sigma ~ dexp(1)
  ) , data=d2 )

m6.10 <- quap(

  alist(
    happiness ~ dnorm( mu , sigma ),
    mu <- a + bA*A,
    a ~ dnorm( 0 , 1 ),
    bA ~ dnorm( 0 , 2 ),
    sigma ~ dexp(1)
  ) , data=d2 )

# Use the compare function to compare the results
compare(m6.9, m6.10)

# Model 6.9 has a better out of sample predictive ability.
# Based on the DAG provided, we should not condition on Marriage as
# it is a collider.

##############
# Question 3 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Reconsider the urban fox analysis from last week’s homework.
# Use WAIC or LOO based model comparison on five different models, each using weight
# as the outcome, and containing these sets of predictor variables:
# (1) avgfood + groupsize + area
# (2) avgfood + groupsize
# (3) groupsize + area
# (4) avgfood
# (5) area
# Can you explain the relative differences in WAIC scores, using the fox DAG from last week’s homework?
# Be sure to pay attention to the standard error of the score differences (dSE).

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

# Build our 5 models
# Since we want to total effect, we don't need to include the other relationships

m.1 <- quap(
  alist(

    # area -> avgfood -> weight <- groupsize

    weight ~ dnorm(mu, sigma),
    mu ~ a + b.avgfood * avgfood + b.groupsize * groupsize + b.area * area,
    a ~ dnorm(0, 0.2),
    b.area ~ dnorm(0, 0.5),
    b.avgfood ~ dnorm(0, 0.5),
    b.groupsize ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),
  data = data.in
)

m.2 <- quap(
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

m.3 <- quap(
  alist(

    # Area -> weight <- groupsize

    weight ~ dnorm(mu, sigma),
    mu ~ a + b.area * area + b.groupsize * groupsize,
    a ~ dnorm(0, 0.2),
    b.area ~ dnorm(0, 0.5),
    b.groupsize ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),
  data = data.in
)

m.4 <- quap(
  alist(

    # Average food -> Weight

    weight ~ dnorm(mu, sigma),
    mu ~ a + b.avgfood * avgfood,
    a ~ dnorm(0, 0.2),
    b.avgfood ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),
  data = data.in
)

m.5 <- quap(
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

# Compare the models
compare(m.1,m.2,m.3,m.4,m.5)

# Models
# (1) avgfood + groupsize + area
# (2) avgfood + groupsize
# (3) groupsize + area
# (4) avgfood
# (5) area

# Model m.1 improperly conditions on variables but has lowest WAIC
# Model m.2 Similar to m.3, but properly conditioned
# Model m.3 has lowest difference
# Model m.4
# Model m.5 is correct, but has poor prediction
