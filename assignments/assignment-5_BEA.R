#
# Title: Assignment 5
# Created: March 3rd, 2022
# Last Updated: March 3rd, 2022
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

# Consider the data (Wines2012) data table. These data are expert ratings of 20 differeing
# French and American wines by 9 different French and American judges. Your goal is to model score,
# the subjective rating assigned by each judge to each wine. I recommend standardizing it.

# Load the Wines data
data("Wines2012")

# Standardize wine score
Wines2012$score.std <- (Wines2012$score - mean(Wines2012$score)) / sd(Wines2012$score)

##############
# Question 1 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# In this first problem, consider only variation among judges and wines.
# Construct index variables of judge and wine and then use these index variables to construct a linear regression model. Justify your priors.
# You should end up with 9 judge parameters and 20 wine parameters.
# Use ulam instead of quap to build this model, and be sure to check the chains for convergence.
# If you’d rather build the model directly in Stan or PyMC3, go ahead.
# I just want you to use Hamiltonian Monte Carlo instead of quadratic approximation .
# How do you interpret the variation among individual judges and individual wines? Do you notice any patterns, just by plotting the differences?
# Which judges gave the highest/lowest ratings? Which wines were rated worst/ best on average?

# For Stan models it is recommended to create data subsets

data_slim <- list(

  score = Wines2012$score.std,
  judge = Wines2012$judge,
  wine = Wines2012$wine

)

# Create model using ulam (MCMC approach)

bayes.model <- ulam(

  alist(
    score ~ dnorm(mu, sigma),
    mu <- a[judge] + b[wine],
    a[judge] ~ dnorm(0, 0.5),
    b[wine] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),

  data = data_slim,
  chains = 4,
  cores = 4,
  iter = 1000

)

# Diagnostics
precis(bayes.model, 2)
traceplot(bayes.model)
plot(precis(bayes.model, 2))

##############
# Question 2 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now consider three features of the wines and judges:
# 1) flight: Whether the wine is red or white.
# 2) wine.amer: Indicator variable for American wines.
# 3) judge.amer: Indicator variable for American judges.

# Use indicator or index variables to model the influence of these features on the scores.
# Omit the individual judge and wine index variables from Problem 1.
# Do not include interaction effects yet. Again use ulam, justify your priors, and be sure to check the chains.
# What do you conclude about the differences among the wines and judges? Try to relate the results to the inferences in Problem 1.

# For Stan models it is recommended to create data subsets

data_slim <- list(

  score = Wines2012$score.std,
  judge = Wines2012$judge.amer,
  wine = Wines2012$wine.amer,
  flight = ifelse(Wines2012$flight == "red", 1L, 0L)

)

# Create model using ulam (MCMC approach)

bayes.model <- ulam(

  alist(
    score ~ dnorm(mu, sigma),
    mu <- a + bJ*judge + bW*wine + bF*flight,
    a ~ dnorm(0, 0.2),
    bJ ~ dnorm(0, 0.5),
    bW ~ dnorm(0, 0.5),
    bF ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),

  data = data_slim,
  chains = 4,
  cores = 4,
  iter = 1000

)

# Diagnostics
precis(bayes.model, 2)
traceplot(bayes.model)
plot(precis(bayes.model, 2))

# Index version
data_slim <- list(

  score = Wines2012$score.std,
  judge = Wines2012$judge.amer + 1,
  wine = Wines2012$wine.amer + 1,
  flight = ifelse(Wines2012$flight == "red", 1L, 2L)

)

# Create model using ulam (MCMC approach)

bayes.model <- ulam(

  alist(
    score ~ dnorm(mu, sigma),
    mu <- bJ[judge] + bW[wine] + bF[flight],
    bJ[judge] ~ dnorm(0, 0.5),
    bW[wine] ~ dnorm(0, 0.5),
    bF[flight] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)

  ),

  data = data_slim,
  chains = 4,
  cores = 4,
  iter = 1000

)

# Diagnostics
precis(bayes.model, 2)
traceplot(bayes.model)
plot(precis(bayes.model, 2))


##############
# Question 3 #
##############~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now consider two-way interactions among the three features. You should end up with three different interaction terms in your model.
# These will be easier to build, if you use indicator variables. Again use ulam, justify your priors, and be sure to check the chains.
# Explain what each interaction means. Be sure to interpret the model’s predictions on the outcome scale (mu, the expected score),
# not on the scale of individual parameters. You can use link to help with this, or just use your knowledge of the linear model instead.

# What do you conclude about the features and the scores? Can you relate the results of your model(s) to the individual judge and wine inferences from Problem 1?

# For Stan models it is recommended to create data subsets

data_slim <- list(

  score = Wines2012$score.std,
  judge = Wines2012$judge.amer,
  wine = Wines2012$wine.amer,
  flight = ifelse(Wines2012$flight == "red", 1L, 0L)

)

# Create model using ulam (MCMC approach)

bayes.model <- ulam(

  alist(
    score ~ dnorm(mu, sigma),
    mu <- a + bJ*judge + bW*wine + bF*flight + bJW*judge*wine + bJF*judge*flight + bWF*flight*wine,
    a ~ dnorm(0, 0.2),
    c(bJ, bW, bF) ~ dnorm(0, 0.5),
    c(bJW, bJF, bWF) ~ dnorm(0, 0.2),
    sigma ~ dexp(1)

  ),

  data = data_slim,
  chains = 4,
  cores = 4,
  iter = 1000

)

# Diagnostics
precis(bayes.model, 2)
traceplot(bayes.model)
plot(precis(bayes.model, 2))
