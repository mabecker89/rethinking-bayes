#-----------------------------------------------------------------------------------------------------------------------

# Assignment 2 - Statistical Rethinking w/ Chris & Brandon
# January 2022

library(rethinking)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)
library(stringr)

#-----------------------------------------------------------------------------------------------------------------------

# Question 1. Five new individuals - provide predicted heights and 89% compatibility intervals
# (based on the !Kung census data)

# Load data
data("Howell1")
d <- Howell1
d1 <- d[d$age >= 18,]
# For centering, calculate mean
mean_weight <- mean(d1$weight)
# Construct model
model <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <-  a + b * (weight - mean_weight),
    # Using my own height as a weak prior
    a ~ dnorm(182, 20),
    b ~ dlnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d1
)

# Predict on new individuals
individ <- data.frame(weight = c(45, 40, 65, 31, 53))
predictions <- sim(model, individ, n = 1e4)
mean_predictions <- apply(predictions, 2, mean)
hpdi <- apply(predictions, 2, HPDI, prob = 0.89)

# Join together
results <- data.frame(individual = 1:5) %>%
  bind_cols(individ) %>%
  mutate(mean_height = mean_predictions,
         lower_89 = hpdi[1,],
         upper_89 = hpdi[2,])

#-----------------------------------------------------------------------------------------------------------------------

# Question 2. This time use the log(weight) in the model, with the entire dataset.
# Plot posterior predictions against the raw data.

# Calculate log(weight) variable ahead of time
d$log_weight <- log(d$weight)
# Again calculate the mean for centering purposes
mean_log_weight <- mean(d$log_weight)
# Construct model
model2 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <-  a + b * (log_weight - mean_log_weight),
    # Using my own height as a weak prior
    a ~ dnorm(182, 20),
    b ~ dlnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

# Data to predict on for interval
weight_seq <- log(1:60)
mu <- sim(model2, data = list(log_weight = weight_seq))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI, 0.99)
line_data <- data.frame(x = exp(weight_seq), y = mu_mean)
shade_data <- data.frame(mu_pi) %>%
  rownames_to_column() %>%
  # Kinda gross. There's a better way to do this.
  pivot_longer(cols = X1:X60, names_to = "name", values_to = "value") %>%
  mutate(conf = ifelse(rowname == "1%", "lower", "upper"),
         weight = as.numeric(str_remove(name, "^X"))) %>%
  select(weight, conf, value) %>%
  arrange(weight) %>%
  pivot_wider(id_cols = weight, names_from = "conf", values_from = "value") %>%
  bind_cols(line_data) %>%
  rename(height = y)

# Plot
d %>%
  ggplot(aes(x = weight, y = height)) +
  geom_ribbon(data = shade_data, aes(ymin = lower, ymax = upper), fill = "grey83") +
  geom_point(col = "grey50", alpha = 0.3) +
  geom_line(data = line_data, aes(x = x, y = y), color = "steelblue", size = 1) +
  scale_y_continuous(limits = c(50, 180))

#-----------------------------------------------------------------------------------------------------------------------

# Question 3.

# Code for polynomial model from book:
d <- Howell1
# Standardize:
d$weight_stan <- (d$weight - mean(d$weight)) / sd(d$weight)
# Squared term:
d$weight_stan2 <- d$weight_stan^2
# Construct model:
model3 <- quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1 * weight_stan + b2 * weight_stan2,
    a ~ dnorm(178, 20),
    b1 ~ dlnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 50)
  ),
  data = d
)

# Extract priors
prior <- extract.prior(model3)
precis(prior)

# Simulate curves (parabolas)
w_seq <- seq(from = min(d$weight_stan), to = max(d$weight_stan), length.out = 50)
w2_seq <- w_seq^2
mu <- link(model3, post = prior, data = list(weight_stan = w_seq, weight_stan2 = w2_seq))

# Plot
plot(NULL, xlim = range(w_seq), ylim = c(55, 270),
     xlab = "weight (standardized)", ylab = "height")
for (i in 1:50) lines(w_seq, mu[i,], col = col.alpha("black", 0.5))

#-----------------------------------------------------------------------------------------------------------------------

