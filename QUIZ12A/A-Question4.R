#### Preamble ####
# Purpose: Build the graph and model
# Author: Hailey Jang
# Date: 26 March 2024
# Contact: Hailey.Jang@utoronto.ca 
# License: MIT

#### Workspace setup ####
library(ggplot2)

#### Create a Graph ####
ggplot(data, aes(x = Year, y = Deaths, color = Hospital)) +
  geom_line() +
  facet_wrap(~CancerType, scales = "free_y") + # Separate plots for each cancer type
  theme_minimal() +
  labs(title = "Cancer-related Deaths in Sydney's Hospitals",
       y = "Number of Deaths", x = "Year")

#### Build a Model ####
if (!requireNamespace("rstanarm", quietly = TRUE)) {
  install.packages("rstanarm")
}

library(rstanarm)

# Fit a Bayesian linear model
stan_model <- stan_glm(Deaths ~ Year, data = data, family = gaussian(), chains = 4, iter = 2000)

print(summary(stan_model))

plot(stan_model)

