#### Preamble ####
# Purpose: Builds graph and model
# Author: Hailey Jang
# Date: 27 March 2024
# Contact: hailey.jang@utoronto.ca
# License: MIT


#### Workspace setup ####
library(ggplot2)
library(rstanarm)

#### Build a graph ####
data$AgeGroup <- factor(data$AgeGroup, levels = c('18-24', '25-34', '35-44', '45-54', '55-64', '65+'))
data$Support <- factor(data$Support, labels = c("No", "Yes"))

# Plotting
ggplot(data, aes(x = AgeGroup, fill = Support)) +
  geom_bar(position = "fill") + # Stacked bar chart to show proportions
  labs(x = "Age Group", y = "Proportion of Support", title = "Proportion of Political Party Support by Age Group") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

#### Build a model ####
data$Gender <- as.factor(data$Gender)
data$IncomeGroup <- as.factor(data$IncomeGroup)
data$HighestEducation <- as.factor(data$HighestEducation)

stan_model <- stan_glm(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, 
                       data = data, 
                       family = binomial(link = "logit"),
                       chains = 2, iter = 2000)

# Summary of the model
print(summary(stan_model))


plot(stan_model)

