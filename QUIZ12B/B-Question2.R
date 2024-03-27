#### Preamble ####
# Purpose: Simulates the data
# Author: Hailey Jang
# Date: 27 March 2024
# Contact: hailey.jang@utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
install.packages("nnet")
library(nnet)

#### Simulate data ####
set.seed(123) 

n <- 1000

# Simulate data
data <- data.frame(
  Support = sample(c(0, 1), n, replace = TRUE, prob = c(0.5, 0.5)),
  AgeGroup = sample(c('18-24', '25-34', '35-44', '45-54', '55-64', '65+'), n, replace = TRUE),
  Gender = sample(c('Male', 'Female', 'Other'), n, replace = TRUE, prob = c(0.48, 0.48, 0.04)),
  IncomeGroup = sample(c('Low', 'Middle', 'High'), n, replace = TRUE),
  HighestEducation = sample(c('High School', 'Bachelor', 'Master', 'PhD'), n, replace = TRUE)
)

#### Test data ####
# test 1
chisq.test(table(data$Support, data$AgeGroup))

# test 2
chisq.test(table(data$Support, data$Gender))

# test 3
chisq.test(table(data$Support, data$IncomeGroup))

# test 4
chisq.test(table(data$Support, data$HighestEducation))

# test 5
chisq.test(table(data$Gender, data$Support))

# test 6
model <- glm(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, data=data, family="binomial")
summary(model)

# test 7
model_interaction <- glm(Support ~ AgeGroup * Gender + IncomeGroup * HighestEducation, data=data, family="binomial")
summary(model_interaction)

# test 8
predictions <- ifelse(predict(model, type="response") > 0.5, 1, 0)
mean(predictions == data$Support)

# test 9

multinom_model <- multinom(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, data=data)
summary(multinom_model)

# test 10
# Fit the logistic regression model
model <- glm(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, data=data, family=binomial())

summary(model)

exp(coef(model))


