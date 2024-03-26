#### Preamble ####
# Purpose: Simulates data through 10 tests
# Author: Hailey Jang
# Date: 26 March 2024
# Contact: Hailey.Jang@utoronto.ca 
# License: MIT

#### Workspace setup ####
library(ggplot2)

#### Simulate data ####
set.seed(123) 

# Simulating data
years <- 2000:2020
hospitals <- paste("Hospital", LETTERS[1:5])
cancer_types <- c("Lung Cancer", "Breast Cancer", "Colorectal Cancer", "Prostate Cancer")

data <- expand.grid(Year = years, Hospital = hospitals, CancerType = cancer_types)
data$Deaths <- sample(50:200, size = nrow(data), replace = TRUE) # Random deaths

head(data)

#### Summary data ####

summary(data$Deaths)
table(data$Hospital)

#### Visualize data ####

ggplot(data, aes(x = Year, y = Deaths, color = Hospital)) +
  geom_line() +
  facet_wrap(~CancerType, scales = "free_y") +
  theme_minimal() +
  labs(title = "Cancer-related Deaths by Hospital and Cancer Type", y = "Number of Deaths", x = "Year")

#### Test data ####
# Test1
anova_model <- aov(Deaths ~ Hospital, data = data)
summary(anova_model)

# Test2
lm_model <- lm(Deaths ~ Year, data = data)
summary(lm_model)

# Test3
table_cancer_hospital <- table(data$Hospital, data$CancerType)
chisq.test(table_cancer_hospital)

# Test4
t.test(data$Deaths[data$Hospital == "Hospital A" & data$CancerType == "Lung Cancer"],
       data$Deaths[data$Hospital == "Hospital B" & data$CancerType == "Lung Cancer"])


# Test5
kruskal.test(Deaths ~ CancerType, data = data)


# Test6
cor.test(data$Year, data$Deaths)

# Test7
ts_data <- ts(data$Deaths[data$Hospital == "Hospital A"], start = 2000, frequency = 1)
autoplot(ts_data)

# Test8
multivar_model <- lm(Deaths ~ Year + CancerType, data = data)
summary(multivar_model)

# Test9
data$DeathHighLow <- as.factor(ifelse(data$Deaths > median(data$Deaths), "High", "Low"))
logistic_model <- glm(DeathHighLow ~ Year + Hospital, data = data, family = "binomial")
summary(logistic_model)

# Test10
tukey_hsd_result <- TukeyHSD(anova_model)
print(tukey_hsd_result)


