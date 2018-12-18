# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
data <- ecoli3.csv(file.choose(), header = T)
str(data)
xtabs(~mcg+chg, data = data)
data$chg <- as.factor(data$chg)
data$mcg <- as.factor(data$mcg)

# Visualization
pairs.panels(data[-1])
data %>%
  ggplot(aes(x=mcg, y=lip, fill = mcg)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>% ggplot(aes(x=lip, fill = mcg)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(mcg ~ ., data = train, usekernel = T)
model

train %>%
  filter(mcg == "1") %>%
  summarise(mean(gvh), sd(gvh))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$mcg))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$mcg))
1 - sum(diag(tab2)) / sum(tab2)
