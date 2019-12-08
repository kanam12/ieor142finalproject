library(dplyr)
library(ggplot2)
library(lubridate)

# Out of sample R^2
OSR2 <- function(predictions, train, test) {
  SSE <- sum((test - predictions)^2)
  SST <- sum((test - mean(train))^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

# R^2 with a particular baseline
BaselineR2 <- function(predictions, truth, baseline) {
  SSE <- sum((truth - predictions)^2)
  SST <- sum((truth - baseline)^2)
  r2 <- 1 - SSE/SST
  return(r2)
}

# Load data and check it out
race = read.csv("salary_race.csv")
str(race)

# Use 2013 as testing data
train <- race %>% filter(Year < 2013)
test <- race %>% filter(Year >= 2013)

set.seed(377)

# Plot initial data -- ggplot knows how to plot dates!
ggplot(race, aes(x = Year, y=Percentage)) + geom_point() + geom_line()

# Linear trend model training data -- Make a new column for the time period
# number (1, 2, ...). The dplyr syntax is a little tricky here -- n() is the
# number of rows in salesTrain, and seq_len(n()) returns the vector 1, 2, ...,
# n(). The end result is that we added a new variable called TimePeriod that
# takes values 1, 2, ..., n().
#old
# trainLM_hourly <- train_hourly %>% mutate(TimePeriod = seq_len(n()))
trainLM <- train %>% mutate(TimePeriod = seq_len(n()))
# Build and plot linear trend model
modLM <- lm(Percentage~TimePeriod + White + Asian + Black.or.African.American + Hispanic.or.Latino.ethnicity, data = trainLM)
ggplot(trainLM, aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(modLM)), col="red", lwd=1.5)

###### Random Walk model training data 
trainRW <- train %>% mutate(LastYear = c(rep(NA,4), head(Percentage, -4)))
head(trainRW)
#random walk aka moving average

# Plot with an additional red line for our predictions as before
ggplot(trainRW, aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=LastYear), col="red")

# Proportion of percentages for which difference is more than 1.
table(abs(trainRW$Percentage-trainRW$LastYear) >= 1)
# Compute training set R^2
# Note that we need to remove the first observation since there is no
# prediction. This is achieved using tail(.., -1) which says to take all but
# the first observation.
BaselineR2(tail(trainRW$LastYear, -4), 
           tail(trainRW$Percentage, -4),
           mean(trainRW$Percentage))

###### AR model

# We need to add sales yesterday and sales two days ago for the two term AR model
# head(.., -2) says take all but the last two 
trainAR <- train %>%
  mutate(LastYear=c(rep(NA, 4), head(White, -4))) %>%
  mutate(TwoYearsAgo = c(rep(NA, 8), head(White, -8)))
# Do the regression with one lag term
mod2a <- lm(Percentage~LastYear + White + Asian + Black.or.African.American + Hispanic.or.Latino.ethnicity, data=trainAR)
summary(mod2a)

# 2-term autoregressive model 
mod2b <- lm(Percentage~LastYear + TwoYearsAgo + White + Asian + Black.or.African.American + Hispanic.or.Latino.ethnicity, data=trainAR)
summary(mod2b)
# Plot with an additional red line for our predictions as before
ggplot(trainAR, aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod2b, newdata=trainAR)), col="red")


## Trying Random Forest
library(randomForest)
set.seed(349)

# Plug in all of the variables that we've created
mod.rf <- randomForest(Percentage ~ LastYear + TwoYearsAgo + Year + White + Asian + Black.or.African.American + Hispanic.or.Latino.ethnicity, data = tail(trainAR, -8))
ggplot(trainAR, aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod.rf, newdata=trainAR)), col="green")

# Both on the same plot:
ggplot(trainAR, aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(mod2b, newdata=trainAR)), col="red") +
  geom_line(aes(y=predict(mod.rf, newdata=trainAR)), col="green")

# Create Test Set
test_final <- test %>%
  mutate(LastYear=c(rep(NA,4), head(Percentage, -4))) %>%
  mutate(TwoYearsAgo = c(rep(NA,8), head(Percentage, -8)))

# Test set prediction and OSR^2
# Test-set prediction
#DEBUG THIS - WHY IS THIS NEGATIVE? AND OSR^2 IS BROKEN
pred.test <- tail(predict(mod2b, newdata = test_final), -4)
OSR2(pred.test, trainAR$Percentage, tail(test_final$Percentage, -4))
# SSE <- sum((tail(test_final$Percentage, -4) - pred.test)^2)
# SST <- sum((tail(test_final$Percentage, -4) - mean(trainAR$Percentage))^2)
# r2 <- 1 - SSE/SST

pred.test.rf <- tail(predict(mod.rf, newdata = test_final), -8)
OSR2(pred.test.rf, trainAR$Percentage, tail(test_final$Percentage, -8))

# Test set plots
ggplot(tail(test_final, -4), aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=pred.test), col="red")

ggplot(test_final, aes(x=Year, y=Percentage)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=pred.test), col="red") +
  geom_line(aes(y=pred.test.rf), col="green")





