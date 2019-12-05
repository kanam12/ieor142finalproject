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
hourly <- read.csv("merged_by_year_hourly.csv")
str(hourly)

# Use 2013 as testing data
train_hourly <- hourly %>% filter(Year < 2013)
test_hourly <- hourly %>% filter(Year >= 2013)
train_hourly_

set.seed(377)

# Plot initial data -- ggplot knows how to plot dates!
melted = reshape2::melt(hourly %>% select(c(Year, Total..16.years.and.older, White, Black.or.African.American, Asian, Hispanic.or.Latino.ethnicity)), id.var='Year')
ggplot(melted, aes(x=Year, y=value, color=variable)) + geom_point() + scale_y_continuous(breaks = round(seq(min(melted$value), max(melted$value), by = 2),1))

####BUILDING MODELS FOR INDIVIDUAL COLUMNS: FIRST COL IS WHITE

# Linear trend model training data -- Make a new column for the time period
# number (1, 2, ...). The dplyr syntax is a little tricky here -- n() is the
# number of rows in salesTrain, and seq_len(n()) returns the vector 1, 2, ...,
# n(). The end result is that we added a new variable called TimePeriod that
# takes values 1, 2, ..., n().
trainLM_hourly <- train_hourly %>% mutate(TimePeriod = seq_len(n()))
# Build and plot linear trend model
modLM <- lm(White~TimePeriod, data=trainLM_hourly)
ggplot(trainLM_hourly, aes(x=Year, y=White)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=predict(modLM)), col="red", lwd=1.5)

###### Random Walk model training data 
trainRW_hourly <- train_hourly %>% mutate(LastYear = c(NA, head(White, -1)))
head(trainRW_hourly)
#random walk aka moving average

# Plot with an additional red line for our predictions as before
ggplot(trainRW_hourly, aes(x=Year, y=White)) +
  geom_line() +
  geom_point() +
  geom_line(aes(y=LastYear), col="red")

# Proportion of percentages for which difference is more than 1.
table(abs(trainRW_hourly$White-trainRW_hourly$LastYear) >= 1)
# Compute training set R^2
# Note that we need to remove the first observation since there is no
# prediction. This is achieved using tail(.., -1) which says to take all but
# the first observation.
BaselineR2(tail(trainRW_hourly$LastYear, -1), 
           tail(trainRW_hourly$White, -1),
           mean(trainRW_hourly$White))

###### AR model

# We need to add sales yesterday and sales two days ago for the two term AR model
# head(.., -2) says take all but the last two 
salesTrainAR <- salesTrain %>%
  mutate(SalesYesterday=c(NA, head(Sales, -1))) %>%
  mutate(SalesTwoDaysAgo = c(NA, NA, head(Sales, -2)))







