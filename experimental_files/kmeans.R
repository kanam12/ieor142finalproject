library(dplyr)
library(ggplot2)
library(caret)

suicides = read.csv("suicide_rates_no_na.csv")
str(suicides)
summary(suicides)
km_suicides_full <- suicides %>% select(gdp_for_year...., HDI.for.year, gdp_per_capita...., population, suicides.100k.pop, year)
km_suicides_full_no_year <- km_suicides_full %>% select(-year)
km_suicides_us <-  suicides %>% filter(country == "United States") %>% select(gdp_for_year...., HDI.for.year, gdp_per_capita...., population, suicides.100k.pop, year)
km_suicides_us_no_year <- km_suicides_us %>% select(-year)
km_suicides_jp <- suicides %>% filter(country == "Japan") %>% select(gdp_for_year...., HDI.for.year, gdp_per_capita...., population, suicides.100k.pop, year)
km_suicides_jp_no_year <- km_suicides_jp %>% select(-year)

### MODEL 1 PP
pp_full <- preProcess(km_suicides_full, method=c("center", "scale"))
suicides_full.scaled <- predict(pp_full, km_suicides_full)
head(suicides_full.scaled)
summary(suicides_full.scaled)

### MODEL 2 PP
pp_full_no_year <- preProcess(km_suicides_full_no_year, method=c("center", "scale"))
suicides_full_no_year.scaled <- predict(pp_full_no_year, km_suicides_full_no_year)
head(suicides_full_no_year.scaled)
summary(suicides_full_no_year.scaled)

### MODEL 3 PP
pp_us <- preProcess(km_suicides_us, method=c("center", "scale"))
suicides_us.scaled <- predict(pp_us, km_suicides_us)
head(suicides_us.scaled)
summary(suicides_us.scaled)

### MODEL 4 PP
pp_us_no_year <- preProcess(km_suicides_us_no_year, method=c("center", "scale"))
suicides_us_no_year.scaled <- predict(pp_us_no_year, km_suicides_us_no_year)
head(suicides_us_no_year.scaled)
summary(suicides_us_no_year.scaled)

### MODEL 5 PP
pp_jp <- preProcess(km_suicides_jp, method=c("center", "scale"))
suicides_jp.scaled <- predict(pp_jp, km_suicides_jp)
head(suicides_jp.scaled)
summary(suicides_jp.scaled)

### MODEL 6 PP
pp_jp_no_year <- preProcess(km_suicides_jp_no_year, method=c("center", "scale"))
suicides_jp_no_year.scaled <- predict(pp_jp_no_year, km_suicides_jp_no_year)
head(suicides_jp_no_year.scaled)
summary(suicides_jp_no_year.scaled)

## K-MEANS MODELS

### Model 1
set.seed(377)
km_full <- kmeans(suicides_full.scaled, iter.max=100, 10)
km_full
# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km_full$withinss
# betweenss: sum of squared distances between each cluster mean and
# the data mean
km_full$betweenss
# number of iters
km_full$iter
# cluster centroids
km_full$centers
# cluster for each point
# km_full$cluster
# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km_full$tot.withinss
# the number of observations in each cluster -- table(km$cluster) also works
km_full$size
# Selecting the value of K
dat <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat$SS <- sapply(dat$k, function(k) {
  set.seed(144)
  kmeans(suicides_full.scaled, iter.max=100, k)$tot.withinss
})
ggplot(dat, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 10, color = "blue")
# choose k = 10

### Model 2
set.seed(377)
km_full_no_year <- kmeans(suicides_full_no_year.scaled, iter.max=100, 8)
km_full_no_year
# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km_full_no_year$withinss
# betweenss: sum of squared distances between each cluster mean and
# the data mean
km_full_no_year$betweenss
# number of iters
km_full_no_year$iter
# cluster centroids
km_full_no_year$centers
# cluster for each point
# km_full_no_year$cluster
# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km_full_no_year$tot.withinss
# the number of observations in each cluster -- table(km$cluster) also works
km_full_no_year$size
# Selecting the value of K
dat_no_year <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat_no_year$SS <- sapply(dat_no_year$k, function(k) {
  set.seed(144)
  kmeans(suicides_full_no_year.scaled, iter.max=100, k)$tot.withinss
})
ggplot(dat_no_year, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 8, color = "blue")
# choose k = 8

### Model 3
set.seed(377)
km_us <- kmeans(suicides_us.scaled, iter.max=100, 10)
km_us
# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km_us$withinss
# betweenss: sum of squared distances between each cluster mean and
# the data mean
km_us$betweenss
# number of iters
km_us$iter
# cluster centroids
km_us$centers
# cluster for each point
# km_us$cluster
# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km_us$tot.withinss
# the number of observations in each cluster -- table(km$cluster) also works
km_us$size
# Selecting the value of K
dat_us <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat_us$SS <- sapply(dat_us$k, function(k) {
  set.seed(144)
  kmeans(suicides_us.scaled, iter.max=100, k)$tot.withinss
})
ggplot(dat, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 10, color = "blue")
# choose k = 10

### Model 4
set.seed(377)
km_us_no_year <- kmeans(suicides_us_no_year.scaled, iter.max=100, 5)
km_us_no_year
# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km_us_no_year$withinss
# betweenss: sum of squared distances between each cluster mean and
# the data mean
km_us_no_year$betweenss
# number of iters
km_us_no_year$iter
# cluster centroids
km_us_no_year$centers
# cluster for each point
# km_us_no_year$cluster
# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km_us_no_year$tot.withinss
# the number of observations in each cluster -- table(km$cluster) also works
km_us_no_year$size
# Selecting the value of K
dat_us_no_year <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat_us_no_year$SS <- sapply(dat_us_no_year$k, function(k) {
  set.seed(144)
  kmeans(suicides_us_no_year.scaled, iter.max=100, k)$tot.withinss
})
ggplot(dat_us_no_year, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 5, color = "blue")
# choose k = 5

### Model 5
set.seed(377)
km_jp <- kmeans(suicides_jp.scaled, iter.max=100, 8)
km_jp
# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km_jp$withinss
# betweenss: sum of squared distances between each cluster mean and
# the data mean
km_jp$betweenss
# number of iters
km_jp$iter
# cluster centroids
km_jp$centers
# cluster for each point
# km_jp$cluster
# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km_jp$tot.withinss
# the number of observations in each cluster -- table(km$cluster) also works
km_jp$size
# Selecting the value of K
dat_jp <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat_jp$SS <- sapply(dat_jp$k, function(k) {
  set.seed(144)
  kmeans(suicides_jp.scaled, iter.max=100, k)$tot.withinss
})
ggplot(dat_jp, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 8, color = "blue")
# choose k = 8

### Model 6
set.seed(377)
km_jp_no_year <- kmeans(suicides_jp_no_year.scaled, iter.max=100, 6)
km_jp_no_year
# how good is the clustering
# withinss: sum of squared distances between each datapoint and its 
# cluster mean
km_jp_no_year$withinss
# betweenss: sum of squared distances between each cluster mean and
# the data mean
km_jp_no_year$betweenss
# number of iters
km_jp_no_year$iter
# cluster centroids
km_jp_no_year$centers
# cluster for each point
# km_jp_no_year$cluster
# the sum of the squared distances of each observation from its cluster centroid.
# we use it to measure cluster dissimilarity / is the objective function for k-means
km_jp_no_year$tot.withinss
# the number of observations in each cluster -- table(km$cluster) also works
km_jp_no_year$size
# Selecting the value of K
dat_jp_no_year <- data.frame(k = 1:100)
# what is sapply? Apply a function over a list or vector
dat_jp_no_year$SS <- sapply(dat_jp_no_year$k, function(k) {
  set.seed(144)
  kmeans(suicides_jp_no_year.scaled, iter.max=100, k)$tot.withinss
})
ggplot(dat_jp_no_year, aes(x=k, y=SS)) +
  geom_line() +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares") +
  geom_vline(xintercept = 6, color = "blue")
# choose k = 6







