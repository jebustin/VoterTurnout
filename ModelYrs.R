## DS 303 Final Project
# Authors: Jessie Bustin and Ben Litterer
#
# Testing 2012/2016 Linear Models 
#
# Data collected from:
# 2012 & 2016 American Community Survey US Census Bureau
# https://www.census.gov/acs/www/data/data-tables-and-tools/
# MIT Election + Data Lab
# https://electionlab.mit.edu/research/voter-turnout

library(leaps)
library(ISLR)
library(tidyverse)
library(caret)
library(ggplot2)

## Import Data
vote <- read.csv("Turnout_Demos_County.csv")

# Clean and Transform
vote <- vote %>%
  select(-X)

vote <- vote %>%
  mutate(logBachelor = log(bachelor + 1)) %>%
  select(-bachelor) %>%
  mutate(logHighschool = log(highschool)) %>%
  select(-highschool) %>%
  mutate(logSexRatio = log(sexRatio)) %>%
  select(-sexRatio) %>%
  mutate(logTotalPop = log(totalPop)) %>%
  select(-totalPop)

vote <- vote %>%
  mutate(incomeSkew = medianIncome - meanIncome)


turnout2012 <- vote %>%
  filter(year == 2012)

turnout2016 <- vote %>%
  filter(year == 2016)


## Run K-fold with best subset for 2012
k = 10
folds = sample(1:k,nrow(turnout2012),replace=TRUE)

val.errors = matrix(NA,k,11)

## loop over k folds (for j in 1:k)
for(j in 1:k) {
  test <- turnout2012[folds==j,]
  train <- turnout2012[folds!=j,]
  
  best.fit = regsubsets(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                          logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
                        data = train, nbest=1, nvmax=11)
  
  test.mat = model.matrix(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                            logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
                          data = test)
  for(i in 1:11) {
    coef.m = coef(best.fit,id=i)
    pred = test.mat[,names(coef.m)]%*%coef.m
    val.errors[j,i] = mean((test$turnout-pred)^2)
  }
}

cv.errors = apply(val.errors,2,mean)
which.min(cv.errors)

full.reg.2012 = regsubsets(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                        logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
                      data = turnout2012, nbest=1, nvmax=11)
coef(full.reg.2012,10)

## Repeat for 2016
k = 10
folds = sample(1:k,nrow(turnout2016),replace=TRUE)

val.errors = matrix(NA,k,11)

## loop over k folds (for j in 1:k)
for(j in 1:k) {
  test <- turnout2016[folds==j,]
  train <- turnout2016[folds!=j,]
  
  best.fit = regsubsets(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                          logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
                        data = train, nbest=1, nvmax=11)
  
  test.mat = model.matrix(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                            logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
                          data = test)
  for(i in 1:11) {
    coef.m = coef(best.fit,id=i)
    pred = test.mat[,names(coef.m)]%*%coef.m
    val.errors[j,i] = mean((test$turnout-pred)^2)
  }
}

cv.errors = apply(val.errors,2,mean)
which.min(cv.errors)

full.reg.2016 = regsubsets(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                        logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
                      data = turnout2016, nbest=1, nvmax=11)
coef(full.reg.2016,10)

## Calculating Test MSE using K-fold Cross Validation
k = 10
folds = sample(1:k,nrow(turnout2012),replace=TRUE)

MSE2012 = rep(NA, 10)

for (j in 1:k) {
  test <- turnout2012[folds==j,]
  train <- turnout2012[folds!=j,]
  
  model <- lm(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
              data = turnout2012)
  
  pred <- predict(model, test)
  
  MSE2012[j] = mean((test$turnout-pred)^2)
}

testMSE2012 <- mean(MSE2012)
testMSE2012

# Repeat with 2016
k = 10
folds = sample(1:k,nrow(turnout2016),replace=TRUE)

MSE2016 = rep(NA, 10)

for (j in 1:k) {
  test <- turnout2016[folds==j,]
  train <- turnout2016[folds!=j,]
  
  model <- lm(turnout~logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome,
              data = turnout2016)
  
  pred <- predict(model, test)
  
  MSE2016[j] = mean((test$turnout-pred)^2)
}

testMSE2016 <- mean(MSE2016)
testMSE2016

## Checking VIF again for 2012 and 2016 Models now
model2012 = lm(turnout~logTotalPop+over65+medianAge+logSexRatio+whitePercent+logHighschool+logBachelor
           +medianIncome+incomeSkew,data=turnout2012)
car::vif(model2012)

model2016 = lm(turnout~logTotalPop+over65+medianAge+logSexRatio+whitePercent+logHighschool+logBachelor
               +medianIncome+incomeSkew,data=turnout2016)
car::vif(model2016)

## Mapping Residuals for models
# Get residuals
model2012 = lm(turnout~logTotalPop+over65+medianAge+logSexRatio+whitePercent+logHighschool+logBachelor
               +medianIncome+incomeSkew+whitePercent*medianIncome,data=turnout2012)
turnout2012$prediction <- predict(model2012)
turnout2012$residual <- turnout2012$turnout - turnout2012$prediction


model2016 = lm(turnout~logTotalPop+over65+medianAge+logSexRatio+whitePercent+logHighschool+logBachelor
               +medianIncome+incomeSkew+whitePercent*medianIncome,data=turnout2016)
turnout2016$prediction <- predict(model2016)
turnout2016$residual <- turnout2016$turnout - turnout2016$prediction

# clean and merge
counties <- map_data('county')

counties <- counties %>%
  rename(state = region, county = subregion)

turnout2012 <- turnout2012 %>%
  mutate(county = tolower(county)) %>%
  mutate(state = tolower(state))

turnout2016 <- turnout2016 %>%
  mutate(county = tolower(county)) %>%
  mutate(state = tolower(state))

counties2012 <- left_join(turnout2012, counties, by = c("county", "state"))
counties2016 <- left_join(turnout2016, counties, by = c("county", "state"))

# Map
plot <- ggplot() +
  geom_polygon(data = counties2012, aes(x = long, y = lat, group = group, fill = residual)) +
  coord_map() +
  ggtitle("2012 Voter Turnout Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("")

plot <- plot + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_blank(), axis.ticks = element_blank())

plot + scale_fill_gradient2()

plot <- ggplot() +
  geom_polygon(data = counties2016, aes(x = long, y = lat, group = group, fill = residual)) +
  coord_map() +
  ggtitle("2016 Voter Turnout Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("")

plot <- plot + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "white"), axis.text = element_blank(), axis.ticks = element_blank())

plot + scale_fill_gradient2()

## Model Assumptions
par(mfrow=c(2,2))
plot(model2012)

par(mfrow=c(2,2))
plot(model2016)

