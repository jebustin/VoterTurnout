## DS 303 Final Project
# Authors: Jessie Bustin and Ben Litterer
#
# Linear Model Predicting Turnout Rate and Assumptions
#
# Data collected from:
# 2012 & 2016 American Community Survey US Census Bureau
# https://www.census.gov/acs/www/data/data-tables-and-tools/
# MIT Election + Data Lab
# https://electionlab.mit.edu/research/voter-turnout

## Load Libraries
library(leaps)
library(ISLR)
library(tidyverse)
library(caret)
library(ggplot2)

## Import Data
vote <- read.csv("Turnout_Demos_County.csv")

vote <- vote %>%
  select(-X)

## Check for Multicolinearity Issues
voteNum <- vote %>%
  select(c(turnout, year, totalPop, under24, over65, medianAge, sexRatio, whitePercent, 
           highschool, bachelor, grad, medianIncome, meanIncome))
cor(voteNum)

## Dropping grad degree rates because of very high correlation with bachelor's degrees
## Also, research showed that bachelor's are more influential than grad's
vote <- vote %>%
  select(-grad)

## Creating new variable for ranking income skew using median - mean
vote <- vote %>%
  mutate(incomeSkew = medianIncome - meanIncome)

hist(vote$incomeSkew)

## Dropping mean/median Income 1 at a time to see which is best
## Much better with median in
vote <- vote %>%
  select(-meanIncome)

## Checking for transformations
# Turnout
hist(vote$turnout)

# Total Population
plot(vote$turnout, vote$totalPop)
hist(vote$totalPop)

## Below used to create more polished plots for final report
vote %>% ggplot(aes(x = totalPop)) +
  geom_histogram() +
  ggtitle("Total Population by County") +
  xlab("Population")

vote %>% ggplot(aes(x = totalPop, y = turnout)) +
  geom_point() +
  ggtitle("Total Population vs Turnout Rate") +
  xlab("Population") +
  ylab("Voter Turnout Rate")


vote <- vote %>%
  mutate(logTotalPop = log(totalPop)) %>%
  select(-totalPop)

hist(vote$logTotalPop)
plot(vote$turnout, vote$logTotalPop)

## Below used to create more polished plots for final report
vote %>% ggplot(aes(x = logTotalPop)) +
  geom_histogram() +
  ggtitle("Log of Total Population by County") +
  xlab("Log of Population")

vote %>% ggplot(aes(x = logTotalPop, y = turnout)) +
  geom_point() +
  ggtitle("Log of Total Population vs Turnout Rate") +
  xlab("Log of Population") +
  ylab("Voter Turnout Rate")

# Under 24
plot(vote$turnout, vote$under24)
hist(vote$under24)

# Over 65
plot(vote$turnout, vote$over65)
hist(vote$over65)

# Median Age
plot(vote$turnout, vote$medianAge)
hist(vote$medianAge)

# Sex Ratio
plot(vote$turnout, vote$sexRatio)
hist(vote$sexRatio)

vote <- vote %>%
  mutate(logSexRatio = log(sexRatio)) %>%
  select(-sexRatio)

plot(vote$turnout, vote$logSexRatio)
hist(vote$logSexRatio)

# White Percent
plot(vote$turnout, vote$whitePercent)
hist(vote$whitePercent)

# Tested Log, Binning, inverse, and no improvement

# High School
plot(vote$turnout, vote$highschool)
hist(vote$highschool)

vote <- vote %>%
  mutate(logHighschool = log(highschool)) %>%
  select(-highschool)

plot(vote$turnout, vote$logHighschool)
hist(vote$logHighschool)

# Bachelor's *Adding a constant of 1 because of 0 entries*
plot(vote$turnout, vote$bachelor)
hist(vote$bachelor)

vote <- vote %>%
  mutate(logBachelor = log(bachelor + 1)) %>%
  select(-bachelor)

plot(vote$turnout, vote$logBachelor)
hist(vote$logBachelor)

# Median Income
plot(vote$turnout, vote$medianIncome)
hist(vote$medianIncome)

## Running Model Selection (K-fold cross validation and best subset selection)
k = 10
folds = sample(1:k,nrow(vote),replace=TRUE)

val.errors = matrix(NA,k,13)

## loop over k folds (for j in 1:k)
for(j in 1:k) {
  test <- vote[folds==j,]
  train <- vote[folds!=j,]
  
  best.fit = regsubsets(turnout~year+logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                          logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome+
                          year*logTotalPop, data = train, nbest=1, nvmax=13)
  
  test.mat = model.matrix(turnout~year+logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                              logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome+
                              year*logTotalPop, data = test)
  for(i in 1:13) {
    coef.m = coef(best.fit,id=i)
    pred = test.mat[,names(coef.m)]%*%coef.m
    val.errors[j,i] = mean((test$turnout-pred)^2)
  }
}

cv.errors = apply(val.errors,2,mean)
which.min(cv.errors)

full.reg = regsubsets(turnout~year+logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                        logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome+
                        year*logTotalPop, data = vote, nbest=1, nvmax=13)
coef(full.reg,10)





## Practice on full set below
regfit = regsubsets(turnout~year+logTotalPop+under24+over65+medianAge+logSexRatio+whitePercent+
                      logHighschool+logBachelor+medianIncome+incomeSkew+whitePercent*medianIncome+
                      year*logTotalPop, data = vote, nbest=1, nvmax=13)
regfit.sum = summary(regfit)
regfit.sum
names(regfit.sum)

n = dim(vote)[1]
p = rowSums(regfit.sum$which)
adjr2 = regfit.sum$adjr2
cp = regfit.sum$cp
rss = regfit.sum$rss
AIC = n*log(rss/n) + 2*(p)
BIC = n*log(rss/n) + (p)*log(n)

cbind(p,rss,adjr2,cp,AIC,BIC)
plot(p,BIC)
plot(p,AIC)

which.min(BIC)
which.min(AIC)
which.min(cp)
which.max(adjr2)

coef(regfit,11)


## Model Assumptions
model = lm(turnout~year+logTotalPop+over65+medianAge+logSexRatio+whitePercent+logBachelor
           +medianIncome+incomeSkew+whitePercent*medianIncome+year*logTotalPop,data=vote)
summary(model)
par(mfrow=c(2,2))
plot(model)

## Check VIF
model = lm(turnout~year+logTotalPop+over65+medianAge+logSexRatio+whitePercent+logBachelor
           +medianIncome+incomeSkew,data=vote)
car::vif(model)

## comparing 2012 vs 2016 residuals
model = lm(turnout~year+logTotalPop+over65+medianAge+logSexRatio+whitePercent+logBachelor
           +medianIncome+incomeSkew+whitePercent*medianIncome+year*logTotalPop,data=vote)

turnout <- vote

vote$predict <- predict(model)

vote$residual <- vote$turnout - vote$predict

vote %>% ggplot(aes(y = residual)) +
  geom_boxplot() +
  title("Model Residuals by Year") +
  facet_wrap(~year)

## Filter by year and rerun models
turnout2012 <- turnout %>%
  filter(year == 2012)

turnout2016 <- turnout %>%
  filter(year == 2016)

### Model selection using K-fold cross validation using the above code was run for
### 2012 and 2016 Individually in the R script ModelYrs.R removing year and the year interaction terms.
### The log of highschool was now included again and analysis continues in that script.



