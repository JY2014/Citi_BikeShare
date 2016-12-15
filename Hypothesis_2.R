setwd("C:/Users/JY/Documents/GitHub/Stat139_project")


############### Project: Hypothesis 2 #################
########### Predicting rush hour trips ################

# Load the data
Data <- read.csv("organized_data_3.csv")
head(Data)


###### Data cleaning ######

# remove the neighborhood regions that tend to have 0 trips
unique(Data$neighborhood[Data$afternoon_outgoing == 0])
unique(Data$neighborhood[Data$afternoon_incoming == 0])

# find the regions maintained for analysis
removed_regions <- unique(Data$neighborhood[Data$afternoon_incoming == 0])
# save the rest into a new dataframe
Data_regions_rm <- Data[! Data$neighborhood %in% removed_regions, ]


### Check response variables
## afternoon outgoing trips
hist(Data_regions_rm$afternoon_outgoing)
# transformation
hist(log(Data_regions_rm$afternoon_outgoing)) # too much
hist(sqrt(Data_regions_rm$afternoon_outgoing)) # not enough
hist((Data_regions_rm$afternoon_outgoing)^(1/3), breaks = 20)
hist((Data_regions_rm$afternoon_outgoing)^(1/4), breaks = 20)
hist((Data_regions_rm$afternoon_outgoing)^(1/5), breaks = 20)

qqnorm((Data_regions_rm$afternoon_outgoing)^(1/4))
qqline((Data_regions_rm$afternoon_outgoing)^(1/4))

##
### The transformation using ^(1/4) or ^(1/5) are the best
### Decide to use ^(1/5)
##
# store into a new dataframe
Data_pred_out <- Data_regions_rm['afternoon_outgoing']
Data_pred_out$afternoon_outgoing <- (Data_regions_rm$afternoon_outgoing)^(1/5)


## afternoon incoming trips
hist(Data_regions_rm$afternoon_incoming, breaks = 20)
# transformation
hist((Data_regions_rm$afternoon_incoming)^(1/5), breaks = 20)
qqnorm((Data_regions_rm$afternoon_incoming)^(1/5))
qqline((Data_regions_rm$afternoon_incoming)^(1/5))
# transform using ^(1/5)
Data_pred_in <- Data_regions_rm['afternoon_incoming']
Data_pred_in$afternoon_incoming <- (Data_regions_rm$afternoon_incoming)^(1/5)


### Check predictor variables
## morning_outgoing and morning_incoming
# transformation
Data_pred_out$morning_incoming <- Data_pred_in$morning_incoming <- (Data_regions_rm$morning_incoming)^(1/5)
Data_pred_out$morning_outgoing <- Data_pred_in$morning_outgoing <- (Data_regions_rm$morning_outgoing)^(1/5)

pairs(Data_pred_in)
pairs(Data_pred_out)

## user: % subscriber (doesn't seem to be a relationship)
plot(Data_regions_rm$monring_in_user, Data_pred_in$afternoon_incoming)

## trip duration
plot(Data_regions_rm$morning_out_duration, Data_pred_in$afternoon_incoming)
#fit1 <- lm(Data_pred_in$afternoon_incoming ~ Data_regions_rm$morning_out_duration)
#abline(fit1)

# add these variables into the dataframes
Data_pred_in$morning_out_duration <- Data_pred_out$morning_out_duration <- Data_regions_rm$morning_out_duration
Data_pred_in$morning_in_duration <- Data_pred_out$morning_in_duration <- Data_regions_rm$morning_in_duration
Data_pred_in$morning_out_user <- Data_pred_out$morning_out_user <- Data_regions_rm$monring_out_user
Data_pred_in$morning_in_user <- Data_pred_out$morning_in_user <- Data_regions_rm$monring_in_user
Data_pred_in$neighborhood <- Data_pred_out$neighborhood <- Data_regions_rm$neighborhood
Data_pred_in$day_of_week <- Data_pred_out$day_of_week <- factor(Data_regions_rm$day_of_week)
Data_pred_in$month <- Data_pred_out$month <- factor(Data_regions_rm$month)



##### Model selection on predicting afternoon incoming trips ######

### single models
# only morning outgoing
model1 <- lm(afternoon_incoming ~ morning_outgoing, data = Data_pred_in)
summary(model1)

# check interaction with month
model1_1 <- lm(afternoon_incoming ~ morning_outgoing*month, data = Data_pred_in)
summary(model1_1)

# morning outgoing and incoming
model2 <- lm(afternoon_incoming ~ morning_outgoing + morning_incoming, data = Data_pred_in)
summary(model2)

# with interaction (interaction not significant)
model3 <- lm(afternoon_incoming ~ morning_outgoing * morning_incoming, data = Data_pred_in)
summary(model3)

# with all the other predictors
model4 <- lm(afternoon_incoming ~ ., data = Data_pred_in)
summary(model4)

# check residuals of model 4 by month
plot(model4$fitted.values[Data_pred_in$month == 7], model4$residuals[Data_pred_in$month == 7], col = 'red', 
     xlim = c(min(model4$fitted.values), max(model4$fitted.values)), main = "redisual by month",
     ylab = "residuals")
points(model4$fitted.values[Data_pred_in$month == 8], model4$residuals[Data_pred_in$month == 8], col = 'blue') 
points(model4$fitted.values[Data_pred_in$month == 9], model4$residuals[Data_pred_in$month == 9], col = 'green')


### Model Selection
# in model 4, almost all the predictors except durations are significant
# include all predictors in the model

# forward selection from no predictor to all predictors with interaction
model5 <- step(lm(afternoon_incoming~ 1, data = Data_pred_in), 
               scope = list(upper = lm(afternoon_incoming~ .^2, data = Data_pred_in)),
               direction = 'forward')
summary(model5)

# backward selection from all predictors with interaction to no predictor
model6 <- step(lm(afternoon_incoming~ .^2, data = Data_pred_in), direction = 'backward')
summary(model6)

# stepwise selection starting from main effects
model7 <- step(model4, scope = list(upper = lm(afternoon_incoming~ .^2, data = Data_pred_in),
                                    lower = lm(afternoon_incoming~ 1, data = Data_pred_in)),
               direction = 'both')
summary(model7)


## further compare model 5-7 by cross validation

# randomize all the data

# create training and testing sets first
n <- dim(Data_pred_in)[1]
# use 70% as training set
n_train <- round(n * 0.7)

niter <- 500
# results of RSS on the original scale
RSS1 <- list() #store result of model5
RSS2 <- list() #store result of model6
RSS3 <- list() #store result of model7

for (i in 1:niter){
  set.seed(i)
  reorder <- sample(n)
  train <- Data_pred_in[reorder[1:n_train], ]
  test <- Data_pred_in[reorder[(n_train+1):n], ]
  
  # model 5
  fit1 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
               month + day_of_week + morning_in_user + month:day_of_week + 
               neighborhood:morning_outgoing + neighborhood:month + morning_outgoing:month + 
               morning_outgoing:day_of_week + day_of_week:morning_in_user + 
               month:morning_in_user, data = train)
  
  result <- predict(fit1, new = test)
  RSS <- sum((test$afternoon_incoming^5 - result^5)^2)
  
  RSS1 <- c(RSS1, RSS)
  
  # model 6
  fit2 <- lm(afternoon_incoming ~ morning_incoming + morning_outgoing + 
               morning_out_duration + morning_in_duration + morning_out_user + 
               morning_in_user + neighborhood + day_of_week + month + morning_incoming:morning_outgoing + 
               morning_incoming:neighborhood + morning_incoming:day_of_week + 
               morning_incoming:month + morning_outgoing:morning_out_duration + 
               morning_outgoing:morning_in_user + morning_outgoing:day_of_week + 
               morning_outgoing:month + morning_out_duration:morning_in_duration + 
               morning_out_duration:morning_in_user + morning_out_duration:neighborhood + 
               morning_out_duration:month + morning_in_duration:morning_out_user + 
               morning_in_user:day_of_week + neighborhood:day_of_week + 
               day_of_week:month, data = train)
  result <- predict(fit2, new = test)
  RSS <- sum((test$afternoon_incoming^5 - result^5)^2)
  
  RSS2 <- c(RSS2, RSS)
  
  # model 7
  fit3 <- lm(afternoon_incoming ~ morning_incoming + morning_outgoing + 
               morning_out_duration + morning_in_user + neighborhood + day_of_week + 
               month + day_of_week:month + morning_incoming:morning_outgoing + 
               morning_incoming:neighborhood + neighborhood:month + morning_outgoing:month + 
               morning_in_user:day_of_week + morning_out_duration:neighborhood + 
               morning_out_duration:month, data = train)
  result <- predict(fit3, new = test)
  RSS <- sum((test$afternoon_incoming^5 - result^5)^2)
  
  RSS3 <- c(RSS3, RSS)
}

n_test <- dim(test)[1]
mean(as.numeric(RSS1))/n_test
mean(as.numeric(RSS2))/n_test
mean(as.numeric(RSS3))/n_test # model 7 is the best


# train using month 7, and test using month 8
# delete month in the models
month7 <- Data_pred_in[Data_pred_in$month == 7, ]
month8 <- Data_pred_in[Data_pred_in$month == 8, ]
# model 5
fit1 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
             day_of_week + morning_in_user + 
             neighborhood:morning_outgoing + 
             morning_outgoing:day_of_week + day_of_week:morning_in_user, 
             data = month7)
result <- predict(fit1, new = month8)
RSS4 <- sum((month8$afternoon_incoming^5 - result^5)^2)

#model 6
fit2 <- lm(afternoon_incoming ~ morning_incoming + morning_outgoing + 
             morning_out_duration + morning_in_duration + morning_out_user + 
             morning_in_user + neighborhood + day_of_week + morning_incoming:morning_outgoing + 
             morning_incoming:neighborhood + morning_incoming:day_of_week + 
             morning_outgoing:morning_out_duration + 
             morning_outgoing:morning_in_user + morning_outgoing:day_of_week + 
             morning_out_duration:morning_in_duration + 
             morning_out_duration:morning_in_user + morning_out_duration:neighborhood + 
             morning_in_duration:morning_out_user + 
             morning_in_user:day_of_week + neighborhood:day_of_week, 
             data = month7)
result <- predict(fit2, new = month8)
RSS5 <- sum((month8$afternoon_incoming^5 - result^5)^2)

# model 7
fit3 <- lm(afternoon_incoming ~ morning_incoming + morning_outgoing + 
             morning_out_duration + morning_in_user + neighborhood + day_of_week + 
             morning_incoming:morning_outgoing + 
             morning_incoming:neighborhood + 
             morning_in_user:day_of_week + morning_out_duration:neighborhood, 
             data = month7)
result <- predict(fit3, new = month8)
RSS6 <- sum((month8$afternoon_incoming^5 - result^5)^2)




### reselect models without month as a predictor
Data_pred_in_withoutmonth <- Data_pred_in
Data_pred_in_withoutmonth$month <- NULL

# some basic models first
model4_2 <- lm(afternoon_incoming ~ ., data = Data_pred_in_withoutmonth)
summary(model4_2) 
  

# forward selection from no predictor to all predictors with interaction
model8 <- step(lm(afternoon_incoming~ 1, data = Data_pred_in_withoutmonth), 
               scope = list(upper = lm(afternoon_incoming~ .^2, data = Data_pred_in_withoutmonth)),
               direction = 'forward')
summary(model8)

# backward selection from all predictors with interaction to no predictor
model9 <- step(lm(afternoon_incoming~ .^2, data = Data_pred_in_withoutmonth), direction = 'backward')
summary(model9)

# stepwise selection starting from main effects
model10 <- step(lm(afternoon_incoming~ ., data = Data_pred_in_withoutmonth),
                scope = list(upper = lm(afternoon_incoming~ .^2, data = Data_pred_in_withoutmonth),
                                    lower = lm(afternoon_incoming~ 1, data = Data_pred_in_withoutmonth)),
               direction = 'both')
summary(model10)


# train using month 7, and test using month 8
# model 8
fit1 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
             day_of_week + neighborhood:morning_outgoing + morning_outgoing:day_of_week, 
           data = month7)
result <- predict(fit1, new = month8)
RSS7 <- sum((month8$afternoon_incoming^5 - result^5)^2)

#model 9
fit2 <- lm(afternoon_incoming ~ morning_outgoing + morning_out_duration + 
             morning_out_user + morning_in_user + neighborhood + day_of_week + 
             morning_outgoing:morning_out_duration + morning_outgoing:morning_in_user + 
             morning_outgoing:neighborhood + morning_out_duration:morning_in_user + 
             morning_out_duration:neighborhood + morning_in_user:neighborhood + 
             morning_in_user:day_of_week, 
           data = month7)
result <- predict(fit2, new = month8)
RSS8 <- sum((month8$afternoon_incoming^5 - result^5)^2)

# model 10
fit3 <- lm(afternoon_incoming ~ morning_outgoing + morning_in_user + 
             neighborhood + day_of_week + morning_outgoing:neighborhood + 
             morning_in_user:day_of_week + morning_outgoing:day_of_week, 
           data = month7)
result <- predict(fit3, new = month8)
RSS9 <- sum((month8$afternoon_incoming^5 - result^5)^2)



# further use lasso to tune model 9
# load the package
library(glmnet)
# generate the data
# month 7
x <- model.matrix(fit2)
y <- as.matrix(month7[,1])

# month 8
fit4 <- lm(afternoon_incoming ~ morning_outgoing + morning_out_duration + 
             morning_out_user + morning_in_user + neighborhood + day_of_week + 
             morning_outgoing:morning_out_duration + morning_outgoing:morning_in_user + 
             morning_outgoing:neighborhood + morning_out_duration:morning_in_user + 
             morning_out_duration:neighborhood + morning_in_user:neighborhood + 
             morning_in_user:day_of_week, 
           data = month8)
x2 <- model.matrix(fit4)

# list of lambda
lambda_list <- 10 ^ seq(-10, 10)

best_RSS <- 1e10
best_lambda <- 0

for (param in lambda_list){
  # fit model
  fit1 <- glmnet(x, y, family="gaussian", alpha=1, lambda=param)
  # test on month 8
  prediction <- predict(fit1, x2)
  RSS <- sum((month8$afternoon_incoming^5 - prediction^5)^2)
  
  if (RSS < best_RSS){
    best_RSS <- RSS
    best_lambda <- param
    best_fit <- fit1
  }
}
best_RSS/dim(month8)[1]


# prediction on month 9
month9 <- Data_pred_in[Data_pred_in$month == 9, ]
fit5 <- lm(afternoon_incoming ~ morning_outgoing + morning_out_duration + 
             morning_out_user + morning_in_user + neighborhood + day_of_week + 
             morning_outgoing:morning_out_duration + morning_outgoing:morning_in_user + 
             morning_outgoing:neighborhood + morning_out_duration:morning_in_user + 
             morning_out_duration:neighborhood + morning_in_user:neighborhood + 
             morning_in_user:day_of_week, 
           data = month9)
x3 <- model.matrix(fit5)

prediction3 <- predict(best_fit, x3)
RSS10 <- sum((month9$afternoon_incoming^5 - prediction3^5)^2)
RSS10/dim(month9)[1]


# Check residuals by month
prediction1 <- predict(best_fit, x)
prediction2 <- predict(best_fit, x2)
plot(prediction1, month7$afternoon_incoming-prediction1, col = 'red', 
     xlim = c(min(model4$fitted.values), max(model4$fitted.values)), main = "redisuals by month",
     ylab = "residuals", xlab = 'fitted values')
points(prediction2, month8$afternoon_incoming-prediction2, col = 'blue') 
points(prediction3, month9$afternoon_incoming-prediction3, col = 'green')

z <- c("prediction1", "prediction2", "prediction3")
dataList <- lapply(z, get, envir=environment())
names(dataList) <- z
boxplot(dataList)


# look at the MSE of Greenpoint in Month 8
prediction4 <- predict(best_fit, x2[month8$neighborhood == 'Greenpoint', ])
MSE <- sum((month8[month8$neighborhood =='Greenpoint', ]$afternoon_incoming^5 - prediction4^5)^2)/
  dim(month8[month8$neighborhood =='Greenpoint', ])[1]


# check assumptions of the best model
resid_best <- month7$afternoon_incoming - prediction1
plot(resid_best, prediction1)
qqnorm(resid_best)
qqline(resid_best)




############### Model Comparison ###############
# train each model on July data, and predict Aug data
# train each model on Aug data, and predict Sep data
# average the R_squared of the two months

# Total Sum of Squares
SST_Aug <- sum((month8$afternoon_incoming^5 - mean(month8$afternoon_incoming^5))^2)
SST_Sep <- sum((month9$afternoon_incoming^5 - mean(month9$afternoon_incoming^5))^2)

### Model 1: only morning_outgoing trips
# July --> Aug
model_1 <- lm(afternoon_incoming ~ morning_outgoing, data = month7)
Aug_pred_1 <- predict(model_1, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_1^5)^2)
R_squared_Aug_1 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_1 <- lm(afternoon_incoming ~ morning_outgoing, data = month8)
Sep_pred_1 <- predict(model_1, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_1^5)^2)
R_squared_Sep_1 <- 1 - (SSR/SST_Sep)


### Model 2: morning outgoing and incoming
# July --> Aug
model_2 <- lm(afternoon_incoming ~ morning_outgoing + morning_incoming, data = month7)
Aug_pred_2 <- predict(model_2, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_2^5)^2)
R_squared_Aug_2 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_2 <- lm(afternoon_incoming ~ morning_outgoing + morning_incoming, data = month8)
Sep_pred_2 <- predict(model_2, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_2^5)^2)
R_squared_Sep_2 <- 1 - (SSR/SST_Sep)


# Model 3: morning outgoing and incoming with interaction
# July --> Aug
model_3 <- lm(afternoon_incoming ~ morning_outgoing * morning_incoming, data = month7)
Aug_pred_3 <- predict(model_3, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_3^5)^2)
R_squared_Aug_3 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_3 <- lm(afternoon_incoming ~ morning_outgoing * morning_incoming, data = month8)
Sep_pred_3 <- predict(model_3, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_3^5)^2)
R_squared_Sep_3 <- 1 - (SSR/SST_Sep)


# Model 4: model from forward selection (model8)
# July --> Aug
model_4 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
           day_of_week + neighborhood:morning_outgoing + morning_outgoing:day_of_week, 
           data = month7)
Aug_pred_4 <- predict(model_4, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_4^5)^2)
R_squared_Aug_4 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_4 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
              day_of_week + neighborhood:morning_outgoing + morning_outgoing:day_of_week, 
              data = month9)
Sep_pred_4 <- predict(model_4, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_4^5)^2)
R_squared_Sep_4 <- 1 - (SSR/SST_Sep)


# Model 5: model from backward selection (model9)
# July --> Aug
model_5 <- lm(afternoon_incoming ~ morning_outgoing + morning_out_duration + 
                morning_out_user + morning_in_user + neighborhood + day_of_week + 
                morning_outgoing:morning_out_duration + morning_outgoing:morning_in_user + 
                morning_outgoing:neighborhood + morning_out_duration:morning_in_user + 
                morning_out_duration:neighborhood + morning_in_user:neighborhood + 
                morning_in_user:day_of_week, 
              data = month7)
Aug_pred_5 <- predict(model_5, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_5^5)^2)
R_squared_Aug_5 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_5 <- lm(afternoon_incoming ~ morning_outgoing + morning_out_duration + 
                morning_out_user + morning_in_user + neighborhood + day_of_week + 
                morning_outgoing:morning_out_duration + morning_outgoing:morning_in_user + 
                morning_outgoing:neighborhood + morning_out_duration:morning_in_user + 
                morning_out_duration:neighborhood + morning_in_user:neighborhood + 
                morning_in_user:day_of_week, 
              data = month7)
Sep_pred_5 <- predict(model_5, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_5^5)^2)
R_squared_Sep_5 <- 1 - (SSR/SST_Sep)


# Model 6: model from stepwise selection (model10)
# July --> Aug
model_6 <- lm(afternoon_incoming ~ morning_outgoing + morning_in_user + 
                neighborhood + day_of_week + morning_outgoing:neighborhood + 
                morning_in_user:day_of_week + morning_outgoing:day_of_week, 
              data = month7)
Aug_pred_6 <- predict(model_6, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_6^5)^2)
R_squared_Aug_6 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_6 <- lm(afternoon_incoming ~ morning_outgoing + morning_in_user + 
                neighborhood + day_of_week + morning_outgoing:neighborhood + 
                morning_in_user:day_of_week + morning_outgoing:day_of_week, 
              data = month7)
Sep_pred_6 <- predict(model_6, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_6^5)^2)
R_squared_Sep_6 <- 1 - (SSR/SST_Sep)


# Model 7: using lasso to tune model 4
## first generate expanded predictors for each month
# July
model_4 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
              day_of_week + neighborhood:morning_outgoing + morning_outgoing:day_of_week, 
              data = month7)
x1 <- model.matrix(model_4)
y1 <- as.matrix(month7[,1])
# Aug
model_4 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
              day_of_week + neighborhood:morning_outgoing + morning_outgoing:day_of_week, 
              data = month8)
x2 <- model.matrix(model_4)
y2 <- as.matrix(month8[,1])
# Sep
model_4 <- lm(afternoon_incoming ~ neighborhood + morning_outgoing + 
              day_of_week + neighborhood:morning_outgoing + morning_outgoing:day_of_week, 
              data = month9)
x3 <- model.matrix(model_4)


# load the package
library(glmnet)

# list of lambda
lambda_list <- 10 ^ seq(-10, 10)

# Lasso regularization
best_R_squared <- 0
R_squared_Aug_7 <- 0
R_squared_Sep_7 <- 0
best_lambda <- 0

for (param in lambda_list){
  # fit model on July
  fit1 <- glmnet(x1, y1, family="gaussian", alpha=1, lambda=param)
  # test on Aug
  prediction <- predict(fit1, x2)
  RSS <- sum((month8$afternoon_incoming^5 - prediction^5)^2)
  R_squared_1 <- 1 - (RSS/SST_Aug)
  
  # fit model on Aug
  fit1 <- glmnet(x2, y2, family="gaussian", alpha=1, lambda=param)
  # test on Sep
  prediction <- predict(fit1, x3)
  RSS <- sum((month9$afternoon_incoming^5 - prediction^5)^2)
  R_squared_2 <- 1 - (RSS/SST_Sep)
  
  # averaged R-squared
  R_squared <- (R_squared_1 + R_squared_2)/2
  # update
  if (R_squared > best_R_squared){
    best_R_squared <- R_squared
    R_squared_Aug_7 <- R_squared_1
    R_squared_Sep_7 <- R_squared_2
    best_lambda <- param
  }
}


# Model 8: using only main effects
# July --> Aug
model_8 <- lm(afternoon_incoming ~ morning_incoming+morning_outgoing+morning_in_user+morning_out_user
              +morning_in_duration+morning_out_duration+neighborhood+day_of_week, data = month7)
Aug_pred_8 <- predict(model_8, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_8^5)^2)
R_squared_Aug_8 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_8 <- lm(afternoon_incoming ~ morning_incoming+morning_outgoing+morning_in_user+morning_out_user
              +morning_in_duration+morning_out_duration, data = month8)
Sep_pred_8 <- predict(model_8, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_8^5)^2)
R_squared_Sep_8 <- 1 - (SSR/SST_Sep)


# Model 9: adding neighborhood to model 1
# July --> Aug
model_9 <- lm(afternoon_incoming ~ morning_outgoing+neighborhood, data = month7)
Aug_pred_9 <- predict(model_9, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_9^5)^2)
R_squared_Aug_9 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_9 <- lm(afternoon_incoming ~ morning_outgoing+neighborhood, data = month8)
Sep_pred_9 <- predict(model_9, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_9^5)^2)
R_squared_Sep_9 <- 1 - (SSR/SST_Sep)


# Model 10: adding neighborhood and interaction to model 1
# July --> Aug
model_10 <- lm(afternoon_incoming ~ morning_outgoing*neighborhood, data = month7)
Aug_pred_10 <- predict(model_10, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_10^5)^2)
R_squared_Aug_10 <- 1 - (SSR/SST_Aug)
# Aug --> Sep
model_10 <- lm(afternoon_incoming ~ morning_outgoing+neighborhood, data = month8)
Sep_pred_10 <- predict(model_10, new = month9)
SSR <- sum((month9$afternoon_incoming^5 - Sep_pred_10^5)^2)
R_squared_Sep_10 <- 1 - (SSR/SST_Sep)


# Model 11: adding day-of-week to model 10
model_11 <- lm(afternoon_incoming ~ morning_outgoing*neighborhood*as.factor(day_of_week), data = month7)
Aug_pred_11 <- predict(model_11, new = month8)
SSR <- sum((month8$afternoon_incoming^5 - Aug_pred_11^5)^2)
R_squared_Aug_11 <- 1 - (SSR/SST_Aug)


# summary
Aug_summary <- c(R_squared_Aug_1, R_squared_Aug_2, R_squared_Aug_3, R_squared_Aug_4, R_squared_Aug_5, R_squared_Aug_6,
                 R_squared_Aug_7, R_squared_Aug_8)
Sep_summary <- c(R_squared_Sep_1, R_squared_Sep_2, R_squared_Sep_3, R_squared_Sep_4, R_squared_Sep_5, R_squared_Sep_6,
                 R_squared_Sep_7, R_squared_Sep_8)
Overall_summary <- (as.numeric(Aug_summary) + as.numeric(Sep_summary))/2
# organize into a dataframe
table <- data.frame(cbind(as.matrix(Aug_summary), as.matrix(Sep_summary), as.matrix(Overall_summary)))





################################### try time series models #########################################
###### use 'Greenpoint' as an example
## visualize: 
ts.plot(Data_pred_in$afternoon_incoming[Data_regions_rm$neighborhood == 'Greenpoint'])
ts.plot(Data_pred_out$afternoon_outgoing[Data_regions_rm$neighborhood == 'Greenpoint'])

## combine them together with morning data for one region: Greenpoint
ts_incoming <- cbind(matrix(Data_pred_in$morning_outgoing[Data_pred_in$neighborhood == 'Greenpoint']), 
                     matrix(Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == 'Greenpoint']))
ts_incoming <- as.vector(t(ts_incoming))
# visualize
ts.plot(ts_incoming)
# need to use difference?
ts_incoming_diff <- ts_incoming[2:length(ts_incoming)] - ts_incoming[1:length(ts_incoming)-1]
ts.plot(ts_incoming_diff)
#acf(ts_incoming_diff)
#pacf(ts_incoming_diff)


# use KPSS test:
library(tseries)
kpss.test(ts_incoming) # stationary


# try auto.arima()
library(forecast)
result <- auto.arima(ts_incoming)
plot(forecast(result, h=20))
forecast(result)$mean[1]


######### predict the data in Aug using a 30-day moving window #########
#region = 'Lower East Side'
region = 'Greenpoint'

morning_data <- Data_pred_in$morning_outgoing[Data_pred_in$neighborhood == region]
afternoon_data <- Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == region]

# there are 31 days in both July and Aug
i_start <- 31 
ts_pred_result <- list()
ts_CI_lower <- list()
ts_CI_upper <- list()

for (i in 1:31){
  i_start <- i_start+1  # start of window in the dataset
  i_end <- i_start + 30  # end of window in the dataset
  
  # take morning and afternoon data of these 30 days
  # include the morning data of the same day
  ts_morning <- morning_data[i_start : (i_end + 1)]
  ts_afternoon <- c(afternoon_data[i_start : i_end], 0)
  # combine
  ts_overall <- cbind(matrix(ts_morning), matrix(ts_afternoon))
  ts_overall <- as.vector(t(ts_overall))[1:63]
  
  # use auto.arima to predict
  fit_ts <- auto.arima(ts_overall)
  print (fit_ts$coef)
  # predict
  forecast_ts <- forecast(fit_ts, level=c(95))
  # point prediction
  ts_pred <- forecast_ts$mean[1]
  # 95% confidence interval
  ts_lower <- forecast_ts$lower[1]
  ts_upper <- forecast_ts$upper[1]
  
  # save the result
  ts_pred_result <- c(ts_pred_result, ts_pred)
  ts_CI_lower <- c(ts_CI_lower, ts_lower)
  ts_CI_upper <- c(ts_CI_upper, ts_upper)
}



####### predict for all the regions in Aug ######
region_list <- unique(month8$neighborhood)
#ts_RSS <- 0
ts_RSS_list <- list()

for (region in region_list){
  morning_data <- Data_pred_in$morning_outgoing[Data_pred_in$neighborhood ==  region]
  afternoon_data <- Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == region]
  
  # there are 31 days in both July and Aug
  i_start <- 31 
  ts_pred_result <- list()
  
  for (i in 1:31){
    i_start <- i_start+1  # start of window in the dataset
    i_end <- i_start + 30  # end of window in the dataset
    
    # take morning and afternoon data of these 30 days
    # include the morning data of the same day
    ts_morning <- morning_data[i_start : (i_end + 1)]
    ts_afternoon <- c(afternoon_data[i_start : i_end], 0)
    # combine
    ts_overall <- cbind(matrix(ts_morning), matrix(ts_afternoon))
    ts_overall <- as.vector(t(ts_overall))[1:63]
    
    # use arima (1, 1, 4) to predict
    #fit_ts <- Arima(ts_overall, order=c(1,1,4))
    fit_ts <- auto.arima(ts_overall)
    ts_pred <- forecast(fit_ts)$mean[1]
    # save the result
    ts_pred_result <- c(ts_pred_result, ts_pred)
  }
  RSS <- sum((month8[month8$neighborhood == region, ]$afternoon_incoming^5 - as.numeric(ts_pred_result)^5)^2)
  ts_RSS_list <- c(ts_RSS_list, RSS)
}

MSE3 <- ts_RSS/31*length(region_list)




############### compute R^2 for each region using model_1 and the time series method (Aug) #################
## model_1
RSS_model_1 <- list()
TSS_model_1 <- list()
for (region in region_list){
  #RSS of model 1
  RSS <- sum((month8$afternoon_incoming[month8$neighborhood == region]^5 - 
                Aug_pred_1[month8$neighborhood == region]^5)^2)
  RSS_model_1 <- c(RSS_model_1, RSS)
  
  region_mean <- mean(month8$afternoon_incoming^5)
  TSS <- sum((month8$afternoon_incoming[month8$neighborhood == region]^5-region_mean)^2)
  TSS_model_1 <- c(TSS_model_1, TSS)
}

# R-square per region
result <- 1- as.numeric(RSS_model_1)/as.numeric(TSS_model_1)
result <- as.numeric(RSS_model_1)/31
write.csv(as.matrix(result), "table3.csv")

#ts_comparison <- cbind(as.numeric(ts_RSS_list)/31, as.numeric(RSS_model_1)/31)
#region_list[as.numeric(ts_RSS_list) > as.numeric(RSS_model_1)]
#region_list[as.numeric(ts_RSS_list) < as.numeric(RSS_model_1)]


## model_10
RSS_model_10 <- list()
for (region in region_list){
  #RSS of model 
  RSS <- sum((month8$afternoon_incoming[month8$neighborhood == region]^5 - 
                Aug_pred_10[month8$neighborhood == region]^5)^2)
  RSS_model_10 <- c(RSS_model_10, RSS)
}


ts_comparison <- cbind(as.numeric(ts_RSS_list)/31, as.numeric(RSS_model_1)/31, as.numeric(RSS_model_10)/31)
region_list[as.numeric(ts_RSS_list) > as.numeric(RSS_model_10)]
region_list[as.numeric(ts_RSS_list) < as.numeric(RSS_model_10)]



### using model 1 to predict Greenpoint only
month7_GP <- month7[month7$neighborhood == 'Lower Manhattan', ]
model1_2 <- lm(afternoon_incoming ~ morning_outgoing + morning_incoming, data = month7_GP)


# visualize
p3 <- ggplot(month7_GP, aes(x=morning_outgoing, y=afternoon_incoming), main = 'Greenpoint') +
  geom_point(shape=19) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 1)
p3 + ggtitle("GreenPoint")+
  labs(y="# incoming trips at afternoon rush hour (transformed)",
       x="# outgoing trips at morning rush hour (transformed)") +
  theme(axis.title = element_text(size=14), legend.text = element_text(size=12),
        legend.title = element_text(size=14)) 

p4 <- ggplot(month7_GP, aes(x=morning_incoming, y=afternoon_incoming), main = 'Greenpoint') +
  geom_point(shape=19) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 1)
p4 + ggtitle("GreenPoint")+
  labs(y="# incoming trips at afternoon rush hour (transformed)",
       x="# incoming trips at morning rush hour (transformed)") +
  theme(axis.title = element_text(size=14), legend.text = element_text(size=12),
        legend.title = element_text(size=14)) 






############################ Figures #########################################
library(ggplot2)
library(wesanderson)
### Plot of model1
p1 <- ggplot(Data_pred_in, aes(x=morning_outgoing, y=afternoon_incoming, colour = month)) +
  geom_point(shape=19) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 1)
# Use brewer color palettes
p1 + scale_color_brewer(palette="Set1") +
  labs(y="# incoming trips at afternoon rush hour (transformed)",
       x="# outgoing trips at morning rush hour (transformed)") +
  theme(axis.title = element_text(size=14), legend.text = element_text(size=12),
       legend.title = element_text(size=14)) 


### Plot of model comparision for a region
### visualize the comparison of the ts model and model 4
# compute prediction and CI from model 4
model_1 <- lm(afternoon_incoming ~ morning_outgoing, data = month7)
Aug_pred_1 <- predict(model_1, new = month8, interval = c("prediction"))

ts_pred_result <- as.numeric(ts_pred_result)^5
lm_pred_result <- Aug_pred_1[month8$neighborhood == region, 1]^5
ts_CI_lower <- as.numeric(ts_CI_lower)^5
ts_CI_upper <- as.numeric(ts_CI_upper)^5
lm_CI_lower <- Aug_pred_1[month8$neighborhood == region, 2]^5
lm_CI_upper <- Aug_pred_1[month8$neighborhood == region, 3]^5


p2 <- qplot(x=c(1:31), y= (month8[month8$neighborhood ==region, ]$afternoon_incoming)^5, 
            main = region) +
  geom_line(aes(y=ts_pred_result, group = 1, color = 'Time Series Model'), size = 1) +
  geom_line(aes(y=lm_pred_result, group = 1, color = 'Regression Model'), size = 1) +
  geom_ribbon(aes(ymin=ts_CI_lower, ymax=ts_CI_upper),fill = "cyan",  alpha = 0.1) + 
  geom_ribbon(aes(ymin=lm_CI_lower, ymax=lm_CI_upper), fill = 'red', alpha = 0.1)

p2 + labs(x="Day of Month", y="# incoming trips at afternoon rush hour") +
  theme(axis.title = element_text(size=14), legend.text = element_text(size=12),
        legend.title = element_text()) 


### Plot total trips per hour
# read in the organized data
Data <- read.csv("organized_data.csv")
head(Data)

# visualize total trips per hour ######
income_data <- Data[Data$trip_type == "incoming", ]
total_trips <- tapply(income_data$trip_number, income_data$hour, sum)
# organize into a dataframe
total_trips <- data.frame(total_trips)
total_trips$time <- c(0:23)

ggplot(data = total_trips, aes(x=time, y=total_trips, group=1)) +
  geom_line() + 
  geom_point(shape=19)+
  labs(x="Time of Day (hour)",
       y="Total number of trips") +
  theme(axis.title = element_text(size=14)) 


### Plot of model1 by regions
p1 <- ggplot(Data_pred_in, aes(x=morning_outgoing, y=afternoon_incoming, colour = neighborhood)) +
  geom_point(shape=19) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, size = 1)
# Use brewer color palettes
p1 + scale_color_brewer(palette="Set1") +
  labs(y="# incoming trips at afternoon rush hour (transformed)",
       x="# outgoing trips at morning rush hour (transformed)") +
  theme(axis.title = element_text(size=14), legend.text = element_text(size=12),
        legend.title = element_text(size=14)) 