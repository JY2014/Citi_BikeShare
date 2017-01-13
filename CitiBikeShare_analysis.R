setwd("C:/Users/JY/Documents/GitHub/Stat139_project")

#
################    Project: Hypothesis 1    ###################
# Exploring which factors are important in affecting the imbalance


### load the organized data
data2 <- read.csv('organized_data_2.csv')
names(data2)[names(data2) == "normalized_imbalance"] <- "norm_im"
attach(data2)


########                                   ########
###                Transformation               ###
########                                   ########

## exploring the distribution of the response variable
qqnorm(norm_im)
qqline(norm_im)
breaks = seq(min(norm_im)-0.05, max(norm_im)+0.05, 0.01)
hist(norm_im, breaks)

## sqaure-root transformation
data2$sqrt_norm_im <- rep(0, nrow(data2))
# transform the absolute values and assign the sign
data2$sqrt_norm_im[data2$norm_im>0] <- sqrt(data2$norm_im[data2$norm_im>0])
data2$sqrt_norm_im[data2$norm_im<0] <- -sqrt(-data2$norm_im[data2$norm_im<0])
# check the distribution
breaks_sqrt = seq(min(data2$sqrt_norm_im)-0.05, max(data2$sqrt_norm_im)+0.05, 0.05)
hist(data2$sqrt_norm_im, breaks_sqrt)
qqnorm(data2$sqrt_norm_im)
qqline(data2$sqrt_norm_im)


########                                       ########
###        Boxplot of different neighborhood        ###
########                                       ########

## exclude five neighbors with too few observations
data2 <- data2[(data2$neighborhood != 'Sunset Park') 
              & (data2$neighborhood != 'Hudson') 
              & (data2$neighborhood != 'Highlands') 
              & (data2$neighborhood !='Canarsie and Flatlands') 
              & (data2$neighborhood != 'Central Harlem'),]

# order the regions
data2$neighborhood <- factor(data2$neighborhood, 
                             levels = c("Lower East Side", "Lower Manhattan", 
                                        "Bushwick and Williamsburg", "Central Brooklyn",
                                        "Chelsea and Clinton", "East Harlem", 
                                        "Gramercy Park and Murray Hill", "Greenpoint",
                                        "Greenwich Village and Soho", "Northwest Brooklyn", 
                                        "Northwest Queens", "Upper East Side",
                                        "Upper West Side"))



########                                                   ########
###     ESS test for significant effect of important factors    ###
########                                                   ########

### Weighted linear regression
# simple linear regression to get residuals
model_0 <- lm(sqrt_norm_im~neighborhood, data = data2)
# assign weight as inverse variance of each region
region_list <- unique(data2$neighborhood)
data2$resid <- resid(model_0)

for (region in region_list){
  data_region <- data2[data2$neighborhood == region,]
  data2$weight[data2$neighborhood == region] <- 1/var(data_region$resid)
}

# weighted regression with additional factors
# adding one factor at a time
model_1 <- lm(sqrt_norm_im~ 1, weights = weight, data = data2) 
model_2 <- lm(sqrt_norm_im~hour, weights = weight,  data = data2) 
model_3 <- lm(sqrt_norm_im~neighborhood+as.factor(hour), weights = weight,  
              data = data2)
model_4 <- lm(sqrt_norm_im~neighborhood*as.factor(hour), weights = weight,  
              data = data2)
model_5 <- lm(sqrt_norm_im~neighborhood*as.factor(hour)+day_of_week, 
              weights = weight,  data = data2)
model_6 <- lm(sqrt_norm_im~neighborhood*as.factor(hour)+month+day_of_week,  
              data = data2)

# test the effect of neighborhood
anova(model_1, model_2)
# test the effect of hour
anova(model_2, model_3)
# test the effect of interaction
anova(model_3, model_4)


### compare residual of model-3 and 5
par(mfrow = c(1, 2))
qqnorm(model_5$residuals, main = 'Q-Q plot of residuals from unweighted regression')
qqline(model_5$residuals, main = 'Q-Q plot of residuals from unweighted regression')

qqnorm(resid(model_3), main = 'Q-Q plot of residuals from weighted regression')
qqline(model_3$residuals, main = 'Q-Q plot of residuals from weighted regression')



#
################    Project: Hypothesis 2    ###################
###              Predicting rush hour trips 

### Load the data
Data <- read.csv("organized_data_3.csv")
head(Data)


########             ########
###     Data cleaning     ###
########             ########

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

# store into a new dataframe
Data_pred_out <- Data_regions_rm['afternoon_outgoing']
Data_pred_out$afternoon_outgoing <- (Data_regions_rm$afternoon_outgoing)^(1/5)


### Check predictor variables
## morning_outgoing and morning_incoming
# transformation
Data_pred_out$morning_incoming <- Data_pred_in$morning_incoming <- 
  (Data_regions_rm$morning_incoming)^(1/5)
Data_pred_out$morning_outgoing <- Data_pred_in$morning_outgoing <- 
  (Data_regions_rm$morning_outgoing)^(1/5)

#pairs(Data_pred_out)

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


########                                                    ########
###    Model selection on predicting afternoon incoming trips    ###
########                                                    ########
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


########                        ########
###      try time series models      ###
########                        ########

### use 'Greenpoint' as an example
## visualize: 
ts.plot(Data_pred_in$afternoon_incoming[Data_regions_rm$neighborhood == 'Greenpoint'])
ts.plot(Data_pred_out$afternoon_outgoing[Data_regions_rm$neighborhood == 'Greenpoint'])

## combine them together with morning data
ts_incoming <- cbind(matrix(Data_pred_in$morning_outgoing[Data_pred_in$neighborhood == 'Greenpoint']), 
                     matrix(Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == 'Greenpoint']))
ts_incoming <- as.vector(t(ts_incoming))

## visualize
ts.plot(ts_incoming)
# need to use difference?
ts_incoming_diff <- ts_incoming[2:length(ts_incoming)] - ts_incoming[1:length(ts_incoming)-1]
ts.plot(ts_incoming_diff)
#acf(ts_incoming_diff)
#pacf(ts_incoming_diff)

## use KPSS test for stationarity:
library(tseries)
kpss.test(ts_incoming) # stationary

## try auto.arima()
library(forecast)
result <- auto.arima(ts_incoming)
plot(forecast(result, h=20))
forecast(result)$mean[1]


######### predict the data in Aug using a 30-day moving window #########
region = 'Greenpoint'

morning_data <- Data_pred_in$morning_outgoing[Data_pred_in$neighborhood == region]
afternoon_data <- Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == region]

# there are 31 days in both July and Aug
i_start <- 31 
# lists to store the results
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


## this is model_10
RSS_model_10 <- list()
for (region in region_list){
  #RSS of model 
  RSS <- sum((month8$afternoon_incoming[month8$neighborhood == region]^5 - 
                Aug_pred_10[month8$neighborhood == region]^5)^2)
  RSS_model_10 <- c(RSS_model_10, RSS)
}

# compare the results of time series model and linear model for each region
ts_comparison <- cbind(as.numeric(ts_RSS_list)/31, as.numeric(RSS_model_1)/31, as.numeric(RSS_model_10)/31)
region_list[as.numeric(ts_RSS_list) > as.numeric(RSS_model_10)]
region_list[as.numeric(ts_RSS_list) < as.numeric(RSS_model_10)]



### using model 1 to predict Greenpoint only
month7_GP <- month7[month7$neighborhood == 'Greenpoint', ]
model1_2 <- lm(afternoon_incoming ~ morning_outgoing + morning_incoming, 
               data = month7_GP)

#
#### visualization
#
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



########                                 ########
###          Figures for the report           ###
########                                 ########
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
=======
setwd("C:/Users/JY/Documents/GitHub/Stat139_project")

#
################    Project: Hypothesis 1    ###################
# Exploring which factors are important in affecting the imbalance


### load the organized data
data2 <- read.csv('organized_data_2.csv')
names(data2)[names(data2) == "normalized_imbalance"] <- "norm_im"
attach(data2)


########                                   ########
###                Transformation               ###
########                                   ########

## exploring the distribution of the response variable
qqnorm(norm_im)
qqline(norm_im)
breaks = seq(min(norm_im)-0.05, max(norm_im)+0.05, 0.01)
hist(norm_im, breaks)

## sqaure-root transformation
data2$sqrt_norm_im <- rep(0, nrow(data2))
# transform the absolute values and assign the sign
data2$sqrt_norm_im[data2$norm_im>0] <- sqrt(data2$norm_im[data2$norm_im>0])
data2$sqrt_norm_im[data2$norm_im<0] <- -sqrt(-data2$norm_im[data2$norm_im<0])
# check the distribution
breaks_sqrt = seq(min(data2$sqrt_norm_im)-0.05, max(data2$sqrt_norm_im)+0.05, 0.05)
hist(data2$sqrt_norm_im, breaks_sqrt)
qqnorm(data2$sqrt_norm_im)
qqline(data2$sqrt_norm_im)


########                                       ########
###        Boxplot of different neighborhood        ###
########                                       ########

## exclude five neighbors with too few observations
data2 <- data2[(data2$neighborhood != 'Sunset Park') 
              & (data2$neighborhood != 'Hudson') 
              & (data2$neighborhood != 'Highlands') 
              & (data2$neighborhood !='Canarsie and Flatlands') 
              & (data2$neighborhood != 'Central Harlem'),]

# order the regions
data2$neighborhood <- factor(data2$neighborhood, 
                             levels = c("Lower East Side", "Lower Manhattan", 
                                        "Bushwick and Williamsburg", "Central Brooklyn",
                                        "Chelsea and Clinton", "East Harlem", 
                                        "Gramercy Park and Murray Hill", "Greenpoint",
                                        "Greenwich Village and Soho", "Northwest Brooklyn", 
                                        "Northwest Queens", "Upper East Side",
                                        "Upper West Side"))



########                                                   ########
###     ESS test for significant effect of important factors    ###
########                                                   ########

### Weighted linear regression
# simple linear regression to get residuals
model_0 <- lm(sqrt_norm_im~neighborhood, data = data2)
# assign weight as inverse variance of each region
region_list <- unique(data2$neighborhood)
data2$resid <- resid(model_0)

for (region in region_list){
  data_region <- data2[data2$neighborhood == region,]
  data2$weight[data2$neighborhood == region] <- 1/var(data_region$resid)
}

# weighted regression with additional factors
# adding one factor at a time
model_1 <- lm(sqrt_norm_im~ 1, weights = weight, data = data2) 
model_2 <- lm(sqrt_norm_im~hour, weights = weight,  data = data2) 
model_3 <- lm(sqrt_norm_im~neighborhood+as.factor(hour), weights = weight,  
              data = data2)
model_4 <- lm(sqrt_norm_im~neighborhood*as.factor(hour), weights = weight,  
              data = data2)
model_5 <- lm(sqrt_norm_im~neighborhood*as.factor(hour)+day_of_week, 
              weights = weight,  data = data2)
model_6 <- lm(sqrt_norm_im~neighborhood*as.factor(hour)+month+day_of_week,  
              data = data2)

# test the effect of neighborhood
anova(model_1, model_2)
# test the effect of hour
anova(model_2, model_3)
# test the effect of interaction
anova(model_3, model_4)


### compare residual of model-3 and 5
par(mfrow = c(1, 2))
qqnorm(model_5$residuals, main = 'Q-Q plot of residuals from unweighted regression')
qqline(model_5$residuals, main = 'Q-Q plot of residuals from unweighted regression')

qqnorm(resid(model_3), main = 'Q-Q plot of residuals from weighted regression')
qqline(model_3$residuals, main = 'Q-Q plot of residuals from weighted regression')



#
################    Project: Hypothesis 2    ###################
###              Predicting rush hour trips 

### Load the data
Data <- read.csv("organized_data_3.csv")
head(Data)


########             ########
###     Data cleaning     ###
########             ########

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

# store into a new dataframe
Data_pred_out <- Data_regions_rm['afternoon_outgoing']
Data_pred_out$afternoon_outgoing <- (Data_regions_rm$afternoon_outgoing)^(1/5)


### Check predictor variables
## morning_outgoing and morning_incoming
# transformation
Data_pred_out$morning_incoming <- Data_pred_in$morning_incoming <- 
  (Data_regions_rm$morning_incoming)^(1/5)
Data_pred_out$morning_outgoing <- Data_pred_in$morning_outgoing <- 
  (Data_regions_rm$morning_outgoing)^(1/5)

#pairs(Data_pred_out)

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


########                                                    ########
###    Model selection on predicting afternoon incoming trips    ###
########                                                    ########
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


########                        ########
###      try time series models      ###
########                        ########

### use 'Greenpoint' as an example
## visualize: 
ts.plot(Data_pred_in$afternoon_incoming[Data_regions_rm$neighborhood == 'Greenpoint'])
ts.plot(Data_pred_out$afternoon_outgoing[Data_regions_rm$neighborhood == 'Greenpoint'])

## combine them together with morning data
ts_incoming <- cbind(matrix(Data_pred_in$morning_outgoing[Data_pred_in$neighborhood == 'Greenpoint']), 
                     matrix(Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == 'Greenpoint']))
ts_incoming <- as.vector(t(ts_incoming))

## visualize
ts.plot(ts_incoming)
# need to use difference?
ts_incoming_diff <- ts_incoming[2:length(ts_incoming)] - ts_incoming[1:length(ts_incoming)-1]
ts.plot(ts_incoming_diff)
#acf(ts_incoming_diff)
#pacf(ts_incoming_diff)

## use KPSS test for stationarity:
library(tseries)
kpss.test(ts_incoming) # stationary

## try auto.arima()
library(forecast)
result <- auto.arima(ts_incoming)
plot(forecast(result, h=20))
forecast(result)$mean[1]


######### predict the data in Aug using a 30-day moving window #########
region = 'Greenpoint'

morning_data <- Data_pred_in$morning_outgoing[Data_pred_in$neighborhood == region]
afternoon_data <- Data_pred_in$afternoon_incoming[Data_pred_in$neighborhood == region]

# there are 31 days in both July and Aug
i_start <- 31 
# lists to store the results
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


## this is model_10
RSS_model_10 <- list()
for (region in region_list){
  #RSS of model 
  RSS <- sum((month8$afternoon_incoming[month8$neighborhood == region]^5 - 
                Aug_pred_10[month8$neighborhood == region]^5)^2)
  RSS_model_10 <- c(RSS_model_10, RSS)
}

# compare the results of time series model and linear model for each region
ts_comparison <- cbind(as.numeric(ts_RSS_list)/31, as.numeric(RSS_model_1)/31, as.numeric(RSS_model_10)/31)
region_list[as.numeric(ts_RSS_list) > as.numeric(RSS_model_10)]
region_list[as.numeric(ts_RSS_list) < as.numeric(RSS_model_10)]



### using model 1 to predict Greenpoint only
month7_GP <- month7[month7$neighborhood == 'Greenpoint', ]
model1_2 <- lm(afternoon_incoming ~ morning_outgoing + morning_incoming, 
               data = month7_GP)

#
#### visualization
#
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



########                                 ########
###          Figures for the report           ###
########                                 ########
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