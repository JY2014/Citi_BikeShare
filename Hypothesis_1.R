#setwd("~/Dropbox/stats139/project")
# install.packages("reshape")
#install.packages("plyr")
library(plyr)
library(ggplot2)
library(cvTools)
library(MASS)
library(reshape)

data2 <- read.csv('organized_data_2.csv')

#### data2
names(data2)[names(data2) == "normalized_imbalance"] <- "norm_im"
qqnorm(norm_im)
qqline(norm_im)
breaks = seq(min(norm_im)-0.05, max(norm_im)+0.05, 0.01)
hist(norm_im, breaks)


##transform square root
data2$sqrt_norm_im <- rep(0, nrow(data2))
data2$sqrt_norm_im[data2$norm_im>0] <- sqrt(data2$norm_im[data2$norm_im>0])
data2$sqrt_norm_im[data2$norm_im<0] <- -sqrt(-data2$norm_im[data2$norm_im<0])
breaks_sqrt = seq(min(data2$sqrt_norm_im)-0.05, max(data2$sqrt_norm_im)+0.05, 0.05)
hist(data2$sqrt_norm_im, breaks_sqrt)
qqnorm(data2$sqrt_norm_im)
qqline(data2$sqrt_norm_im)

###############
####Boxplot of different neighborhood
###############

# ## exclude five neighbors with too few observations
# 
data2 <-
  data2[(data2$neighborhood != 'Sunset Park') & (data2$neighborhood != 'Hudson') & (data2$neighborhood != 'Highlands')
        & (data2$neighborhood !='Canarsie and Flatlands') & (data2$neighborhood != 'Central Harlem'),]

data2$neighborhood <- factor(data2$neighborhood, levels = c("Lower East Side", "Lower Manhattan", "Bushwick and Williamsburg", "Central Brooklyn",
                                                                                                "Chelsea and Clinton", "East Harlem", "Gramercy Park and Murray Hill", "Greenpoint",
                                                                                                "Greenwich Village and Soho", "Northwest Brooklyn", "Northwest Queens", "Upper East Side",
                                                                                                "Upper West Side"))
head(data2)

model_0 <- lm(sqrt_norm_im~neighborhood, data = data2)

# assign weight as inverse variance of each region
region_list <- unique(data2$neighborhood)
data2$resid <- resid(model_0)

for (region in region_list){
  data_region <- data2[data2$neighborhood == region,]
  data2$weight[data2$neighborhood == region] <- 1/var(data_region$resid)
}

#aov_1 <- aov(sqrt_norm_im~neighborhood*as.factor(hour), data = data2)
model_1 <- lm(sqrt_norm_im~ 1, weights = weight, data = data2) 
model_2 <- lm(sqrt_norm_im~hour, weights = weight,  data = data2) 
model_3 <- lm(sqrt_norm_im~neighborhood+as.factor(hour), weights = weight,  data = data2)
model_4 <- lm(sqrt_norm_im~neighborhood*as.factor(hour), weights = weight,  data = data2)
model_5 <- lm(sqrt_norm_im~neighborhood*as.factor(hour)+day_of_week, weights = weight,  data = data2)

# test the effect of neighborhood
anova(model_1, model_2)
# test the effect of hour
anova(model_2, model_3)
# test the effect of interaction
anova(model_3, model_4)

vars=by(resid(model_1),data2$neighborhood,var)
vars <- as.numeric(vars)

model_6 <- lm(sqrt_norm_im~neighborhood*as.factor(hour)+month+day_of_week,  data = data2)


# compare residual of model-3 and 5
par(mfrow = c(1, 2))
qqnorm(model_5$residuals, main = 'Q-Q plot of residuals from unweighted regression')
qqline(model_5$residuals, main = 'Q-Q plot of residuals from unweighted regression')

qqnorm(resid(model_3), main = 'Q-Q plot of residuals from weighted regression')
qqline(model_3$residuals, main = 'Q-Q plot of residuals from weighted regression')
