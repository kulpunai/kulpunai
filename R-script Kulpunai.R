#Kulpunai Kurstanbek kyzy
#Statistical data Analysis 
#project R-script 

library(tidyverse)
library(caret)
library(plotly)
library(data.table)
library(GGally)
library(car)
library(scales)
library(lmtest)
library(ggplot2)
library(performance)
library(MLmetrics)
library(rmdformats)

#Data wrangling 
fish <- fish %>%
  select(-Species) 

#check data type
glimpse(fish)

#check missing values
anyNA(fish)

#EDA
#correlation
ggcorr(fish, label = T, label_size = 3, hjust = 1, layout.exp = 2)

#cross validation
set.seed(100)
index <- sample(nrow(fish), nrow(fish)*0.7)

fish_train <- fish[index,]
fish_test <- fish[-index,]


#modelling
set.seed(100)
model_init <- lm(formula = Weight ~ Width, data = fish_train)
summary(model_init)


model_null <- lm(formula = Weight ~ 1, data = fish_train)
summary(model_null)

model_all <- lm(formula = Weight ~ ., data = fish_train)
summary(model_all)

# Create formula using Step-Wise Regression
step(object = model_all, direction = "backward", trace = F)

step(object = model_null, direction = "forward", scope = list(lower = model_null, upper = model_all), trace = F)

step(object = model_null, direction = "both", scope = list(lower = model_null, upper = model_all), trace = F)

# Create the model

model_step_back <- lm(formula = Weight ~ Length1 + Length3 + Height, data = fish_train)
model_step_forw <- lm(formula = Weight ~ Length3 + Width + Height + Length1, data = fish_train)
model_step_both <- lm(formula = Weight ~ Length3 + Height + Length1, data = fish_train)

# Compare these models with `model_all`
compare_performance(model_all, model_step_back, model_step_forw, model_step_both)

#evaluation
fish_pred <- predict(object = model_step_both, newdata = fish_test, level = 0.95)

# RMSE of train dataset
RMSE(y_pred = model_step_both$fitted.values, y_true = fish_train$Weight)
# RMSE of test dataset
RMSE(y_pred = fish_pred, y_true = fish_test$Weight)

#Assumption checking


#normality test
shapiro.test(model_step_both$residuals)
# Histogram
hist(model_step_both$residuals)
# Density Plot
plot(density(model_step_both$residuals))

#Homoscedasticity Test
bptest(model_step_both)
#visualize
plot(model_step_both$fitted.values, model_step_both$residuals)
abline(h = 0, col = "red")

#Multicolinearity test
vif(model_step_both)

#Data and model tuning
model_step_both
#correlation again
ggcorr(fish, label = T, label_size = 3, hjust = 1, layout.exp = 2)
#outliers in fish data
boxplot(fish)

#removing outliers
fish_new <- fish %>%
  filter(Weight < 1500) %>%
  select(-Length1, -Length2)

set.seed(100)
index_new <- sample(nrow(fish_new), nrow(fish_new)*0.7)

fish_train_new <- fish_new[index_new,]
fish_test_new <- fish_new[-index_new,]

model_init_new <- lm(formula = Weight ~ Width, data = fish_train_new)
model_null_new <- lm(formula = Weight ~ 1, data = fish_train_new)
model_all_new <- lm(formula = Weight ~ ., data = fish_train_new)

compare_performance(model_init_new, model_null_new, model_all_new)


#step wise regression
step(object = model_all_new, direction = "backward", trace = F)

step(object = model_null_new, direction = "forward", scope = list(lower = model_null_new, upper = model_all_new), trace = F)

step(object = model_null_new, direction = "both", scope = list(lower = model_null_new, upper = model_all_new), trace = F)
model_step_back_new <- lm(formula = Weight ~ Length3 + Height + Width, data = fish_train_new)
model_step_forw_new <- lm(formula = Weight ~ Length3 + Width + Height, data = fish_train_new)
model_step_both_new <- lm(formula = Weight ~ Length3 + Width + Height, data = fish_train_new)


compare_performance(model_step_back_new, model_step_both_new, model_step_forw_new, model_all_new)


fish_pred_new <- predict(object = model_step_both_new, newdata = fish_test_new, level = 0.95)

# RMSE of train_rev dataset
RMSE(y_pred = model_step_both_new$fitted.values, y_true = fish_train_new$Weight)

# RMSE of test_rev dataset
RMSE(y_pred = fish_pred_new, y_true = fish_test_new$Weight)

#assumption checking
shapiro.test(model_step_both_new$residuals)

bptest(model_step_both_new)

vif(model_step_both_new)


 












