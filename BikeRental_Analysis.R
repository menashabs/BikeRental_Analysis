######################################
######################################
## Project - BikeRental

# loading the libraries
library(tidyverse)
library(broom)
library(GGally)
library(ggplot2)
library(readxl)
library(olsrr)
library(MASS)
library(modelr)

# Analysis
data_2011 <- read_xlsx("BikeRental_Dataset.xlsx")
data_2011

data_2011 %>%  ggplot(aes(x=record_index, y=Y)) + 
  geom_point(col="dark blue", shape=15) +
  ggtitle("Figure 3.2.1: Visualizing the behaviour of Count of total rental bikes")


data_2011 %>% select(Y,X1,X4) %>%
  ggpairs(title="Figure 3.2.2: Exploration of selected quantitative variables")

data_2011 %>% ggpairs(columns=c("Y","X1","X4"), 
                      mapping=aes(color=as.factor(X5), alpha=0.5), 
                      title="Figure 3.2.3: Exploration of selected quantitative variables by weather situation")

data_2011 %>% ggpairs(columns=c("Y","X1","X4"), 
                      mapping=aes(color=as.factor(X6), alpha=0.5),
                      title="Figure 3.2.4: Exploration of selected quantitative variables by season")

data_2011 %>% ggpairs(columns=c("Y","X1","X4"), 
                      mapping=aes(color=as.factor(X7), alpha=0.5),
                      title="Figure 3.2.5: Exploration of selected quantitative variables by working day")


# using only x1,x4,x5,x6,x7


##########

model1 <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6)+as.factor(X7), data_2011)
summary(model1)

final_model <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6), data=data_2011)
final_model
summary(final_model)

ols_plot_resid_qq(final_model)

ols_plot_resid_hist(final_model)

ols_plot_resid_fit(final_model)

ols_test_normality(final_model)

ols_vif_tol(model1)

boxcox_values <- boxcox(final_model)
boxcox_lambda <- boxcox_values$x[which.max(boxcox_values$y)]
boxcox_lambda

ols_vif_tol(final_model)

data_2011$factorX5 <- as.factor(data_2011$X5)
data_2011$factorX6 <- as.factor(data_2011$X6)

final_model_residual_analysis <- lm(Y~X1+X4+factorX5+factorX6, data=data_2011)

model_fitresid <- augment(final_model_residual_analysis)
model_fitresid

ggplot(model_fitresid, 
       aes(x=.fitted, y=.resid)) + geom_point() + geom_hline(yintercept=0)

ggplot(model_fitresid, 
       aes(x=X1, y=.resid)) + geom_point() + geom_hline(yintercept=0)

ggplot(model_fitresid, 
       aes(x=X4, y=.resid)) + geom_point() + geom_hline(yintercept=0)

ggplot(model_fitresid, 
       aes(sample=.resid)) +
  stat_qq() + stat_qq_line()

boxcox_values <- boxcox(final_model)
boxcox_lambda <- boxcox_values$x[which.max(boxcox_values$y)]
boxcox_lambda

# Studentized residuals (internally studentized residuals)
# r_i
ols_plot_resid_stand(final_model)

# Studentized Deleted residual plot (bar)
ols_plot_resid_stud(final_model)

#Deleted Studentized Residual vs Fitted Values Plot
ols_plot_resid_stud_fit(final_model) 

#Cooks Distance (Bar chart)
ols_plot_cooksd_bar(final_model)

#Cooks Distance (line chart)
ols_plot_cooksd_chart(final_model)

# DFBETSs panel
ols_plot_dfbetas(final_model)

# DFITS
ols_plot_dffits(final_model)


# after removing values taken from cooks distance

new_data <- data_without_influencial_cases

final_model_new <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6), data=new_data)
final_model_new
summary(final_model_new)

anova(final_model_new)

ols_hsp(final_model_new)

ols_fpe(final_model_new)


ols_plot_resid_qq(final_model_new)

ols_plot_resid_hist(final_model_new)

ols_plot_resid_fit(final_model_new)

ols_test_normality(final_model_new)

ols_vif_tol(model1)

ols_plot_obs_fit(final_model)
ols_plot_obs_fit(final_model_new)

plot(model_fitresid$.fitted, model_fitresid$Y) abline (final_model, final_model_new)
# after removing values outliers

new_data_2 <- data_without_outliers

final_model_new_2 <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6), data=new_data_2)
final_model_new_2
summary(final_model_new_2)

anova(final_model_new_2)

ols_plot_resid_qq(final_model_new_2)

ols_plot_resid_hist(final_model_new_2)

ols_plot_resid_fit(final_model_new_2)

ols_test_normality(final_model_new)

ols_vif_tol(final_model_new)

# heteroscadasticity

residuals<-resid(final_model_residual_analysis)

fitted_values <- fitted(final_model_residual_analysis)

data_y <- data.frame(residuals, fitted_values)

ggplot(data_y, aes(x=fitted_values, y=(residuals)^2)) + geom_point() 

ggplot(model_fitresid, 
       aes(x=.fitted, y=(.resid)^2)) + geom_point()

ggplot(model_fitresid, 
       aes(x=X1, y=(.resid)^2)) + geom_point()

ggplot(model_fitresid, 
       aes(x=X4, y=(.resid)^2)) + geom_point()

# autocorrelation

observation_number <- data_2011 %>% pull(record_index)

d1 <- data.frame(observation_number, residuals)

ggplot(d1, 
       aes(x=observation_number, y=residuals)) + geom_point()


ols_hsp(final_model)

ols_fpe(final_model)

ols_plot_obs_fit(final_model)

ols_test_score(final_model)
anova(final_model)

ols_plot_comp_plus_resid(final_model)

####

# validation 

data_2012 <- data_used_model_validating
data_2012

data_2012$factorX5 <- as.factor(data_2012$X5)
data_2012$factorX6 <- as.factor(data_2012$X6) 

# prediction
predictions <- predict(final_model, data_2012)
predictions

length(predictions)

RECORD_INDEX <- data_2012$RECORD_INDEX

predictions_data <- data.frame(RECORD_INDEX, predictions)

data_validation <- left_join(data_2012, predictions_data, by="RECORD_INDEX")
data_validation

calculating_mspr <- data_validation %>% mutate((Y-predictions)^2)

mspr <- (sum(calculating_mspr$`(Y - predictions)^2`))/336
mspr

ols_msep(final_model_new)

summary(final_model_new)

ols_pred_rsq(final_model)


data_2012 <- data_used_model_validating

validate <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6), data=data_2012)
validate
summary(validate)
anova(validate)
ols_hsp(validate)
ols_fpe(validate)

final_model <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6), data=data_2011)
final_model
summary(final_model)
anova(final_model)
 
data_rr <- data_used_model_validating_removing_1_
validaterr <- lm(Y~X1+X4+as.factor(X5)+as.factor(X6), data=data_rr)
validaterr
summary(validaterr)
anova(validaterr)
ols_hsp(validaterr)
ols_fpe(validaterr)

# final model
ols_hsp(final_model)
ols_hsp(validate)

ols_fpe(final_model)

data_2012 %>% ggpairs(columns=c("Y","X1","X4"), 
                      mapping=aes(color=as.factor(X5), alpha=0.5), 
                      title="For Test Set, Exploration of selected quantitative variables by weather situation")


data_2012 %>%  ggplot(aes(x=record_index, y=Y)) + 
  geom_point(col="red", shape=15) +
  ggtitle("For the test set Visualizing the behaviour of Count of total rental bikes")




