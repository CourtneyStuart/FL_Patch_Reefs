#### SET-UP ####

# set working directory (home project folder)
setwd("E:/BIOL398_Patch_Reef_Residency/")

# associated github repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reefs

# data directories
# prepped data from github
git_data = "E:/BIOL398_Patch_Reef_Residency/GitHub/FL_Patch_Reefs/Data/"

# temporary directory (for intermediate/temporary products or checks)
temp_wd = "E:/BIOL398_Patch_Reef_Residency/Temporary/"

# for saving figures to github
git_figs = "E:/BIOL398_Patch_Reef_Residency/GitHub/FL_Patch_Reefs/Figures/"

# install packages
#install.packages("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
#                "dplyr", "ISLR", "caret", "MLeval")

# load packages
library(easypackages) # lets you load more than one package at once
libraries("conflicted", "tidyverse", "ggplot2", "PNWColors", "readxl", 
          "dplyr", "ISLR", "caret", "MLeval")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# set seed for reproducibility
set.seed(123)

#### Standard Logistic Regressions using glm() ####

#### Subadult Gray Snapper (Lutjanus griseus) ####
# read in the model calibration and validation subsets
lg_train = read.csv(paste0(git_data, "Subadult_Gray_Snapper_Model_Calibration_Data.csv"))
lg_test = read.csv(paste0(git_data, "Subadult_Gray_Snapper_Model_Validation_Data.csv"))

# we first need to make sure that our response variable (Presence2) is stored
# as a factor in both the training and testing subsets
class(lg_train$Presence2)
class(lg_test$Presence2)

# now check the levels of the response variable
levels(lg_train$Presence2)
levels(lg_test$Presence2)

# for modeling, we only need the response variable and the predictors
lg_train = lg_train %>%
  select(Presence2, 12:25) # where 12:25 is the range of predictors

lg_test = lg_test %>%
  select(Presence2, 12:25)

# let's use density plots to check out how the density distribution of our 
# response varies across our predictors. our hypothesis is that several of these
# spatial and environmental predictors interact to drive the distributions
# of sub-adult fishes on patch reefs. therefore, we should see decent overlap in
# our response values for each variable.
require(caret)
lg_x = lg_train[,2:15] # predictors
lg_y = lg_train[,1] # response
scales = list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = lg_x, y = lg_y, plot = "density", scales = scales)

# based on the overlapping responses, it would be very challenging to predict 
# presence/absence with just one predictor

# we can now fit a logistic regression to our data. logistic regression is a way
# of measuring the relationship between a categorical dependent variable and one
# or more independent (predictor) variables by estimating probabilities using a 
# logistic function.
lg_glm = glm(Presence2 ~ ., data = lg_train, family = binomial(link = "logit"))

# check out the results
summary(lg_glm)

# use the fitted model to predict the probability of presence across the witheld
# test data
lg_probs = predict(lg_glm, lg_test, type = "response")

# convert these probability predictions to a binary presence/absence response
# using a standard threshold of 0.5 for the probability of presence 
lg_pred = as.factor(ifelse(lg_probs >= 0.5, "PRESENCE", "ABSENCE"))

# now calculate a confusion matrix comparing our fitted model predictions to the 
# actual (known) species presence/absence at the witheld validation sites
lg_glm_CM = confusionMatrix(lg_pred, lg_test$Presence2,  positive = "PRESENCE") 

# check out the results, where accuracy is the overall accuracy of predictions,
# sensitivity is the percentage of correctly predicted presences, and specificity
# is the percentage of correctly predicted absences
lg_glm_CM 

#### Subadult Bluestriped Grunt (Haemulon sciurus) ####
hs_train = read.csv(paste0(git_data, "Subadult_Bluestriped_Grunt_Model_Calibration_Data.csv"))
hs_test = read.csv(paste0(git_data, "Subadult_Bluestriped_Grunt_Model_Validation_Data.csv"))

# we first need to make sure that our response variable (Presence2) is stored
# as a factor in both the training and testing subsets
class(hs_train$Presence2)
class(hs_test$Presence2)

# now check the levels of the response variable
levels(hs_train$Presence2)
levels(hs_test$Presence2)

# for modeling, we only need the response variable and the predictors
hs_train = hs_train %>%
  select(Presence2, 12:25) # where 12:25 is the range of predictors

hs_test = hs_test %>%
  select(Presence2, 12:25)

# examine density plots for bluestriped grunt
require(caret)
hs_x = hs_train[,2:15] # predictors
hs_y = hs_train[,1] # response
scales = list(x = list(relation = "free"), y = list(relation= "free"))
featurePlot(x = hs_x, y = hs_y, plot = "density", scales = scales)

# based on the overlapping responses, it would be very challenging to predict 
# presence/absence with just one predictor

# we can now fit a logistic regression to our training data
hs_glm = glm(Presence2 ~ ., data = hs_train, family = binomial(link = "logit"))

# check out the results
summary(hs_glm)

# use the fitted model to predict the probability of presence across the withheld
# test data
hs_probs = predict(hs_glm, hs_test, type = "response")

# convert these probability predictions to a binary presence/absence response
# using a standard threshold of 50%
hs_pred = as.factor(ifelse(hs_probs >= 0.5, "PRESENCE", "ABSENCE"))

# now calculate a confusion matrix comparing our fitted model predictions to the 
# actual (known) species presence/absence at the withheld validation sites
hs_glm_CM = confusionMatrix(hs_pred, hs_test$Presence2,  positive = "PRESENCE") 

# check out the results, where accuracy is the overall accuracy of predictions,
# sensitivity is the percentage of correctly predicted presences, and specificity
# is the percentage of correctly predicted absences
hs_glm_CM 

#### 10-fold CV Logistic Regressions using caret() ####

# set up caret command: 10-fold cross-validation, two class response (presence
# or absence), calculate class probabilities, and save predictions
trainControl = trainControl(method = "cv", number = 10,
                            summaryFunction = twoClassSummary, 
                            classProbs = TRUE, savePredictions = TRUE)

#### Subadult Gray Snapper (Lutjanus griseus) ####

# logistic regression model (no penalty) with Area Under the Receiver
# Operator Curve (AUC-ROC) set as the test statistic
lg_caret = caret::train(Presence2 ~ ., lg_train, method = "glm", 
                        family = "binomial", trControl = trainControl, 
                        metric = "ROC", maximize = TRUE, na.action = na.pass)

summary(lg_caret)

# Check how well (or poorly) the model fit to the training data using 
# AUC-ROC, training sensitivity, and training specificity

# *NOTE* caret automatically uses absence as the reference class, so 
# sensitivity here is referring to absences and specificity is referring
# to presences. 
lg_caret$levels 
lg_caret$results

# check the order in which coefficients are listed, then make a data frame with
# variable names in the same order
coef(lg_caret$finalModel)
coef_names = c("Intercept", "Mangrove_Dist", "Reef_Dist",
               "Depth", "Slope", "BPI_Broad", "Mean_Sum_Temp",
               "Mean_Sum_Sal", "Mean_Win_Temp", "Mean_Win_Sal", 
               "Patch_Area", "PA_Ratio", "Patch_Neigh_Dist", 
               "Area_CRHB", "Area_SG")

# extract coefficient values from the fitted model
lg_coef = as.vector(coef(lg_caret$finalModel))

# extract odds ratios (exponentiated coefficients)
lg_odds = as.vector(exp(coef(lg_caret$finalModel)))

# extract probabilities (odds ratio / odds ratio + 1)
lg_pr = as.vector(exp(coef(lg_caret$finalModel))/
                    (exp(coef(lg_caret$finalModel)) + 1))

# combine all of this info into a digestible data frame
lg_caret_results = data.frame(coef_names, lg_coef, lg_odds, lg_pr)

# rename columns for clarity
lg_caret_results = lg_caret_results %>%
  rename(Variable = coef_names,
         Coefficient = lg_coef,
         Odds_Ratio = lg_odds,
         Probability = lg_pr)

# save data frame as csv in github repo
write.csv(lg_caret_results, 
          paste0(git_data, "Subadult_Gray_Snapper_Logistic_Regression_Results.csv"),
          row.names = FALSE)

# predict the probability of presence/absence across the withheld validation data
lg_caret_probs = predict(lg_caret, newdata = lg_test, type = "prob")

# predict binary response (presence/absence) across validation data and calculate 
# overall map accuracy, sensitivity, and specificity at the default suitability
# threshold of 0.5 for the predicted probability of presence
lg_caret_pred = predict(lg_caret, newdata = lg_test)
lg_caret_CM = confusionMatrix(data = lg_caret_pred, reference = lg_test$Presence2,
                              positive = "PRESENCE")
lg_caret_CM 
# *NOTE* because we set positive = presence, sensitivity here is referring to 
# presences predictions and specificity is referring to absence predictions! 
# this is the OPPOSITE of how caret treats our response classes when we
# ran lg_caret$results above to check how well the model fit to the training data.

# let's combine the testing data (known outcomes) to the predictions at the
# testing sites (predicted outcomes)
lg_evaluation = cbind(read.csv(paste0(git_data, "Subadult_Gray_Snapper_Model_Validation_Data.csv")),
                      as.data.frame(lg_caret_pred))
lg_evaluation = lg_evaluation %>% 
  rename(Prediction = lg_caret_pred) # rename prediction column

# isolate the false positives
lg_FP = lg_evaluation %>%
  filter(Prediction == "PRESENCE" & Presence2 == "ABSENCE") %>%
  add_column(Result = "FALSE POSITIVE")

# isolate the true positives
lg_TP = lg_evaluation %>%
  filter(Prediction == "PRESENCE" & Presence2 == "PRESENCE") %>%
  add_column(Result = "TRUE POSITIVE")

# isolate the false negatives
lg_FN = lg_evaluation %>%
  filter(Prediction == "ABSENCE" & Presence2 == "PRESENCE") %>%
  add_column(Result = "FALSE NEGATIVE")

# isolate the true negatives
lg_TN = lg_evaluation %>%
  filter(Prediction == "ABSENCE" & Presence2 == "ABSENCE") %>%
  add_column(Result = "TRUE NEGATIVE")

# double check the confusion matrix to make sure our filtering worked properly
lg_caret_CM 

# combine the rows back together again
lg_evaluation2 = do.call("rbind", list(lg_FP, lg_TP, lg_FN, lg_TN))

# write out these data as a csv so that we can map the distribution of false vs.
# true negatives and positives
write.csv(lg_evaluation2,
          paste0(git_data, "Subadult_Gray_Snapper_Model_Testset_Predictions.csv"),
          row.names = FALSE)

# remove intermediate data 
rm(list = c("lg_TP", "lg_FP", "lg_TN", "lg_FN"))

#### Subadult Bluestriped Grunt (Haemulon sciurus) ####

# logistic regression model (no penalty) with Area Under the Receiver
# Operator Curve (AUC-ROC) set as the test statistic
hs_caret = caret::train(Presence2 ~ ., hs_train, method = "glm", 
                        family = "binomial", trControl = trainControl, 
                        metric = "ROC", maximize = TRUE, na.action = na.pass)

summary(hs_caret)

# Check how well (or poorly) the model fit to the training data using 
# AUC-ROC, training sensitivity, and training specificity

# *NOTE* caret automatically uses absence as the reference class, so 
# sensitivity here is referring to absences and specificity is referring
# to presences. 
hs_caret$levels 
hs_caret$results

# check the order in which coefficients are listed, then make a data frame with
# variable names in the same order
coef(hs_caret$finalModel)
coef_names = c("Intercept", "Mangrove_Dist", "Reef_Dist",
               "Depth", "Slope", "BPI_Broad", "Mean_Sum_Temp",
               "Mean_Sum_Sal", "Mean_Win_Temp", "Mean_Win_Sal", 
               "Patch_Area", "PA_Ratio", "Patch_Neigh_Dist", 
               "Area_CRHB", "Area_SG")

# extract coefficient values from the fitted model
hs_coef = as.vector(coef(hs_caret$finalModel))

# extract odds ratios (exponentiated coefficients)
hs_odds = as.vector(exp(coef(hs_caret$finalModel)))

# extract probabilities (odds ratio / odds ratio + 1)
hs_pr = as.vector(exp(coef(hs_caret$finalModel))/
                    (exp(coef(hs_caret$finalModel)) + 1))

# combine all of this info into a digestible data frame
hs_caret_results = data.frame(coef_names, hs_coef, hs_odds, hs_pr)

# rename columns for clarity
hs_caret_results = hs_caret_results %>%
  rename(Variable = coef_names,
         Coefficient = hs_coef,
         Odds_Ratio = hs_odds,
         Probability = hs_pr)

# save data frame as csv in github repo
write.csv(hs_caret_results, 
          paste0(git_data, "Subadult_Bluestriped_Grunt_Logistic_Regression_Results.csv"),
          row.names = FALSE)

# predict the probability of presence/absence across the withheld validation data
hs_caret_probs = predict(hs_caret, newdata = hs_test, type = "prob")

# predict binary response (presence/absence) across validation data and calculate 
# overall map accuracy, sensitivity, and specificity at the default suitability
# threshold of 0.5 for the predicted probability of presence
hs_caret_pred = predict(hs_caret, newdata = hs_test)
hs_caret_CM = confusionMatrix(data = hs_caret_pred, reference = hs_test$Presence2,
                              positive = "PRESENCE")
hs_caret_CM 
# *NOTE* because we set positive = presence, sensitivity here is referring to 
# presences predictions and specificity is referring to absence predictions! 
# this is the OPPOSITE of how caret treats our response classes when we
# ran hs_caret$results above to check how well the model fit to the training data.

# let's combine the testing data (known outcomes) to the predictions at the
# testing sites (predicted outcomes)
hs_evaluation = cbind(read.csv(paste0(git_data, "Subadult_Bluestriped_Grunt_Model_Validation_Data.csv")),
                      as.data.frame(hs_caret_pred))
hs_evaluation = hs_evaluation %>% 
  rename(Prediction = hs_caret_pred) # rename prediction column

# isolate the false positives
hs_FP = hs_evaluation %>%
  filter(Prediction == "PRESENCE" & Presence2 == "ABSENCE") %>%
  add_column(Result = "FALSE POSITIVE")

# isolate the true positives
hs_TP = hs_evaluation %>%
  filter(Prediction == "PRESENCE" & Presence2 == "PRESENCE") %>%
  add_column(Result = "TRUE POSITIVE")

# isolate the false negatives
hs_FN = hs_evaluation %>%
  filter(Prediction == "ABSENCE" & Presence2 == "PRESENCE") %>%
  add_column(Result = "FALSE NEGATIVE")

# isolate the true negatives
hs_TN = hs_evaluation %>%
  filter(Prediction == "ABSENCE" & Presence2 == "ABSENCE") %>%
  add_column(Result = "TRUE NEGATIVE")

# double check the confusion matrix to make sure our filtering worked properly
hs_caret_CM 

# combine the rows back together again
hs_evaluation2 = do.call("rbind", list(hs_FP, hs_TP, hs_FN, hs_TN))

# write out these data as a csv so that we can map the distribution of false vs.
# true negatives and positives
write.csv(hs_evaluation2,
          paste0(git_data, "Subadult_Bluestriped_Grunt_Model_Testset_Predictions.csv"),
          row.names = FALSE)

# remove intermediate data 
rm(list = c("hs_TP", "hs_FP", "hs_TN", "hs_FN"))
