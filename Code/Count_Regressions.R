# associated GitHub repo: 
# https://github.com/CourtneyStuart/FL_Patch_Reefs


# install packages (only need to do this once)
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("MASS")
#install.packages("conflicted")
#install.packages("dplyr")
#install.packages("easypackages")
#install.packages("pscl")
#install.packages("lmtest")
#install.packages("AER")


# load packages
library(easypackages)
libraries("tidyverse", "ggplot2", "MASS", "conflicted",
          "dplyr", "pscl", "lmtest", "AER")
conflicted::conflict_prefer("select", "dplyr")


# set plot margins for later (only need to do this once)
par(mar = c(4,4,2.1,2.1))


##### data prep and exploration ####
# read in the model calibration and evaluation data subsets from GitHub Repo

# gray snapper (Lutjanus griseus (lg))
lg_train = read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Gray_Snapper_Model_Calibration_Data.csv")
lg_test = read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Gray_Snapper_Model_Validation_Data.csv")

# bluestriped grunt (Haemulon sciurus (hs))
hs_train = read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Bluestriped_Grunt_Model_Calibration_Data.csv")
hs_test = read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Bluestriped_Grunt_Model_Validation_Data.csv")


# plot basic histograms of species abundance in the training subsets
ggplot(lg_train, aes(x = Abundance)) + geom_histogram(binwidth = 1) + theme_bw()
ggplot(hs_train, aes(x = Abundance)) + geom_histogram(binwidth = 1) + theme_bw()


# create a new column "Count" where values represent abundance as integers 
# (whole numbers), rather than averages across buddy diver pairs. 
lg_train = lg_train %>%
  mutate(Count = round(lg_train$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

lg_test = lg_test %>%
  mutate(Count = round(lg_test$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

hs_train = hs_train %>%
  mutate(Count = round(hs_train$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)

hs_test = hs_test %>%
  mutate(Count = round(hs_test$Abundance, digits = 0)) %>%
  relocate(Count, .after = Abundance)


# keep only columns of interest for modeling
lg_train = lg_train %>%
  select(Count, 12:26) # Count is the response & 12:26 is the range of predictor columns

lg_test = lg_test %>%
  select(Count, 12:26)

hs_train = hs_train %>%
  select(Count, 12:26)

hs_test = hs_test %>%
  select(Count, 12:26)


#### modeling ####
# Overview of Poisson Regression assumptions
# 1. Poisson Response - the response variable is a count per unit of time or space,
# described by a Poisson distribution. 
# 2. Independence - the observations must be independent of one another. 
# 3. Mean = Variance - by definition, the mean of a Poisson random variable must
# be equal to its variance. --> equidispersion
# 4. Linearity - the log of the mean rate, log(lambda), must be a linear function of x.


# first compare the observed mean and variance for each species
mean(lg_train$Count)
var(lg_train$Count)

mean(hs_train$Count)
var(hs_train$Count)
# variance far exceeds the mean for both species, especially bluestriped grunts


# Having considered these assumptions, let's proceed with caution...
# fit a simple Poisson Regression to the gray snapper training data
lg_pois = glm(Count ~ ., data = lg_train, family = poisson(link = "log"))

# check out the results
summary(lg_pois)

# calculate the p-value for the deviance goodness of fit test --> calculate the 
# probability to the right of the deviance value for the chi-squared distribution
# on 157 degrees of freedom:
pchisq(lg_pois$deviance, df = lg_pois$df.residual, lower.tail = FALSE)

# The null hypothesis is that our model is correctly specified. according to the 
# deviance goodness of fit test, we have strong evidence to reject the null 
# hypothesis (suggesting a poor model fit). 

# overdispersion is likely a problem, let's use the following code to check the 
# null hypothesis of equidispersion
dispersiontest(lg_pois, trafo = 1, alternative = "greater") 
# evidence of overdispersion (p-value < significance level of 0.05)


# It's worth trying to fit a negative binomial regression to address this issue.
lg_nb = glm.nb(Count ~ ., data = lg_train)
summary(lg_nb)


# residual plot for Poisson regression
lg_pois_res = resid(lg_pois)
plot(fitted(lg_pois), lg_pois_res, col = 'steelblue', pch = 16, 
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Poisson')
abline(0,0)

# residual plot for negative binomial regression 
lg_nb_res = resid(lg_nb)
plot(fitted(lg_nb), lg_nb_res, col = 'steelblue', pch = 16,
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Negative Binomial')
abline(0,0)


# likelihood ratio test to compare models (null H0 is that the restricted Poisson
# model provides a better fit than the more complex negative binomial model)
# by hand...
pchisq(2 * (logLik(lg_nb) - logLik(lg_pois)), df = 1, lower.tail = FALSE)

# using lmtest package...
lrtest(lg_pois, lg_nb)

# reject H0,  there is a statistically significant difference between the negative
# binomial and poisson regressions, NB provides a better fit to the training data

# bluestriped grunt (HS) tests/models
# poisson regression for HS
hs_pois = glm(Count ~ ., data = hs_train, family = poisson(link = "log"))
summary(hs_pois)


# calculate the p-value for the deviance goodness of fit test --> calculate the 
# probability to the right of the deviance value for the chi-squared distribution
# on 157 degrees of freedom:
pchisq(hs_pois$deviance, df = hs_pois$df.residual, lower.tail = FALSE)

# The null hypothesis is that our model is correctly specified. according to the 
# deviance goodness of fit test, we have strong evidence to reject the null 
# hypothesis (suggesting a poor model fit). 

# overdispersion is likely a problem, let's use the following code to check the 
# null hypothesis of equidispersion
dispersiontest(hs_pois, trafo = 1, alternative = "greater") 
#  evidence of overdispersion (p-value < significance level 0.05)


# negative binomial regression for HS
# line for lg: lg_nb = glm.nb(Count ~ ., data = lg_train, control = glm.control(maxit = 15))
hs_nb = glm.nb(Count ~ ., data = hs_train)
summary(hs_nb)


# residual plot for Poisson regression
hs_pois_res = resid(hs_pois)
plot(fitted(hs_pois), hs_pois_res, col = 'steelblue', pch = 16, 
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', main = 'Poisson')
abline(0,0)

# residual plot for negative binomial regression 
hs_nb_res = resid(hs_nb)
plot(fitted(hs_nb), hs_nb_res, col = 'steelblue', pch = 16,
     xlab = 'Predicted Outcomes', ylab = 'Standardized Residuals', 
     main = 'Negative Binomial')
abline(0,0)


# likelihood ratio test to compare models (null H0 is that the restricted Poisson
# model provides a better fit than the more complex negative binomial model)
# by hand...
pchisq(2 * (logLik(hs_nb) - logLik(hs_pois)), df = 1, lower.tail = FALSE)

# using lmtest package...
lrtest(hs_pois, hs_nb)

# reject H0, there is a statistically significant difference between the negative
# binomial and poisson regressions, NB provides a better fit to the training data


#### interpreting NB models ####
#install.packages("jtools")
library(jtools)

#  gray snapper NB model 
summary(lg_nb)

# remember this model uses a log link function, so we need to exponentiate the 
# coefficients to get the incidence rate ratios
lg_nb_coef = as.data.frame(lg_nb$coefficients) %>%
  mutate(Coefficients = lg_nb$coefficients) %>%
  mutate(Covariates = c("Intercept", "Mangrove_Dist", "Reef_Dist", "Depth", 
                      "Slope", "BPI_Broad", "Mean_Sum_Temp", "Mean_Sum_Sal", 
                      "Mean_Win_Temp", "Mean_Win_Sal", "Patch_Area", "PA_Ratio", 
                      "Patch_Neigh_Dist", "Area_CRHB", "Area_SG", "Pred_Density")) %>%
  select(Covariates, Coefficients) # negative binomial coefficients

# add exponentiated incident rate ratios and CIs
lg_nb_exp = summ(lg_nb, exp = T)
lg_nb_exp = as.data.frame(lg_nb_exp$coeftable) %>% 
  mutate(Covariates = c("Intercept", "Mangrove_Dist", "Reef_Dist", "Depth", 
                        "Slope", "BPI_Broad", "Mean_Sum_Temp", "Mean_Sum_Sal", 
                        "Mean_Win_Temp", "Mean_Win_Sal", "Patch_Area", "PA_Ratio", 
                        "Patch_Neigh_Dist", "Area_CRHB", "Area_SG", "Pred_Density"))
  
lg_nb_table = merge(lg_nb_coef, lg_nb_exp, by = "Covariates")

# let's now try predicting the validation data subset using the fitted negative 
# binomial models
lg_test$nb_Predicted = predict(lg_nb, lg_test, type = "response")

# plot the observed counts vs. the predicted counts
ggplot(lg_test, aes(nb_Predicted, Count)) +
  geom_point() +
  ggtitle("Negative binomial regression") +
  ylab("Observed count") +
  xlab("Predicted count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 

# how often did the NB model accurately predict the known counts?
# make a new dataframe, where a value of 1 means a correct prediction and a value 
# of 0 means an incorrect prediction 
lg_predictions = as.data.frame(ifelse(
  lg_test$Count == round(lg_test$nb_Predicted, digits = 0), 1, 0)) %>%
  mutate(Correct_Prediction = ifelse(lg_test$Count == round(lg_test$nb_Predicted, digits = 0), 1, 0)) %>%
  mutate(Observed_Count = lg_test$Count) %>%
  mutate(Predicted_Count = round(lg_test$nb_Predicted, digits = 0)) %>%
  select(Observed_Count, Predicted_Count, Correct_Prediction)

# calculate accuracy by summing the correct predictions, dividing by the total
# number of predictions, and multiplying by 100
lg_nb_accuracy = (sum(lg_predictions$Correct_Prediction)/as.numeric(nrow(lg_predictions))) * 100

# how about a predicted fish count within (+-) 5 from the observed count
lg_predictions$Within_Five = 
  ifelse((abs(lg_test$Count - round(lg_test$nb_Predicted, digits = 0)) <= 5), 1, 0)

# calculate accuracy by summing the correct predictions (+- 5 fishes), dividing 
# by the total number of predictions, and multiplying by 100
lg_within_five_accuracy = (sum(lg_predictions$Within_Five)/as.numeric(nrow(lg_predictions))) * 100


# save predictions for mapping 
lg_map_pred = lg_test %>%
  mutate(Observed_Count = lg_test$Count) %>%
  mutate(Predicted_Count = round(lg_test$nb_Predicted, digits = 0)) %>%
  mutate(Correct_Prediction = ifelse(Observed_Count == Predicted_Count, 1, 0)) %>%
  mutate(Within_Five = ifelse((abs(Observed_Count - Predicted_Count) <= 5), 1, 0))

# we need to bring back in the lat/long data for mapping
lg_map_pred2 = cbind(
  (read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Gray_Snapper_Model_Validation_Data.csv")), 
  lg_map_pred)
  

#  bluestriped grunt NB model 
summary(hs_nb)

# remember this model uses a log link function, so we need to exponentiate the 
# coefficients to get the incidence rate ratios
hs_nb_coef = as.data.frame(hs_nb$coefficients) %>%
  mutate(Coefficients = hs_nb$coefficients) %>%
  mutate(Covariates = c("Intercept", "Mangrove_Dist", "Reef_Dist", "Depth", 
                        "Slope", "BPI_Broad", "Mean_Sum_Temp", "Mean_Sum_Sal", 
                        "Mean_Win_Temp", "Mean_Win_Sal", "Patch_Area", "PA_Ratio", 
                        "Patch_Neigh_Dist", "Area_CRHB", "Area_SG", "Pred_Density")) %>%
  select(Covariates, Coefficients) # negative binomial coefficients

# add exponentiated incident rate ratios and 95% CIs
hs_nb_exp = summ(hs_nb, exp = T)
hs_nb_exp = as.data.frame(hs_nb_exp$coeftable) %>% 
  mutate(Covariates = c("Intercept", "Mangrove_Dist", "Reef_Dist", "Depth", 
                        "Slope", "BPI_Broad", "Mean_Sum_Temp", "Mean_Sum_Sal", 
                        "Mean_Win_Temp", "Mean_Win_Sal", "Patch_Area", "PA_Ratio", 
                        "Patch_Neigh_Dist", "Area_CRHB", "Area_SG", "Pred_Density"))

hs_nb_table = merge(hs_nb_coef, hs_nb_exp, by = "Covariates")

# let's now try predicting the validation data subset using the fitted negative 
# binomial models
hs_test$nb_Predicted = predict(hs_nb, hs_test, type = "response")

# plot the observed counts vs. the predicted counts
ggplot(hs_test, aes(nb_Predicted, Count)) +
  geom_point() +
  ggtitle("Negative binomial regression") +
  ylab("Observed count") +
  xlab("Predicted count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# how often did the NB model accurately predict the known counts?
# make a new dataframe, where a value of 1 means a correct prediction and a value 
# of 0 means an incorrect prediction 
hs_predictions = as.data.frame(ifelse(
  hs_test$Count == round(hs_test$nb_Predicted, digits = 0), 1, 0)) %>%
  mutate(Correct_Prediction = ifelse(hs_test$Count == round(hs_test$nb_Predicted, digits = 0), 1, 0)) %>%
  select(Correct_Prediction)

# calculate accuracy by summing the correct predictions, dividing by the total
# number of predictions, and multiplying by 100
hs_nb_accuracy = (sum(hs_predictions$Correct_Prediction)/as.numeric(nrow(hs_predictions))) * 100

# how about a predicted fish count within (+-) 5 from the observed count
hs_predictions$Within_Five = 
  ifelse((abs(hs_test$Count - round(hs_test$nb_Predicted, digits = 0)) <= 5), 1, 0)

# calculate accuracy by summing the correct predictions (+- 5 fishes), dividing 
# by the total number of predictions, and multiplying by 100
hs_within_five_accuracy = (sum(hs_predictions$Within_Five)/as.numeric(nrow(hs_predictions))) * 100

# save predictions for mapping 
hs_map_pred = hs_test %>%
  mutate(Observed_Count = hs_test$Count) %>%
  mutate(Predicted_Count = round(hs_test$nb_Predicted, digits = 0)) %>%
  mutate(Correct_Prediction = ifelse(Observed_Count == Predicted_Count, 1, 0)) %>%
  mutate(Within_Five = ifelse((abs(Observed_Count - Predicted_Count) <= 5), 1, 0))

hs_map_pred2 = cbind(
  (read.csv("https://raw.githubusercontent.com/CourtneyStuart/FL_Patch_Reefs/main/Data/Subadult_Bluestriped_Grunt_Model_Validation_Data.csv")), 
  hs_map_pred)

#### saving the outputs ####

# Saving the negative binomial coefficients, incident rate ratios (IRR - aka 
# exponentiated coefficients), 95% confidence intervals around IRRs, and p-values
# as a csv file to our external hard drive

# save our output file location (only need to do this once)
data_wd = "E:/BIOL398_Patch_Reef_Residency/Tabular_Data/"

# save csv for gray snapper
write.csv(lg_nb_table,
          paste0(data_wd, "Subadult_Gray_Snapper_Negative_Binomial_Results.csv"),
          row.names = FALSE)

# repeat for bluestriped grunt
write.csv(hs_nb_table,
          paste0(data_wd, "Subadult_Bluestriped_Grunt_Negative_Binomial_Results.csv"),
          row.names = FALSE)

# validation predictions for mapping spatial accuracy
write.csv(lg_map_pred2,
          paste0(data_wd, "Subadult_Gray_Snapper_Negative_Binomial_Map_Predictions.csv"),
          row.names = FALSE)

write.csv(hs_map_pred2,
          paste0(data_wd, "Subadult_Bluestriped_Grunt_Negative_Binomial_Map_Predictions.csv"),
          row.names = FALSE)

# let's also prep these datasets for committing and pushing to our github repo
# github repo folder local location 
git_wd = "E:/BIOL398_Patch_Reef_Residency/GitHub/FL_Patch_Reefs/Data/"

# save csv for gray snapper
write.csv(lg_nb_table,
          paste0(git_wd, "Subadult_Gray_Snapper_Negative_Binomial_Results.csv"),
          row.names = FALSE)

# repeat for bluestriped grunt
write.csv(hs_nb_table,
          paste0(git_wd, "Subadult_Bluestriped_Grunt_Negative_Binomial_Results.csv"),
          row.names = FALSE)
