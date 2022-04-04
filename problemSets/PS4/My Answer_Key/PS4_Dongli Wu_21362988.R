
# load libraries
library(survival)
library(eha)
library(tidyverse)
library(ggfortify)
library(ggplot2)

# import data
data(infants)

# have a look at some details about this infants data information
help(infants)

# A data frame with 80 rows and five variables.
# stratum: Triplet No. Each triplet consist of one infant whose mother died (a case), and two controls, i.e, infants whose mother did not die. Matched on covariates below.
# enter: Age (in days) of case when its mother died.
# exit: Age (in days) at death or right censoring (at age 365 days).
# event: Follow-up ends with death (1) or right censoring (0).
# mother: dead for cases, alive for controls.
# age: Mother's age at infant's birth.
# sex: The infant's sex.
# parish: Birth parish, either Nedertornea or not Nedertornea.
# civst: Civil status of mother, married or unmarried.
# ses: Socio-economic status of mother, either farmer or not farmer.
# year: Year of birth of the infant.

## Run a Kaplan-Meier plot for overall survival

# build a survival object
infants_surv <- with(infants, Surv(enter, exit, event)) 

# Kaplan-Meier plot 
km <- survfit(infants_surv ~ 1, data = infants)
autoplot(km, main = "Infants Overall Survial", xlab = "Days", ylab = "Survival Rate")

# Use summary function to get a general discription 
summary(km) 

## run a Cox Proportional Hazard regression using mother’s age and infant’s gender
cox <- coxph(infants_surv ~ age + sex, data = infants)
summary(cox)

## Plot the Cox Proportional Hazard model with Cumulative Hazard Function

cox_fit <- coxreg(infants_surv ~ age + sex, data = infants)
plot(cox_fit, xlab = "Days", ylab = "Cumulative Hazard")


# make a Chisq test to assess the quality of this model
drop1(cox, test = "Chisq")


exp(-0.04)
exp(-0.485)

