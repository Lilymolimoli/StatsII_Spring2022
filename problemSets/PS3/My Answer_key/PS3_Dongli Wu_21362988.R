
library(nnet)
library(stargazer)
library(survivalROC)
library(pROC)
library(cutpointr)
library(pscl)
library(DHARMa)

### Question 1.1 
# import data
setwd(getwd())
gdp <- read.csv("gdpChange.csv")

# observe the data information
summary(gdp) 

# select the useful columns
gdp1 <- gdp[, c("GDPWdiff", "REG", "OIL")]

# check type of GDPWdiff 
class(gdp1$GDPWdiff)

# use cut() to make the levels to 3 groups. Because all the value of GDPWdiff are integer so we can use (-0.1,0] to get =0, meanwhile it can change type of GDPWdiff from integer to factor. 
gdp1$GDPWdiff <- cut(gdp1$GDPWdiff, breaks = c(min(gdp1$GDPWdiff), -0.1,0, max(gdp1$GDPWdiff)), labels = c( "negative", "no change","positive") )

# check and make sure the levels are right now
levels(gdp1$GDPWdiff)

# set reference level as no change and convert the factor to un-ordered
gdp1$GDPWdiff <- relevel(gdp1$GDPWdiff, ref = "no change", ordered = FALSE)

# check the data information that we can see 1 NA in the column of GDPWdiff 
summary(gdp1) 

# remove NA
gdp1 <- gdp1[!is.na(gdp1$GDPWdiff),]

# After clean and organize the data as above, we now get an unordered variable GDPWdiff with 3 catagories("negative", "no change","positive")as the output and also set "no change" as the reference category

# Let's run multinomial logit regression now
mult_log <- multinom(GDPWdiff ~ REG + OIL, data = gdp1)
stargazer(mult_log, type = "text")

### Check coefficents and cutoff point
# get coefficents 
exp <- exp(coef(mult_log))
stargazer(exp, type = "text")

# get cutoff points with odds and CI
cutoff_points <- exp(confint(mult_log, alpa = 0.05))
cutoff_points

#  get P-values
z <- summary(mult_log)$coefficients/summary(mult_log)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
stargazer(p, type = "text")

# generate the table of coefficents and P-values


### Question 1.2 Construct and interpret an ordered multinomial logit with GDPWdiff as the outcome variable, including the estimated cutoff points and coefficients.

# select the useful columns
gdp2 <- gdp[, c("GDPWdiff", "REG", "OIL")]

# check type of GDPWdiff 
class(gdp2$GDPWdiff)

# use cut() to make the levels to 3 groups. Because all the value of GDPWdiff are integer so we can use (-0.1,0] to get =0, meanwhile it can change type of GDPWdiff from integer to factor. 
gdp2$GDPWdiff <- cut(gdp2$GDPWdiff, breaks = c(min(gdp2$GDPWdiff), -0.1,0, max(gdp2$GDPWdiff)), labels = c( "negative", "no change","positive") )

# check and make sure the levels are right now
levels(gdp2$GDPWdiff)

# set reference level and convert the factor to ordered
gdp2$GDPWdiff <- relevel(gdp2$GDPWdiff, ref = TRUE)
as.ordered(gdp2$GDPWdiff)

# check the data information that we can see 1 NA in the column of GDPWdiff 
summary(gdp2) 

# remove NA
gdp2 <- gdp2[!is.na(gdp2$GDPWdiff),]

# run multinomial logit regression
mult_log2 <- multinom(GDPWdiff ~ REG + OIL, data = gdp2)
stargazer(mult_log2, type = "text")

# get coefficents 
exp2 <- exp(coef(mult_log2))
stargazer(exp2, type = "text")

# get cutoff points with odds and CI
cutoff_points2 <- exp(confint(mult_log2, alpa = 0.05))
cutoff_points2

### QUestion 2 (a) Run a Poisson regression because the outcome is a count variable. Is there evidence that PAN presidential candidates visit swing districts more? Provide a test statistic and p-value.
# import the data
dt <- read.csv("MexicoMuniData.csv")

# select useful variables
dt <- dt[, c("PAN.visits.06", "competitive.district", "marginality.06", "PAN.governor.06")]

summary(dt)
str(dt)

# run Poisson Regression Model
dt_p <- glm (PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=dt, family=poisson)
stargazer(dt_p, type = "text")

summary(dt_p)

# check the histogram of PAN. visits.06
hist(dt$PAN.visits.06)

#  Let's check if it is over-dispersion
testDispersion(dt_p)

### Therefore, Zero-Inflated Model should be considered.
dt_zip <- zeroinfl(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data=dt, dist="poisson")
summary(dt_zip)

### (b) Interpret the marginality.06 and PAN.governor.06 coefficients.
coeff <- coefficients(dt_p)
coeff_ecp <- exp(coeff)
stargazer(coeff, type = "text")
stargazer(coeff_ecp, type = "text")


### (c) Provide the estimated mean number of visits from the winning PAN presidential candi-date for a hypothetical district that was competitive (competitive.district=1), had an average poverty level (marginality.06 = 0), and a PAN governor (PAN.governor.06=1).

# calculate the exp
exp(coeff[1] + coeff[2]*1 + coeff[3]*0 + coeff[4]*1)
