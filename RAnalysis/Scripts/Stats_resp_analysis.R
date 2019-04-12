# Project: Juvenile_Geoduck_OA
# Title: Resp_LoLinR_datasets.R
# Supported by: FFAR
# Author: Sam Gurr
# Date Updated: 20190411
# Contact: samuel_gurr@uri.edu

# OBJECTIVE: This script is a series of linear regressions between the "reference" dataset and the nine combinations of 
# alpha values and data truncations (from the Lolin package) to arrive to a reproducible, non-bias, and accurate representation of the respiration data

# WHY is this IMPORTANT?: Although the LoLin package was used to remove bias, the "reference" dataset, in which each run was completed
# individually and parameters of the model were altered as needed, contains a tedious criterion that does not make this exact data 
# easy to obtain for a reader. This  script assess the correlative strength of the complete automated analysis (of nine parameter combinations)
# to the reference in order to choose the set of model paremeters (LoLin outputs) that best represent the data. 

# PROCEDURE:
# 1) Determine the dataset (i.e. alpha = 0.4, no truncation) with parmeters that best represents the "reference" via loess curve and linear regression
# choose dataset with least values outside of the loess curve (data from each exposure period analyzed seperately; initial and secondary) 
# 2) Isolate these values by subsetting rows from the orignal table
# 3) Repeat step 1 for this subset and choose parameters with greatest linear strength to the reference
# 4) Merge the two datasets as "Finalresp" and check linear strength to the initial "reference" for that exposure period

rm(list=ls())
##Install and load packages
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(Rmisc)
library(nlme)
library(lme4)
library(ggplot2) 
library(lme4)
library(ggpubr)
library(nlme)
library(plotrix)
library(lsmeans)
library(gridExtra)
#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/") #set working

#Load Size Data
resp<-read.csv("Data/Metabolic.Rates/Resp.30d.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(resp)

# subset for exposure periods
resp_EXP1 <- subset(resp, EXP.numb=="EXP1") # subset initial 10-d (all resp values on days 2,4,6,8,10)
resp_EXP2 <- subset(resp, EXP.numb=="EXP2") # subset secondary 6-d (contains resp values days 0,2,4,6; NOTE day 0 was BEFORE exposure to treatment)
resp_EXP2.0 <-subset(resp_EXP2, Day!=0) # filter out Day 0 of secondary exposure

#------------- Initial exposure "reference" vs. automated outputs
# RESULT: 
# first linear fit determined  alpha 0.4 and no truncation had the least data outside loess curve
# second subset determined 0.6 and 10-20min truncation had the strongest fit
# merged a final respiration rate dataset with r^2 0.887 with the initial "reference"
# view data to verify column number
head(resp_EXP1)
# "Ref"               = [,1]
# "alpha0.2_all"      = [,2]
# "alpha0.2_min10.20" = [,3]
# "alpha0.2_min10.25" = [,4]
# "alpha0.4_all"      = [,5]
# "alpha0.4_min10.20" = [,6]
# "alpha0.4_min10.25" = [,7]
# "alpha0.6_all"      = [,8]
# "alpha0.6_min10.20" = [,9]
# "alpha0.6_min10.25" = [,10]

plot(resp_EXP1[,1],resp_EXP1[,2], main= "Ref vs. alpha0.2_all")      # Adjusted R-squared:  0.4961 
summary(lm(resp_EXP1[,1]~resp_EXP1[,2]))
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,2])) +
  geom_point() +
  geom_smooth(method = 'loess') +
 geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))
plot(resp_EXP1[,1],resp_EXP1[,3], main= "Ref vs. alpha0.2_min10.20") # Adjusted R-squared:  0.2873
summary(lm(resp_EXP1[,1]~resp_EXP1[,3]))
plot(resp_EXP1[,1],resp_EXP1[,4], main= "Ref vs. alpha0.2_min10.25") # Adjusted R-squared:  0.1662
summary(lm(resp_EXP1[,1]~resp_EXP1[,4]))
plot(resp_EXP1[,1],resp_EXP1[,5], main= "Ref vs. alpha0.4_all")      # Adjusted R-squared:  0.3902
summary(lm(resp_EXP1[,1]~resp_EXP1[,5]))
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) # 19 total outside of loess curve =  57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94,
plot(resp_EXP1[,1],resp_EXP1[,6], main= "Ref vs. alpha0.4_min10.20") # Adjusted R-squared:  0.2565 
summary(lm(resp_EXP1[,1]~resp_EXP1[,6]))
plot(resp_EXP1[,1],resp_EXP1[,7], main= "Ref vs. alpha0.4_min10.25") # Adjusted R-squared:  0.314 
summary(lm(resp_EXP1[,1]~resp_EXP1[,7]))
plot(resp_EXP1[,1],resp_EXP1[,8], main= "Ref vs. alpha0.6_all")      # Adjusted R-squared:  0.4921 
summary(lm(resp_EXP1[,1]~resp_EXP1[,8]))
ggplot(resp_EXP1, aes(x = resp_EXP1[,8], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))
plot(resp_EXP1[,1],resp_EXP1[,9], main= "Ref vs. alpha0.6_min10.20") # Adjusted R-squared:  0.4012 
summary(lm(resp_EXP1[,1]~resp_EXP1[,9]))
ggplot(resp_EXP1, aes(x = resp_EXP1[,9], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))
plot(resp_EXP1[,1],resp_EXP1[,10], main= "Ref vs. alpha0.6_min10.25") # Adjusted R-squared:  0.4393 
summary(lm(resp_EXP1[,1]~resp_EXP1[,10]))
ggplot(resp_EXP1, aes(x = resp_EXP1[,10], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))

# CONCLUSION - alpha 0.4 and no truncation has the least values outside of the loess fit

# subset resp_EXP1 for a new data file
newdata_resp_EXP1_ALL <- data.frame(resp_EXP1[,c(1:10,20)]) # call the rows we need
newdata_resp_EXP1_ALL_1 <-newdata_resp_EXP1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ] # subset for values outside of loess curve for alpha 0.4 and no truncation

# plot the new subsetted dataset for each set of parameters
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,2])
summary(lm(newdata_resp_EXP1_ALL_1[,2]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.5087 
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,3])
summary(lm(newdata_resp_EXP1_ALL_1[,3]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared:  0.6168 
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,4])
summary(lm(newdata_resp_EXP1_ALL_1[,4]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.05779
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,5])
summary(lm(newdata_resp_EXP1_ALL_1[,5]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.05779 
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,6])
summary(lm(newdata_resp_EXP1_ALL_1[,6]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.3174 
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,7])
summary(lm(newdata_resp_EXP1_ALL_1[,7]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.204 
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,8])
summary(lm(newdata_resp_EXP1_ALL_1[,8]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.2296 
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,9])  
summary(lm(newdata_resp_EXP1_ALL_1[,9]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.7219; alpha 0.6 and trunc at 10-20 min
plot(newdata_resp_EXP1_ALL_1[,1],newdata_resp_EXP1_ALL_1[,10])
summary(lm(newdata_resp_EXP1_ALL_1[,10]~newdata_resp_EXP1_ALL_1[,1])) # Adjusted R-squared: 0.3295 

#assgin new  "FINAL" column base as the alpha 0.4 all and inserted points from alpha = 0.6 10-20 min
resp_EXP1$FINALresp <- resp_EXP1[,5] # calls alpha 0.4 and  no trunc as a baseline column (first curve fit step)
resp_EXP1[,21] # this is the column for resp_EXP1$FINALresp 
resp_EXP1[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94),21] <- resp_EXP1$LpcResp_alpha0.6_min10.20.csv[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] # adds the values from alpha 0.6 and trunc at 10-20 min (second curve fit step)
resp_EXP1$FINALresp # view final data

# check the strength of Finalresp to the reference
plot(resp_EXP1[,1],resp_EXP1$FINALresp, main= "Ref vs. FINALresp")
summary(lm(resp_EXP1[,1]~resp_EXP1$FINALresp)) # Multiple R-squared:  0.8783,	Adjusted R-squared:  0.8771 
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01))

# normality test - data tranformation
shapiro.test(resp_EXP1$FINALresp) # not normal
hist(resp_EXP1$FINALresp) # positive or right skewed - need to transform the data
# transform via squareroot gets a normal distrubtion via shapro wilk test
resp_EXP1$sqrt_FINALresp <- sqrt(resp_EXP1$FINALresp)
shapiro.test(resp_EXP1$sqrt_FINALres) #  not normal
hist(resp_EXP1$sqrt_FINALres)

# Two-way ANOVA (not transformed)
resp_EXP1$Day <- as.factor(resp_EXP1$Day) # treat day as a character 
EXP1.ANOVA<- aov(FINALresp ~ Init.treat * Day, data = resp_EXP1) #use untransformed data for model - test residuals for normality
anova(EXP1.ANOVA) # significant effect of initial treatment

# Two-way ANOVA w/ sqrt tranformed data
EXP1.ANOVA.TRANSFORMED <- aov(sqrt_FINALresp ~ Init.treat*Day, data = resp_EXP1) #use untransformed data for model - test residuals for normality
summary(EXP1.ANOVA.TRANSFORMED) # significant effect of initial treatment

# diagnostic tests and plots of model residuals (not transformed)
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.ANOVA)) #plot histogram of residuals
shapiro.test(residuals(EXP1.ANOVA)) # Shapiro test - residuals are normal
leveneTest(EXP1.ANOVA) ## Levene's test for homogeneity  - residuals are normal
boxplot(residuals(EXP1.ANOVA)) #plot boxplot of residuals
plot(fitted(EXP1.ANOVA),residuals(EXP1.ANOVA)) #display residuals versus fitter, normal QQ plot, leverage plot
qqnorm(residuals(EXP1.ANOVA)) # QQ plot

# post-hoc
exp1.resp.ph <- lsmeans(EXP1.ANOVA, pairwise ~  Init.treat * Day)# pariwise Tukey Post-hoc test between repeated treatments
exp1.resp.ph # view post hoc summary
E1.pairs.RESP.05 <- cld(exp1.resp.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.RESP.05 # NO PAIRWISE DIFFERENCES
E1.pairs.RESP.1 <- cld(exp1.resp.ph, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.1
E1.pairs.RESP.1 # maringal differences between elevated (day 5) and ambient (days 2 and 5)

# sumamry tables to calculate the mean difference between treatments
sumresp_EXP1 <- summarySE(resp_EXP1, 
                       measurevar="FINALresp", 
                       groupvars=c("Date","Init.treat")) # means by date
sumresp_EXP1
# get percent difference
sumresp_EXP1_means <- summarySE(sumresp_EXP1, 
                             measurevar="FINALresp", 
                             groupvars=c("Init.treat")) # means by treatment (from previous means of date)
sumresp_EXP1_means
percentdiff <- ((sumresp_EXP1_means[1,3] - sumresp_EXP1_means[2,3])/sumresp_EXP1_means[1,3])*100 # calculate the percent difference between elevated and ambient pCO2
percentdiff # view the data - 25% metabolic depression under elevated pCO2

#------------- Secondary exposure "reference" vs. automated outputs
# RESULT: 
# first linear fit determined  alpha 0.4 and no truncation had the least data outside loess curve
# second subset determined 0.6 and 10-20min truncation had the strongest fit
# merged a final respiration rate dataset with r^2 0.887 with the initial "reference"
# view data to verify column number
head(resp_EXP2)
# "Ref"               = [,1]
# "alpha0.2_all"      = [,2]
# "alpha0.2_min10.20" = [,3]
# "alpha0.2_min10.25" = [,4]
# "alpha0.4_all"      = [,5]
# "alpha0.4_min10.20" = [,6]
# "alpha0.4_min10.25" = [,7]
# "alpha0.6_all"      = [,8]
# "alpha0.6_min10.20" = [,9]
# "alpha0.6_min10.25" = [,10]

par(mfrow=c(1,1)) # set margins for plots

# plot all parameters against the reference 
plot(resp_EXP2[,1],resp_EXP2[,2], main= "Ref vs. alpha0.2_all")        # Adjusted R-squared:  0.4458 
summary(lm(resp_EXP2[,1]~resp_EXP2[,2]))
plot(resp_EXP2[,1],resp_EXP2[,3], main= "Ref vs. alpha0.2_min10.20")   # Adjusted R-squared:  0.4568 
summary(lm(resp_EXP2[,1]~resp_EXP2[,3]))
plot(resp_EXP2[,1],resp_EXP2[,4], main= "Ref vs. alpha0.2_min10.25")   # Adjusted R-squared:  0.4989 
summary(lm(resp_EXP2[,1]~resp_EXP2[,4]))
plot(resp_EXP2[,1],resp_EXP2[,5], main= "Ref vs. alpha0.4_all")        # Adjusted R-squared:  0.7953, strongest regression and leqst number of values outside the loess interval
summary(lm(resp_EXP2[,1]~resp_EXP2[,5]))
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01))
plot(resp_EXP2[,1],resp_EXP2[,6], main= "Ref vs. alpha0.4_min10.20")   # Adjusted R-squared:  0.5705 
summary(lm(resp_EXP2[,1]~resp_EXP2[,6]))
plot(resp_EXP2[,1],resp_EXP2[,7], main= "Ref vs. alpha0.4_min10.25")   # Adjusted R-squared:  0.4885 
summary(lm(resp_EXP2[,1]~resp_EXP2[,7]))
plot(resp_EXP2[,1],resp_EXP2[,8], main= "Ref vs. alpha0.6_all")        # Adjusted R-squared:  0.4289 
summary(lm(resp_EXP2[,1]~resp_EXP2[,8]))
plot(resp_EXP2[,1],resp_EXP2[,9], main= "Ref vs. alpha0.6_min10.20")   # Adjusted R-squared:  0.5976
summary(lm(resp_EXP2[,1]~resp_EXP2[,9]))
plot(resp_EXP2[,1],resp_EXP2[,10], main= "Ref vs. alpha0.6_min10.25")  # Adjusted R-squared:  0.584 
summary(lm(resp_EXP2[,1]~resp_EXP2[,10]))

# CONCLUSION - alpha 0.4 and no truncation has the strongest r^2 to the reference AND least values outside of the loess fit

# look at the loess graph again...
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) + geom_point() + geom_smooth(method = 'loess') + geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01))
# numbers outside of loess curve are 97,98,105,114,120,132,141,142,143,146,147,150,152,153,155,156,173,176,180,188 
numberID <- (c(98,105,113,114,130, 141,146,147,150,152,153,155,156,188)) - 96 # to obtain the actual order (EXP1 was 96 rows)
numberID # actuall number of ID 1  2  9 18 24 36 45 46 47 50 51 54 56 57 59 60 77 80 84 92
#check values
resp_EXP2[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92), 20] # check what row numbers are outside loess curve
# subset resp_EXP2 for a new data file
newdata_resp_EXP2_ALL <- data.frame(resp_EXP2$Date , resp_EXP2[,c(1:10)])
newdata_resp_EXP2_ALL_1 <-newdata_resp_EXP2_ALL[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92), ] 

# plot the new subsetted dataset for each set of parameters
par(mfrow=c(1,1)) # set margins for plots
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,3])
summary(lm(newdata_resp_EXP2_ALL_1[,3]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared: 0.3447 
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,4])
summary(lm(newdata_resp_EXP2_ALL_1[,4]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared:  0.5029  
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,5])
summary(lm(newdata_resp_EXP2_ALL_1[,5]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared: 0.44 
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,6])
summary(lm(newdata_resp_EXP2_ALL_1[,6]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared: 0.03169  
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,7])
summary(lm(newdata_resp_EXP2_ALL_1[,7]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared: 0.5182 
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,8])
summary(lm(newdata_resp_EXP2_ALL_1[,8]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared: 0.517  
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,9])
summary(lm(newdata_resp_EXP2_ALL_1[,9]~newdata_resp_EXP2_ALL_1[,2]))  # Adjusted R-squared: 0.4168 
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,10])
summary(lm(newdata_resp_EXP2_ALL_1[,10]~newdata_resp_EXP2_ALL_1[,2])) # Adjusted R-squared: 0.6381
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,11])
summary(lm(newdata_resp_EXP2_ALL_1[,11]~newdata_resp_EXP2_ALL_1[,2])) # Adjusted R-squared: 0.7145 use these paramenters - alpha 0.6 and truncation at 10 - 25 mins

# assgin new  "FINAL" column base as the alpha 0.4 with no truncation as a base
head(resp_EXP2) # alpha 0.4 with no truncation = resp_EXP2[,5]
resp_EXP2$FINALresp <- resp_EXP2[,5]
# Note: FINALresp is column # 18 in resp_EXP2; as resp_EXP2[,18]
head(resp_EXP2) # alpha 0.6 10-25mins = resp_EXP2[,10]
resp_EXP2[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92),21] <- resp_EXP2[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92), 10]

# check the strength of Finalresp to the reference; RESULT: Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9458 
plot(resp_EXP2[,1],resp_EXP2$FINALresp, main= "Ref vs. FINALresp")
summary(lm(resp_EXP2[,1]~resp_EXP2$FINALresp))       
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = -0.01))

# subset dataset for Day 0 and days 2 4 and 6
resp_EXP2_2.4.6 <- subset(resp_EXP2, Day!=0) # subset out pre-exposure measurements on day 0 (day 24 of experiment)

# ANALYSIS
# normality test - data tranformation
shapiro.test(resp_EXP2_2.4.6$Resp_individually_all.csv) # not normal
hist(resp_EXP2_2.4.6$FINALresp) # positive or right skewed - need to transform the data
# transform via sqrt for normal distrubtion via shapro wilk test
resp_EXP2_2.4.6$sqrt_FINALresp <- sqrt(resp_EXP2_2.4.6$FINALresp)
shapiro.test(resp_EXP2_2.4.6$sqrt_FINALres) #  transformation is now normal
hist(resp_EXP2_2.4.6$sqrt_FINALres) # view histogram of sqrt transformation
# Three-way ANOVA (not transformed)
resp_EXP2_2.4.6$Day <- as.factor(resp_EXP2_2.4.6$Day) # treat day as a character 
EXP2.ANOVA <- aov(FINALresp ~ Init.treat*Sec.treat*Day, data = resp_EXP2_2.4.6) # run anova on treatment and time
anova(EXP2.ANOVA) # significant effect of time and secondary treatment
# diagnostic tests and plots of model residuals (not transformed)
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.ANOVA)) #plot histogram of residuals
shapiro.test(residuals(EXP2.ANOVA)) # Shapiro test - residuals are not normal
library(car)
leveneTest(EXP2.ANOVA) ## Levene's test for homogeneity  - residuals are normal
boxplot(residuals(EXP2.ANOVA)) #plot boxplot of residuals
plot(fitted(EXP2.ANOVA),residuals(EXP2.ANOVA)) #display residuals versus fitter, normal QQ plot, leverage plot
qqnorm(residuals(EXP2.ANOVA)) # QQ plot

# post-hoc
exp2.resp.ph <- lsmeans(EXP2.ANOVA, pairwise ~  Init.treat*Sec.treat*Day)# pariwise Tukey Post-hoc test between repeated treatments
exp2.resp.ph # view post hoc summary
E1.pairs.RESP.05 <- cld(exp2.resp.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.RESP.05 # NO PAIRWISE DIFFERENCES
E1.pairs.RESP.1 <- cld(exp2.resp.ph, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.1
E1.pairs.RESP.1 # maringal differences between elevated (day 5) and ambient (days 2 and 5)

