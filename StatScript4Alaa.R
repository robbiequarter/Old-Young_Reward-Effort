#Loads R-packages used for stats.
library("lme4")
library("multcomp")
library("stats4")
library(sjPlot)


#Delete all variables
rm(list=ls())

# Read in text files (added by AAA 04/26/2022)
GrossMet1212 <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/GrossMet1212.txt")
newjerk <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/newjerk.txt")
Aged_RewardLMER0418 <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/Aged_RewardLMER0418.txt")
SumTorque <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/SumTorque.txt")

#Calculating stats for effect of resting rate, age, velocity and distance on gross metabolic rate
#attach(GrossMet1212)
GrossMet1212$Vel=(0.2-GrossMet1212$Dist*0.1)/GrossMet1212$Dur
GrossMet1212$FactSubj=factor(GrossMet1212$Subj)
Gross.lmer <- lmer(log(Gross)~ Rest + Old  + log(Vel) + Dist   + (1| FactSubj), data=GrossMet1212, REML=FALSE, na.action=na.omit)
cftest(Gross.lmer)
summary(Gross.lmer)
rm(GrossMet1212)


#Calculates effect of age, velocity and distance on y-error, x-error, and jerk
#attach(newjerk)
newjerk$vel=(0.2-newjerk$dist*0.1)/newjerk$dur
newjerk$FactSubj=factor(newjerk$subj)
Acc.lmer <- lmer(y~ age  + vel + dist + (1| FactSubj), data=newjerk, REML=FALSE, na.action=na.omit)
cftest(Acc.lmer)
summary(Acc.lmer)
rm(newjerk)

#Calculates effect of age, trial, and reward on reaction time, peak velocity, and duration
#attach(Aged_RewardLMER0418)
dataset=Aged_RewardLMER0418
Aged_RewardLMER0418$FactSubj=factor(Aged_RewardLMER0418$Subj)
#reward.lmer <- lmer(Duration~ Reward + Trial + Old + Reward*Old + Reward*Trial +Old*Trial + (1 | FactSubj), data=Aged_RewardLMER0418, REML=FALSE, na.action=na.omit)
#cftest(reward.lmer)
reward.lmer <- lmer(Rxn~ Reward + Old + Reward*Old + (1 | FactSubj), data=Aged_RewardLMER0418, REML=FALSE, na.action=na.omit)
cftest(reward.lmer)
summary(reward.lmer)
anova(reward.lmer)
plot_model(reward.lmer,type="int")
rm(Aged_RewardLMER0418)

#Calculates effect of age, distance, and velocity on sum of torque squared(?)
#attach(SumTorque)
Parts=factor(SumTorque$SubjNum)
SumTorque$FactSubj=factor(SumTorque$SubjNum)
dataset=SumTorque

torque.lmer <- lmer(Torque~ Old + Dist + log(Vel) + (1 | FactSubj), data=dataset, REML=FALSE, na.action=na.omit)
cftest(torque.lmer)
summary(torque.lmer)
rm(SumTorque)


