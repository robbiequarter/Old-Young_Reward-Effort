### Statistical Analyses for Reward-Effort experiment ###

### Import Libraries
  pacman::p_load(knitr, tidyverse, magrittr, kableExtra, GGally, 
                 car, lmerTest, lme4, rstatix, ggpubr, atable, cowplot, 
                 sjPlot)

### Import and clean data
  #Delete all variables
  rm(list=ls())
  
  #GrossMet1212 <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/GrossMet1212.txt")
  #newjerk <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/newjerk.txt")
  #SumTorque <- read.delim("~/Downloads/SumShaAhm_InPrep/Statistics/SumTorque.txt")
  df <- as.data.frame(read.delim("data/Aged_RewardLMER0418.txt"))

  # Add Vigor
  df$VIG <- 1/(df$Rxn+df$Duration)
  
  # Make things factors
  df$Reward %<>% as.factor()
  df$Old %<>% as.factor()
  df$Subj %<>% as.factor()
  df$Block %<>% as.factor()
  
  ### Statistical Analyses for Young/Old experiment
    # Vigor
    vig.lmer <- lmer(VIG ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(vig.lmer)
    
    # Reaction time
    rxn.lmer <- lmer(Rxn ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(rxn.lmer)
  
    # Duration
    dur.lmer <- lmer(Duration ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(dur.lmer)
  
    # Peak velocity
    pv.lmer <- lmer(PeakV ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(pv.lmer)
  
  