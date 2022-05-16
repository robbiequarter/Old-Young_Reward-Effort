### Statistical Analyses for Reward-Effort experiment ###

### Import Libraries
  pacman::p_load(knitr, tidyverse, magrittr, kableExtra, GGally, 
                 car, lmerTest, lme4, rstatix, ggpubr, atable, cowplot, 
                 sjPlot, glmmTMB)

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
  df$sTrial <- scale(df$Trial) # scaled & centered trial variable
  
  
### Plot distribution of dependent variables
  ggplot(df, aes(x=VIG)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   color="black", fill="white") +
    geom_density(alpha=.3, fill="#FF6666")  # Overlay with transparent density plot
  
  ggplot(df, aes(x=Rxn)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  
  ggplot(df, aes(x=Duration)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  
  ggplot(df, aes(x=PeakV)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  
  
  
### Statistical Analyses for Young/Old experiment
  # Vigor
    vig.lmer <- lmer(VIG ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(vig.lmer)
    
    vig.lmer2 <- lmer(VIG ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(vig.lmer2)
    
    vig.lmer3 <- glmer(VIG ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, family = Gamma(log))
    summary(vig.lmer3)
    plot_model(vig.lmer3, vline.color = "black")
    plot_model(vig.lmer3, type = "int")
    plot_model(vig.lmer3, type = "re", vline.color="black")
  
  # Reaction time
    rxn.lmer <- lmer(Rxn ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(rxn.lmer)
  
    rxn.lmer2 <- lmer(Rxn ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(rxn.lmer2)
    
    rxn.lmer3 <- glmer(Rxn ~ Reward*Old+ (1 | Subj), data=df, family = Gamma(log))
    summary(rxn.lmer3)
    plot_model(rxn.lmer3, vline.color = "black")
    plot_model(rxn.lmer3, type = "int")
    plot_model(rxn.lmer3, type = "re", vline.color="black")
    #plot_model(rxn.lmer3, type = "pred")
  
    rxn.lmer4 <- glmer(Rxn ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, family = Gamma(log))

  # Duration
    dur.lmer <- lmer(Duration ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(dur.lmer)
    
    dur.lmer2 <- lmer(Duration ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(dur.lmer2)
    
    dur.lmer3 <- glmer(Duration ~ Reward*Old + (1 | Subj), data=df, family = Gamma(log))
    summary(dur.lmer3)
    plot_model(dur.lmer3, vline.color = "black")
    plot_model(dur.lmer3, type = "int")
    plot_model(dur.lmer3, type = "re", vline.color="black")
    plot_model(dur.lmer3, type = "pred")
    
    dur.lmer4 <- glmer(Duration ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, family = Gamma(log))
    summary(dur.lmer4)
    
  # Peak velocity
    pv.lmer <- lmer(PeakV ~ Reward*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(pv.lmer)
    
    pv.lmer2 <- lmer(PeakV ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, REML=FALSE, na.action=na.omit)
    summary(pv.lmer2)
    
    pv.lmer3 <- glmer(PeakV ~ Reward*Old + (1 | Subj), data=df, family = Gamma(log))
    summary(pv.lmer3)
    plot_model(pv.lmer3, vline.color = "black")
    plot_model(pv.lmer3, type = "int")
    plot_model(pv.lmer3, type = "re", vline.color="black")
    plot_model(pv.lmer3, type = "pred")
    
    pv.lmer4 <- glmer(PeakV ~ Reward*Old + sTrial*Reward + sTrial*Old + (1 | Subj), data=df, family = Gamma(log))

  