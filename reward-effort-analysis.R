### Statistical Analyses for Reward-Effort experiment ###

### Import Libraries
  pacman::p_load(knitr, tidyverse, magrittr, kableExtra, GGally, 
                 car, lmerTest, lme4, rstatix, ggpubr, atable, cowplot, 
                 sjPlot)

### Import and clean data
  rm(list=ls())
  
  R <- as.data.frame(read.csv('data/reward_table_long.csv'))
  
  # Change factors to factors
  R$ID %<>% as.factor()
  R$MASS %<>% as.factor()
  R$PREF %<>% as.factor()
  R$REWD %<>% as.factor()
  R$TARG %<>% as.factor()
  
  # Add VIGOR
  R$VIG <- 1/(R$MVTDUR + R$RXNEX)
  
  # Number of Subjects
  n <- length(levels(R$ID)) # number of subjs # number of subjs
  
  # Separate Stuff
  R.pref <- filter(R, PREF == 1)
  R.exp <- filter(R, PREF == 0)
  
  # Add Block column
  block <- c(rep(1, 100), rep(2, 100), rep(3,100), rep(4,100))
  R.exp$BLOCK <- rep(block, nrow(R.exp)/400) %>% 
    as.factor()
  
  # Add Trial column
  trial <- seq(1,400)
  R.exp$TRIAL <- rep(trial, n*2)
  
  # For later use in binning reaches
  R.exp2 <- R.exp
  R.exp2[,6:10][R.exp2[,6:10] == 99 | R.exp2$MVTDUR < 0] <- NA
  
  # Filter out stuff flagged with a 99 (from MATLAB processing)
  R.exp <- filter(R.exp, PEAKV != 99, MVTDUR > 0) 
  R.pref <- filter(R.pref, PEAKV != 99, MVTDUR > 0)
  
  # Create dataframe of p-vals for correcting stuff later on
  pvals <- data.frame(measure = character(), beta = character(), pval = double(), rank=integer(), apval = double())
  
  # Colors  
  tuftePal <- c("#367a37", "#cc6600", "#333333","#990033")
  #hist(c(1:5), col=tuftePal)
  
  # Binning every 4 reaches, average 3 NRWD, keep 1 RWD 
  R.bin <- R.exp2[FALSE, c(1,2,6,8,9)]
  for (i in seq(1, nrow(R.exp2), 4)) {
    if (anyNA( R.exp2[i:(i+3),]) == FALSE) {
      temp <- R.exp2[i:(i+3), ] %>%
        aggregate(cbind(PEAKV, RXNEX, MVTDUR) ~ ID + MASS + REWD, data=., function(x) mean(x, na.rm=TRUE))
      R.bin <- rbind(R.bin, temp)    
    }
  }
  
  R.bins <- R.bin %>% 
    group_by(REWD) %>%
    mutate(row=row_number()) %>%
    pivot_wider(., id_cols = c(row,ID,MASS), names_from = REWD, values_from = c("PEAKV", "RXNEX", "MVTDUR"))
  
### Statistical Analyses for Reward-Effort experiment
  # Peak velocity
    # initial model
    lmer.pv <- lmer(PEAKV ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.pv)
    plot(lmer.pv)
    plot_model(lmer.pv, type = "pred") # plot main effects with sjPlot
    plot_model(lmer.pv, type = 'int') # plot interaction 
    
    # model 2
    lmer.pv2 <- lmer(PEAKV ~ TRIAL*MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.pv2)
  
  # Reaction time
    # initial model
    lmer.rxn <-  lmer(RXNEX ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.rxn)
    plot_model(lmer.rxn)
 
    # model 2
    lmer.rxn2 <-  lmer(RXNEX ~ TRIAL*MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.rxn2)     
    
  # Movement time
    # initial model
    lmer.dur <-  lmer(MVTDUR ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.dur)
    plot(lmer.dur)
    
    # model 2
    lmer.dur2 <-  lmer(MVTDUR ~ TRIAL*MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.dur2)
    
  # Overall vigor
    # initial model
    lmer.vig <-  lmer(VIG ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.vig)
    plot(lmer.vig)
    
    # model 2
    lmer.vig2 <-  lmer(VIG ~ TRIAL*MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(lmer.vig2)  
   
  
  
  
  
  