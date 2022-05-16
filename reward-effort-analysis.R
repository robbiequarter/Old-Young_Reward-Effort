### Statistical Analyses for Reward-Effort experiment ###

### Import Libraries
  pacman::p_load(knitr, tidyverse, magrittr, kableExtra, GGally, 
                 car, lmerTest, lme4, rstatix, ggpubr, atable, cowplot, 
                 sjPlot, glmmTMB)

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
  R.exp$sTRIAL <- scale(R.exp$TRIAL) # scaled trial
  
  # For later use in binning reaches
  R.exp2 <- R.exp
  R.exp2[,6:10][R.exp2[,6:10] == 99 | R.exp2$MVTDUR < 0] <- NA
  
  # Filter out stuff flagged with a 99 (from MATLAB processing)
  R.exp <- filter(R.exp, PEAKV != 99, MVTDUR > 0) 
  R.pref <- filter(R.pref, PEAKV != 99, MVTDUR > 0)
  
  # Create dataframe of p-vals for correcting stuff later on
  pvals <- data.frame(measure = character(), beta = character(), pval = double(), rank=integer(), apval = double())
  
  # Colors  
  #tuftePal <- c("#367a37", "#cc6600", "#333333","#990033")
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
  
### Plot distribution of dependent variables
  ggplot(R.exp, aes(x=VIG)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   color="black", fill="white") +
    geom_density(alpha=.3, fill="#FF6666")  # Overlay with transparent density plot
  
  ggplot(R.exp, aes(x=RXNEX)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  
  ggplot(R.exp, aes(x=MVTDUR)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  
  ggplot(R.exp, aes(x=PEAKV)) + 
    geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                   colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot
  
  
### Statistical Analyses for Reward-Effort experiment
  # Peak velocity
    # initial model
    pv.lmer <- lmer(PEAKV ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(pv.lmer)
    plot(pv.lmer)
    plot_model(pv.lmer, type = "pred") # plot main effects with sjPlot
    plot_model(pv.lmer, type = 'int') # plot interaction 
    
    # model 2
    pv.lmer2 <- lmer(PEAKV ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, REML = FALSE)
    summary(pv.lmer2)
    
    #model 3
    pv.lmer3 <- glmer(PEAKV ~ MASS*REWD + (1|ID), data = R.exp, family=Gamma(log))
    summary(pv.lmer3)
    plot_model(pv.lmer3, vline.color = "black")
    plot_model(pv.lmer3, type = "int")
    plot_model(pv.lmer3, type = "re", vline.color="black")
    plot_model(pv.lmer3, type = "pred")
    
    pv.lmer4 <- glmer(PEAKV ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, family=Gamma(log))
    summary(pv.lmer4)
  
  # Reaction time
    # initial model
    rxn.lmer <-  lmer(RXNEX ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(rxn.lmer)

    # model 2
    rxn.lmer2 <-  lmer(RXNEX ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, REML = FALSE)
    summary(rxn.lmer2)
    
    # model 3
    rxn.lmer3 <- glmer(RXNEX ~ MASS*REWD + (1|ID), data = R.exp, family=Gamma(log))
    summary(rxn.lmer3)
    plot_model(rxn.lmer3, vline.color = "black")
    plot_model(rxn.lmer3, type = "int")
    plot_model(rxn.lmer3, type = "re", vline.color="black")
    plot_model(rxn.lmer3, type = "pred")
    
    rxn.lmer4 <- glmer(RXNEX ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, family=Gamma(log))
    summary(rxn.lmer4)
    
  # Movement time
    # initial model
    dur.lmer <-  lmer(MVTDUR ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(dur.lmer)

    # model 2
    dur.lmer2 <-  lmer(MVTDUR ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, REML = FALSE)
    summary(dur.lmer2)
    
    # model 3
    dur.lmer3 <- glmer(MVTDUR ~ MASS*REWD + (1|ID), data = R.exp, family=Gamma(log))
    summary(dur.lmer3) 
    plot_model(dur.lmer3, vline.color = "black")
    plot_model(dur.lmer3, type = "int")
    plot_model(dur.lmer3, type = "re", vline.color="black")
    plot_model(dur.lmer3, type = "pred")
    
    dur.lmer4 <- glmer(MVTDUR ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, family=Gamma(log))
    summary(dur.lmer4)
    
  # Overall vigor
    # initial model
    vig.lmer <-  lmer(VIG ~ MASS*REWD + (1|ID), data = R.exp, REML = FALSE)
    summary(vig.lmer)

    # model 2
    vig.lmer2 <-  lmer(VIG ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, REML = FALSE)
    summary(vig.lmer2)  
   
    # model 3
    vig.lmer3 <- glmer(VIG ~ MASS*REWD + (1|ID), data = R.exp, family=Gamma(log))
    summary(vig.lmer3)   
    plot_model(vig.lmer3, vline.color = "black")
    plot_model(vig.lmer3, type = "int")
    plot_model(vig.lmer3, type = "re", vline.color="black")
    plot_model(vig.lmer3, type = "pred")
    
    vig.lmer4 <- glmer(VIG ~ MASS*REWD + MASS*sTRIAL + REWD*sTRIAL + (1|ID), data = R.exp, family=Gamma(log))
    summary(vig.lmer4)
  
  
  
  