library(tidyverse) #for all data wrangling
library(cowplot) #for manuscript ready figures
library(lme4) #for lmer & glmer models
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(ggeffects)


setwd("~/Dropbox (MIT)/projects/task_dependent_WOC/write-up/arxiv/data/")

data <- read.csv('../data/empirical_data/prior_research_group.csv')


#### interaction on absolute error
model.interaction <- lmer(revised_abs_error_zscore ~ Omega_hat*influence + (1 | group_id), 
                     data = data)

tab_model(model.interaction)
plot_model(model.interaction, type = "int")



#probability of improvement

data <- data[!(data$influence==0),] #we can't say anything about solo as they can't improve

m <- glmer(improved ~ Omega_hat +  (1 | group_id) +  (1 | dataset),
           data = data, family = binomial)

summary(m)
sjPlot::tab_model(m, show.re.var= TRUE)

pred=ggpredict(m, "Omega_hat") %>%
  plot() +
  labs(x="Omega (Fitted)", y="Prob. of Improvement")

pred
