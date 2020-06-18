library(tidyverse) #for all data wrangling
library(stargazer)
library(lme4) #for lmer & glmer models
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions
library(ggeffects)



setwd("~/Dropbox (MIT)/projects/task_dependent_WOC/write-up/arxiv/data/")


######################## robustness_check
data_robust.omega_01_n_5 <- read.csv('../data/robustness_data/prior_research_group_robust_omega_0.1_n_5.csv')
data_robust.omega_033_n_50 <- read.csv('../data/robustness_data/prior_research_group_robust_omega_0.33_n_50.csv')
data_robust.omega_1_n_35 <- read.csv('../data/robustness_data/prior_research_group_robust_omega_1.0_n_35.csv')

model.omega_01_n_5<- lmer(revised_abs_error_zscore ~ Omega_hat*influence + (1 | group_id), 
                          data = data_robust.omega_01_n_5)

model.omega_033_n_50<- lmer(revised_abs_error_zscore ~ Omega_hat*influence + (1 | group_id), 
                            data = data_robust.omega_033_n_50)

model.omega_1_n_35<- lmer(revised_abs_error_zscore ~ Omega_hat*influence + (1 | group_id), 
                          data = data_robust.omega_1_n_35)


tab_model(model.omega_01_n_5,model.omega_033_n_50,model.omega_1_n_35,
          collapse.ci = TRUE,
          pred.labels = c("Intercept", "Omega", "Influence [0, 1]", "Omega x Influence"),
          dv.labels = c("omega=0.1, n=5", "omega=1/3, n=50", "omega=1, n=35")
)


stargazer(c(model.omega_01_n_5,model.omega_033_n_50,model.omega_1_n_35),
          title="Robustness checks (by varying $omega$ and $n$ when calculating the task environment, $Omega$, from the empirical data) for the marginal effect of the interaction term between the centralization of influence and task environment on group performance---in terms of standardized absolute error. Each datapoint is an experimental trial. The results are from a mixed effect model with a random effect for the group ID.",
          covariate.labels = c("Omega", "Influence [0, 1]", "Omega x Influence", "Intercept"),
          column.labels =  c("omega=0.1, n=5", "omega=1/3, n=50", "omega=1, n=35"),
          dep.var.labels = "Standardized Absolute Error"
          )

plot_model(model.omega_01_n_5, type = "int")
plot_model(model.omega_033_n_50, type = "int")
plot_model(model.omega_1_n_50, type = "int")


########################
data_robust.omega_01_n_5  <- data_robust.omega_01_n_5 [!(data_robust.omega_01_n_5 $influence==0),]
data_robust.omega_033_n_50  <- data_robust.omega_033_n_50 [!(data_robust.omega_033_n_50 $influence==0),] #we can't say anything about solo as they can't improve
data_robust.omega_1_n_35  <- data_robust.omega_1_n_35 [!(data_robust.omega_1_n_35 $influence==0),] #we can't say anything about solo as they can't improve


m.omega_01_n_5 <- glmer(improved ~ Omega_hat +  (1 | group_id),
           data = data_robust.omega_01_n_5, family = binomial)

m.omega_033_n_50 <- glmer(improved ~ Omega_hat +  (1 | group_id),
            data = data_robust.omega_033_n_50, family = binomial)

m.omega_1_n_35 <- glmer(improved ~ Omega_hat +  (1 | group_id),
            data = data_robust.omega_1_n_35, family = binomial)

stargazer(c(m.omega_01_n_5,m.omega_033_n_50,m.omega_1_n_35),
          title="Robustness checks (by varying $omega$ and $n$ when calculating the task environment, $Omega$, from the empirical data) for the effect of the task environment on group performance after social interaction. Each datapoint is an experimental trial. The results are from a mixed effect model with a random effect for the group ID.",
          covariate.labels = c("Omega", "Intercept"),
          column.labels =  c("$omega=0.1, n=5$", "$omega=1/3, n=50$", "$omega=1, n=35$"),
          dep.var.labels = "Whether the group improved after social interaction"
)


