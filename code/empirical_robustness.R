# clear environment
rm(list = ls())

# install + load packages
pkgs <- c("tidyverse", "cowplot", "lme4", "sjPlot", 
          "sjmisc", "effects", "sjstats", "ggeffects",
          "stargazer", "e1071", "fitdistrplus", "evir")

for (p in pkgs){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
rm(p, pkgs)

## Calculate skewness, kurtosis, and log-laplace rel. log lik. for each task ----
# read in data from prior research 
# assumes you've downloaded GitHub master folder to working directory
df <- read.csv('./data/empirical_data/prior_research.csv')

# create vector with task ids
tasks <- unique(df$task_id) %>% as.character()

## Skewness ----

# empty df for results
skews <- data.frame(task_id = NA, skew = NA)

# loop through tasks and calc skewness with e1071::skewness
for(i in c(1:54)){
  # get estimates for a particular task
  estimates <- df %>% 
    filter(task_id == tasks[i]) %>%
    filter(!is.na(pre_influence)) 
  
  # calculate skewness
  s <- skewness(estimates$pre_influence)
  
  # create row with task id and skewness
  task_skew <- c(paste0(tasks[i]), s)
  
  # bind to dataframe
  skews <- as.data.frame(rbind(skews, task_skew))
  
  # remove row with NAs on first iteration
  if(i==1){
    skews <- filter(skews, !is.na(task_id))
  }
}

# tidy up environment
rm(i, s, estimates, task_skew)

# make skewness measures numeric
skews$skew <- as.numeric(skews$skew)

## Kurtosis ----

# empty df for results
kurts <- data.frame(task_id = NA, kurt = NA)

# loop through tasks and calc skewness with e1071::kurtosis
for(i in c(1:54)){
  # get estimates for a particular task
  estimates <- df %>% 
    filter(task_id == tasks[i]) %>%
    filter(!is.na(pre_influence)) 
  
  # calculate kurtosis
  ku <- kurtosis(estimates$pre_influence)
  
  # create row with task id and kurtosis
  task_kurt <- c(paste0(tasks[i]), ku)
  
  # bind to dataframe
  kurts <- as.data.frame(rbind(kurts, task_kurt))
  
  # remove row with NAs on first iteration
  if(i==1){
    kurts <- filter(kurts, !is.na(task_id))
  }
}

# tidy up environment
rm(i, ku, estimates, task_kurt)

# make skewness measures numeric
kurts$kurt <- as.numeric(kurts$kurt)

## Hill estimate of the tail index -----

# empty df for results
hill_ests <- data.frame(task_id = NA, hill_est = NA)

# loop through tasks and calc hill estimate
for(i in c(1:length(tasks))){
  # get estimates for a particular task
  estimates <- df %>% 
    filter(task_id == tasks[i]) %>%
    filter(!is.na(pre_influence)) 
  
  # Hill estimation
  h <- hill(data = estimates$pre_influence, auto.scale = F)
  
  # in task 46, remove estimate == "Inf"
  if(max(h$y)=="Inf"){
    h$y <- h$y[-length(h$y)]
  }
  
  # create row with task id and average alpha (in lieu of proper k selection)
  task_hill <- c(paste0(tasks[i]), mean(h$y))
  
  # bind to dataframe
  hill_ests <- as.data.frame(rbind(hill_ests, task_hill))
  
  # remove row with NAs on first iteration
  if(i==1){
    hill_ests <- filter(hill_ests, !is.na(task_id))
  }
}

# tidy up environment
rm(i, h, estimates, task_hill)

# make hill estimates numeric
hill_ests$hill_est <- as.numeric(hill_ests$hill_est)

## read in + join data ----
# read in
data <- read.csv('./data/empirical_data/prior_research_group.csv')

# join 
data <- inner_join(data, kurts, by = "task_id")
data <- inner_join(data, skews, by = "task_id")
data <- inner_join(data, hill_ests, by = "task_id")

## interaction models ----

data$IV <- data$kurt
model.kurtosis<- lmer(revised_abs_error_zscore ~ IV*influence + (1 | group_id), 
                          data = data)
tab_model(model.kurtosis)

data$IV <- data$skew
model.skewness<- lmer(revised_abs_error_zscore ~ IV*influence + (1 | group_id), 
                            data = data)
tab_model(model.skewness)

data$IV <- data$hill_est
model.hill<- lmer(revised_abs_error_zscore ~ IV*influence + (1 | group_id), 
                          data = data)

# inspect results
tab_model(model.kurtosis,
          model.skewness,
          model.hill,
          collapse.ci = F,
          pred.labels = c("Intercept", "IV", "Influence [0, 1]", "DV x Influence"),
          dv.labels = c("Kurtosis" 
                        ,"Skewness" 
                        ,"Hill Estimate"
                        ))

## print latex table S2 ----
class(model.kurtosis) <- "lmerMod"
class(model.skewness) <- "lmerMod"
class(model.hill) <- "lmerMod"

stargazer(c(model.kurtosis
            ,model.skewness
            ,model.hill
            ),
          title="Robustness checks (by varying the task environment metric, the independent variable (IV), derived from the empirical data) for the marginal effect of the interaction term between the centralization of influence and task environment on group performance---in terms of standardized absolute error. Each datapoint is an experimental trial. The results are from a mixed effect model with a random effect for the group ID.",
          #covariate.labels = c("IV", "Social Influence [0, 1]", "IV x Social Influence", "Intercept"),
          column.labels =  c("Kurtosis", "Skewness","Hill Estimate"
          ),
          dep.var.labels = "Standardized Absolute Error"
          )

plot_model(model.kurtosis, type = "int")
plot_model(model.skewness, type = "int")
plot_model(model.hill, type = "int")


## logistic models ----
data <- data[!(data$influence==0),] #we can't say anything about solo as they can't improve

data$IV <- data$kurt
m.kurtosis <- glmer(improved ~ IV +  (1 | group_id),
           data = data, family = binomial)

data$IV <- data$skew
m.skewness <- glmer(improved ~ IV +  (1 | group_id),
            data = data, family = binomial)

data$IV <- data$hill_est
m.hill <- glmer(improved ~ IV +  (1 | group_id),
            data = data, family = binomial)

## print latex table S3 ----
class(m.kurtosis) <- "lmerMod"
class(m.skewness) <- "lmerMod"
class(m.hill) <- "lmerMod"
stargazer(c(m.kurtosis,m.skewness,m.hill),
          title="Robustness checks (by varying the task environment metric, the independent variable IV, derived from the empirical data) for the effect of the task environment on group performance after social interaction. Each datapoint is an experimental trial. The results are from a mixed effect model with a random effect for the group.",
          covariate.labels = c("IV", "Intercept"),
          column.labels =  c("Kurtosis", "Skewness"
                             , "Hill Estimate"
                             ),
          dep.var.labels = "Whether the group improved after social interaction"
)


