# predicting effect of (de)centralized influence on collective accuracy 
# with relatiove log likelihoods: log-normal likelihood / (log-normal likelihood + normal likelihood) 
# NOTE: to generate the final panel of plots, you should first run `descriptives_table.R`

# clear environment -- except for desc_tab from `descriptives_table.R`
rm(list=setdiff(ls(), "desc_tab"))

# install + load packages
pkgs <- c("tidyverse", "cowplot", "lme4", "sjPlot", 
          "sjmisc", "effects", "sjstats", "ggeffects",
          "stargazer", "e1071", "ggnewscale", "fitdistrplus")

for (p in pkgs){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
rm(p, pkgs)

# read in data from prior research 
# assumes you've downloaded GitHub master folder to working directory
df <- read.csv('./data/empirical_data/prior_research.csv')

# create vector with task ids
tasks <- unique(df$task_id) %>% droplevels() %>% as.character()

# empty df for results
RLLs <- data.frame(task_id = NA, rll = NA)

# loop through tasks and calc rel log likelihoods
for(i in c(1:length(tasks))){
  # get estimates for a particular task
  estimates <- df %>% 
    filter(task_id == tasks[i]) %>%
    filter(!is.na(pre_influence)) 
  
  # fit normal and log-normal dist to data
  fit.norm <- fitdist(estimates$pre_influence+1, distr = "norm", method = "mle")
  fit.lognorm <- fitdist(estimates$pre_influence+1, distr = "lnorm", method = "mle")
  
  # calculate rel log lik
  relLL <- fit.norm$loglik/(fit.lognorm$loglik+fit.norm$loglik)

  # create row with task id and tail weight
  task_rll <- c(paste0(tasks[i]), relLL)

  # bind to dataframe
  RLLs <- as.data.frame(rbind(RLLs, task_rll))
  
  # remove row with NAs on first iteration
  if(i==1){
    RLLs <- filter(RLLs, !is.na(task_id))
  }
}

# tidy up environment
rm(i, estimates, task_rll, fit.lognorm, fit.norm)

# make rel log lik numeric
RLLs$rll <- as.numeric(RLLs$rll)

# merging into old "regression.R" script
# assumes you've downloaded GitHub master folder to working directory
data <- read.csv('./data/empirical_data/prior_research_group.csv')

# join rel log like data to `data`
data <- inner_join(data, RLLs, by = "task_id")
data$influence <- as.factor(data$influence)

#### interaction on absolute error
model.interaction <- lmer(revised_abs_error_zscore ~ rll*influence + (1 | group_id), 
                          data = data)

tab_model(model.interaction, show.stat=TRUE)
plot_model(model.interaction, type = "int", show.data=T)

# make pretty plots
RLLs$study <- with(RLLs, ifelse(str_detect(task_id, "lorenz2017"), "a_lorenz2011",
                                ifelse(str_detect(task_id, "gurcay2015"), "b_gurcay2015",
                                       ifelse(str_detect(task_id, "becker2017"), "c_becker2017", "d_becker2019")))) %>%
  as.factor()

my_cols <- c("a_lorenz2011"="firebrick",
             "b_gurcay2015"="darkgoldenrod1", 
             "c_becker2017"="cornflowerblue",
             "d_becker2019"="azure4")

p_dists <- ggplot(RLLs, aes(x = rll, color = study))+
  geom_density(size = 0.9)+
  geom_rug(alpha = 0.6, size = 1.5)+
  theme_minimal()+
  scale_color_manual(name = "Study", 
                       values = my_cols,
                       labels = c("c_becker2017"="Becker et al. 2017", "d_becker2019"="Becker et al. 2019", 
                                  "b_gurcay2015"="Gürçay et al. 2015", "a_lorenz2011"="Lorenz et al. 2011"))+
  labs(y = "Density")+
  xlab(expression(italic("R")))+
  theme(legend.position = "bottom")+
  xlim(0,1)

my_cols2 <- c("1"="chocolate1", "0"="chartreuse4")  
pdat <- get_model_data(model.interaction, type = "int", show.data=T) %>% as.data.frame()
p_int <- ggplot(pdat, aes(x=x, y=predicted, group=group, color=group))+
  #stat_smooth(method="lm", aes(color = group), fullrange=TRUE, se=T)+
  geom_smooth(method = "lm", show.legend = T)+ 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group, color = NULL), 
              alpha = .1, show.legend = F)+
  scale_color_manual(name = "", values = my_cols2, labels = c("0"="Decentralized","1"="Centralized"))+
  scale_fill_manual(name = "", values = my_cols2, labels = c("0"="Decentralized","1"="Centralized"))+
  new_scale_color()+
  geom_rug(inherit.aes = F, data = RLLs, aes(x=rll, color=study), alpha=0.6, size = 1.5,
           show.legend = F)+
  scale_color_manual(name = "", 
                     values = my_cols,
                     labels = c("c_becker2017"="Becker et al. 2017", "d_becker2019"="Becker et al. 2019", 
                                "b_gurcay2015"="Gürçay et al. 2015", "a_lorenz2011"="Lorenz et al. 2011"),
                     guide = F)+  
  labs(y = "Standardized Absolute Error")+
  xlab(expression(italic("R")))+
  annotate(x=0, y=2.2, geom="text",
           label="N = 687", hjust = 0)+
  annotate(x=0, y=2.05, geom="text",
           label="t-statistic = -3.95", hjust = 0)+
  annotate(x=0, y=1.9, geom="text",
           label="italic(p) < 0.001", parse = T, hjust = 0)+
  xlim(0,1)+
  theme_minimal()+
  theme(legend.position = c(0.24, 0.8))


#probability of improvement

data <- data[!(data$influence==0),] # we can't say anything about solo as they can't improve

m <- glmer(improved ~ rll +  (1 | group_id),
           data = data, family = binomial)

summary(m)
sjPlot::tab_model(m, show.re.var= TRUE, show.stat=TRUE)


mydf <- ggpredict(m, "rll [all]", add.data = TRUE)
p_pred <- ggplot(mydf, aes(x, predicted)) +
  geom_line() +
  #stat_smooth(method="lm", fullrange=TRUE, se=T, color = "black")+
  ylim(0, 1) +
  xlim(0, 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  new_scale_color()+
  geom_rug(inherit.aes = F, data = RLLs, aes(x=rll, color=study), alpha=0.6, size = 1.5,
           show.legend = F)+
  scale_color_manual(name = "", 
                     values = my_cols,
                     labels = c("c_becker2017"="Becker et al. 2017", "d_becker2019"="Becker et al. 2019", 
                                "b_gurcay2015"="Gürçay et al. 2015", "a_lorenz2011"="Lorenz et al. 2011"),
                     guide = F)+  
  labs(y ="\u03a9")+
  xlab(expression(italic("R")))+
  annotate(x=0, y=1, geom="text",
           label="N = 582", hjust = 0)+
  annotate(x=0, y=0.955, geom="text",
           label="z-statistic = 5.26", hjust = 0)+
  annotate(x=0, y=0.905, geom="text",
           label="italic(p) < 0.001", parse = T, hjust = 0)+
  theme_minimal()

class(model.interaction) <- "lmerMod"
class(m) <- "lmerMod"
stargazer(c(m, model.interaction), 
          title="The main effects of task environment (relative log likelihood; R) and the interaction with social influence (i.e., centralization) Each datapoint is an experimental trial. The results are from a mixed effect model with a random effect for the group",
          covariate.labels = c("R", "Influence", "R x Influence", "Intercept"),
          column.labels =  NULL,
          dep.var.labels = c("Group improved after social interaction", "Standardized Absolute Error")
)


# create + save panel
t_row <- cowplot::plot_grid(desc_tab, 
                            p_dists+theme(legend.position = "none"), 
                            ncol = 1, labels = c("A", "B"), rel_heights = c(0.6, 1))
b_row <- cowplot::plot_grid(p_pred, 
                            p_int, 
                            ncol = 2, labels = c("C", "D"))
col_leg <- cowplot::get_legend(p_dists+theme(legend.position = "bottom"))

#png("fig3.png", width = 6.5, height = 9.1, units = "in", res = 500)
cowplot::plot_grid(t_row, col_leg, b_row, ncol = 1, rel_heights = c(1, 0.1, 1))
#dev.off()
