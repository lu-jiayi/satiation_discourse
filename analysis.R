this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(lmerTest)
library(bootstrap)
library(ggpubr)
library(stringr)
library(brms)
library(BayesFactor)
library(ordinal)
library(devtools)

if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}
devtools::install_github("DejanDraschkow/mixedpower") 
library(mixedpower)


theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm)}
`%notin%` <- Negate(`%in%`)


raw_data_path <- "data.csv"
data<-read.csv(raw_data_path)

data_acc <- data%>%
  filter(trial_type == "acceptability")

data_neg <- data%>%
  filter(trial_type %in% c("negation_pre", "negation_post"))
###############
#Acceptability#
###############

data_acc_nofill <- data_acc %>%
  filter(structure %in% c("island", "nonisland")) %>%
  mutate(block = (order-25)%/% 12 + 1) %>%
  group_by(workerid)%>%
  mutate(zscore = (response - mean(response)) / sd(response)) %>%
  ungroup()
data_acc_nofill$structure <- as.factor(data_acc_nofill$structure)
data_acc_nofill$structure <- factor(data_acc_nofill$structure, levels = c("nonisland", "island"))
data_acc_nofill$dependency_length <- as.factor(data_acc_nofill$dependency_length)
data_acc_nofill$dependency_length <- factor(data_acc_nofill$dependency_length, levels = c("short", "long"))

acc_summary <- data_acc_nofill%>%
  group_by(block, dependency_length, structure) %>%
  summarise(response = mean(response), zscore = mean(zscore))%>%
  ungroup()


cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")



#raw rating plot
island_raw_plot <- ggplot(data_acc_nofill, aes(x = block, y=response, linetype = structure, fill=dependency_length)) +
  geom_point(data=acc_summary,alpha=.9) +
  xlab("block number") +
  ylab("average acceptability")+
  geom_smooth(method=lm) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()
island_raw_plot

#z-score plot
island_zscore_plot <- ggplot(data_acc_nofill, aes(x = block, y=zscore, linetype = structure, fill=dependency_length)) +
  geom_point(data=acc_summary,alpha=.9) +
  xlab("block number") +
  ylab("average acceptability")+
  geom_smooth(method=lm) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()
island_zscore_plot

#DD score plot
data_island_dd <- data_acc_nofill %>%
  group_by(block, workerid) %>%
  mutate(
    long_nonisl = mean(zscore[structure == "nonisland" & dependency_length == "long"]),
    long_isl = mean(zscore[structure == "island" & dependency_length == "long"]),
    short_nonisl = mean(zscore[structure == "nonisland" & dependency_length == "short"]),
    short_isl = mean(zscore[structure == "island" & dependency_length == "short"]),
    DD = (long_nonisl - long_isl) - (short_nonisl - short_isl)
  ) %>%
  ungroup() %>%
  select(-long_nonisl, -long_isl, -short_nonisl, -short_isl) 
block_means_dd = data_island_dd %>%
  group_by(block) %>%
  summarize(DD = mean(DD)) %>%
  ungroup()
DD_plot <- ggplot(data_island_dd, aes(x = block, y=DD)) +
  geom_point(data=block_means_dd,alpha=.9) +
  xlab("Block number") +
  ylab("Average DD score")+
  geom_smooth(method=lm) +
  scale_fill_manual(values=cbPalette) +
  theme_bw()
DD_plot


################
#Negation Tests#
################

neg_nofill <- data_neg %>%
  filter(sentence_id < 2000)

neg_nofill$trial_type <- as.factor(neg_nofill$trial_type)
neg_nofill$trial_type <- factor(neg_nofill$trial_type, levels = c("negation_pre", "negation_post"))
neg_nofill$polarity <- as.factor(neg_nofill$polarity)


neg_nofill_summary <- neg_nofill %>%
  group_by(polarity, trial_type) %>%
  summarise(Mean = mean(response),
            CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup()%>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)


cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")


neg_line_plot <- 
  ggplot(neg_nofill_summary, aes(x = trial_type, y = Mean, color = polarity, group = polarity)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_errorbar(aes(ymin = YMin, ymax = YMax), width = 0.5, linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = cbPalette, name = "Polarity") + 
  theme_bw() +
  scale_x_discrete(labels = c(
    "negation_pre"  = "Pre-exposure",
    "negation_post" = "Post-exposure"
  ))+
  xlab("Task Position") +
  ylab("Mean Negation score") +
  theme(legend.position = "bottom") +
  ggtitle("Negation Score Plot") +
  ylim(0,1)
neg_line_plot

neg_bar_plot <- 
  ggplot(neg_nofill_summary, 
         aes(x = trial_type, y = Mean, fill = polarity)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = YMin, ymax = YMax),
                position = position_dodge(width = 0.8),
                width = 0.3,
                linewidth = 1,
                show.legend = FALSE) +
  scale_fill_manual(values = cbPalette, name = "Polarity") + 
  theme_bw() +
  scale_x_discrete(labels = c(
    "negation_pre"  = "Pre-exposure",
    "negation_post" = "Post-exposure"
  ))+
  xlab("Task Position") +
  ylab("Mean Negation score") +
  theme(legend.position = "bottom") +
  ggtitle("Negation Score Plot") +
  ylim(0, 1)
neg_bar_plot

#####################
#Acceptability Stats#
#####################

#centering and scaling predictors
data_acc_nofill$block <- scale(data_acc_nofill$block, center = TRUE, scale = FALSE)
data_acc_nofill$dependency_length <- relevel(data_acc_nofill$dependency_length, ref = "short")
data_acc_nofill$structure <- relevel(data_acc_nofill$structure, ref = "nonisland")
contrasts(data_acc_nofill$structure) <- contr.sum(2)
contrasts(data_acc_nofill$dependency_length) <- contr.sum(2)

#lmer
lmer_island_raw <- lmer(response~block*structure*dependency_length+ 
                             (1+block*structure*dependency_length|workerid)+
                             (1+block*structure*dependency_length|lexicalization), 
                           data = data_acc_nofill)
summary(lmer_island_raw )

lmer_island_z <- lmer(zscore~block*structure*dependency_length+ 
                          (1|workerid)+
                          (1|lexicalization), 
                        data = data_acc_nofill)
summary(lmer_island_z )

#brms
brms_island_raw <- brm(response~block*structure*dependency_length+ 
                          (1|workerid)+
                          (1|lexicalization), 
                        data = data_acc_nofill, family = gaussian())
summary(brms_island_raw)


################
#Negation Stats#
################
neg_nofill$trial_type <- relevel(neg_nofill$trial_type, ref = "negation_pre")
neg_nofill$polarity <- relevel(neg_nofill$polarity, ref = "base")
contrasts(neg_nofill$trial_type) <- contr.sum(2)
contrasts(neg_nofill$polarity) <- contr.sum(2)
neg_nofill <- neg_nofill%>%
  mutate(item = (sentence_id - 1000)%/%10 +1)
lmer_neg <- lmer(response~trial_type*polarity+ 
                          (1+trial_type*polarity|workerid) +
                   (1|item), 
                        data = neg_nofill)
summary(lmer_neg)


####################
#Neg Power Analysis#
####################
run_neg_power_mixedpower <- function(steps   = c(40, 60, 80, 100, 120),
                                        n_sim  = 500,
                                        alpha  = 0.05,
                                        SESOI  = FALSE,
                                        databased = TRUE,
                                        make_plot = TRUE) {
  # two-sided alpha => critical t/z value
  crit <- qnorm(1 - alpha/2)
  
  power_res <- mixedpower(
    model         = lmer_neg,
    data          = neg_nofill,
    fixed_effects = c("trial_type", "polarity"),
    simvar        = "workerid",  
    steps         = steps,     
    critical_value = crit,
    n_sim         = n_sim,
    SESOI         = SESOI,
    databased     = databased
  )
  
  if (make_plot) {
    multiplotPower(power_res)
  }
  
  return(power_res)
}

power_table <- run_neg_power_mixedpower(
  steps  = c(200,300),
  n_sim  = 100,
  alpha  = 0.05
)

power_table
