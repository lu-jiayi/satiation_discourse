this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
source("helper.R")
library(dplyr)
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
#####################
#Data Pre-processing#
#####################
data <- read.csv("cdata.csv")
exclude_wrong_attempt <- data %>%
  dplyr::group_by(workerid) %>%
  dplyr::summarise(
    exclude_wrong = any(wrong_attempts > 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::filter(exclude_wrong) %>%
  dplyr::pull(workerid)
filler_data <- data %>%
  filter(item_type %in% c("filler_good", "filler_bad"))
filler_summary <- filler_data %>%
  group_by(workerid, item_type) %>%
  summarise(
    n = sum(!is.na(response)),
    mean_response = mean(response, na.rm = TRUE),
    sd_response = sd(response, na.rm = TRUE),
    se = sd_response / sqrt(n),
    ci_lower = mean_response - qt(0.975, df = n - 1) * se,
    ci_upper = mean_response + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )

filler_wide <- filler_summary %>%
  select(workerid, item_type, ci_lower, ci_upper) %>%
  tidyr::pivot_wider(
    names_from = item_type,
    values_from = c(ci_lower, ci_upper)
  )
exclude_ci_overlap <- filler_wide %>%
  mutate(
    exclude_overlap = !(
      ci_upper_filler_good < ci_lower_filler_bad |
        ci_upper_filler_bad < ci_lower_filler_good
    )
  ) %>%
  filter(exclude_overlap) %>%
  pull(workerid)
excluded_workerids <- union(exclude_wrong_attempt, exclude_ci_overlap)

###############
#Acceptability#
###############
data_acc <- read.csv("data_acc.csv")
data_acc_nofill <- data_acc %>%
  filter(!workerid %in% excluded_workerids)%>%
  filter(structure %in% c("island", "nonisland")) %>%
  mutate(block = (order - 25) %/% 12 + 1) %>%
  group_by(workerid) %>%
  mutate(zscore = (response - mean(response)) / sd(response)) %>%
  ungroup()
data_acc_nofill$structure <- as.factor(data_acc_nofill$structure)
data_acc_nofill$structure <- factor(data_acc_nofill$structure, levels = c("nonisland", "island"))
data_acc_nofill$dependency_length <- as.factor(data_acc_nofill$dependency_length)
data_acc_nofill$dependency_length <- factor(data_acc_nofill$dependency_length, levels = c("short", "long"))
acc_summary <- data_acc_nofill %>%
  group_by(block, dependency_length, structure) %>%
  summarise(response = mean(response), zscore = mean(zscore), .groups = "drop")

#satiation plot
island_plot <- ggplot(
  data_acc_nofill,
  aes(x = block, y = response, linetype = structure, fill = dependency_length)
) +
  geom_point(data = acc_summary, alpha = .9) +
  xlab("Block number") +
  ylab("Average acceptability") +
  geom_smooth(method = lm) +
  scale_fill_manual(values = cbPalette) +
  labs(fill = "Dependency Length", linetype = "Structure") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
<<<<<<< HEAD
=======
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing = unit(2, "pt")
  )

island_raw_plot

# z-score plot
island_z_plot <- ggplot(
  data_acc_nofill,
  aes(x = block, y = zscore, linetype = structure, fill = dependency_length)
) +
  geom_point(data = acc_summary, alpha = .9) +
  xlab("Block number") +
  ylab("Average acceptability z-score") +
  geom_smooth(method = lm) +
  scale_fill_manual(values = cbPalette) +
  labs(fill = "Dependency Length", linetype = "Structure") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
>>>>>>> f881af37121649ba63d97eae744bac5e4892a87a
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.spacing = unit(2, "pt")
  )

island_plot

# DD score plot
data_island_dd <- data_acc_nofill %>%
  group_by(block, workerid) %>%
  mutate(
    long_nonisl = mean(response[structure == "nonisland" & dependency_length == "long"]),
    long_isl = mean(response[structure == "island" & dependency_length == "long"]),
    short_nonisl = mean(response[structure == "nonisland" & dependency_length == "short"]),
    short_isl = mean(response[structure == "island" & dependency_length == "short"]),
    DD = (long_nonisl - long_isl) - (short_nonisl - short_isl)
  ) %>%
  ungroup() %>%
  select(-long_nonisl, -long_isl, -short_nonisl, -short_isl)
block_means_dd <- data_island_dd %>%
  group_by(block) %>%
  summarize(
    n = n(),
    mean_DD = mean(DD),
    ci_low = ci.low(DD),
    ci_high = ci.high(DD),
    .groups = "drop"
  )
DD_plot <- ggplot(block_means_dd, aes(x = block, y = mean_DD)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(
    aes(ymin = mean_DD - ci_low, ymax = mean_DD + ci_high),
    width = .15
  ) +
  xlab("Block number") +
  ylab("Average DD score") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

DD_plot

################
#Negation Tests#
################
data_neg<-read.csv("data_neg.csv")
neg_nofill <- data_neg %>%
  filter(sentence_id < 2000)
neg_nofill$trial_type <- as.factor(neg_nofill$trial_type)
neg_nofill$trial_type <- factor(neg_nofill$trial_type, levels = c("negation_pre", "negation_post"))
neg_nofill$polarity <- as.factor(neg_nofill$polarity)

neg_nofill_summary <- neg_nofill %>%
  group_by(polarity, trial_type) %>%
  summarise(
    Mean = mean(response),
    CILow = ci.low(response),
    CIHigh = ci.high(response),
    .groups = "drop"
  ) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)

neg_bar_plot <- ggplot(
  neg_nofill_summary,
  aes(x = trial_type, y = Mean, fill = polarity)
) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(
    aes(ymin = YMin, ymax = YMax),
    position = position_dodge(width = 0.8),
    width = 0.3,
    linewidth = 1,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = cbPalette, name = "Polarity") +
  theme_bw() +
  scale_x_discrete(labels = c(
    "negation_pre" = "Pre-exposure",
    "negation_post" = "Post-exposure"
  )) +
  xlab("Task Position") +
  ylab("Mean Negation score") +
  theme(legend.position = "bottom") +
  ylim(0, 1) +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

neg_bar_plot

#####################
#Acceptability Stats#
#####################

data_acc_nofill$block <- scale(data_acc_nofill$block, center = TRUE, scale = FALSE)
data_acc_nofill$dependency_length <- relevel(data_acc_nofill$dependency_length, ref = "short")
data_acc_nofill$structure <- relevel(data_acc_nofill$structure, ref = "nonisland")
contrasts(data_acc_nofill$structure) <- contr.sum(2)
contrasts(data_acc_nofill$dependency_length) <- contr.sum(2)

lmer_island <- lmer(
<<<<<<< HEAD
  response ~ block * structure * dependency_length +
    (1+block * structure * dependency_length| workerid) +
    (1+block * structure * dependency_length| lexicalization),
=======
  zscore ~ block * structure * dependency_length +
    (1| workerid) +
    (1| lexicalization),
>>>>>>> f881af37121649ba63d97eae744bac5e4892a87a
  data = data_acc_nofill
)
summary(lmer_island)
brm_island_z <- brm(
  zscore ~ block * structure * dependency_length +
    (1 + block * structure * dependency_length| workerid) +
    (1 + block * structure * dependency_length | lexicalization),
  data = data_acc_nofill,
  family = gaussian(),
  chains = 4,
  cores = 4,
  iter = 4000,
  warmup = 1000
)

summary(brm_island_z)

################
#Negation Stats#
################
data_neg <- read.csv("data_neg.csv")
neg_nofill$trial_type <- relevel(neg_nofill$trial_type, ref = "negation_pre")
neg_nofill$polarity <- relevel(neg_nofill$polarity, ref = "base")
contrasts(neg_nofill$trial_type) <- contr.sum(2)
contrasts(neg_nofill$polarity) <- contr.sum(2)

neg_nofill <- neg_nofill %>%
  mutate(item = (sentence_id - 1000) %/% 10 + 1)

lmer_neg <- lmer(
  response ~ trial_type * polarity +
    (1 + trial_type * polarity | workerid) +
    (1 + trial_type * polarity | item),
  data = neg_nofill
)

summary(lmer_neg)

