# ==============================================================================
# Study 1A (chimpanzees) - Main analysis
# ==============================================================================
#
# Purpose:  Tests whether chimpanzees peek into the unchosen box after a
#           decision, and whether peeking depends on condition (choice,
#           no-choice, control) and on the value of the reward received.
# Input:    chimpanzees/study1/data/data_study1A_chimpanzees.csv
# Output:   Results of Study 1A, Figure 2 and Figure S1 (saved to figures/),
#           additional analyses for the reviewers (Section 12, reported in the SI),
#           chimpanzees/study1/R_objects/analysis_1.RData
# Run from: Project root (open Post_Decisional_Curiosity.Rproj first)
# Runtime:  The bootstraps (Section 9 and 11.5) take a long time. To skip
#           them, load the saved workspace (see Section 2).
#
# ==============================================================================

rm(list = ls())
set.seed(123)

# 1. Setup ----

## 1.1 Packages ----
# Install missing packages, then load all of them
required_packages <- c(
  "lme4", "readxl", "tidyverse", "tidyselect", "parallel", "optimx", "emmeans", "car",
  "irr"
)
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

## 1.2 Helper functions ----
# GLMM utility functions by Roger Mundry (see README)
source("./chimpanzees/study1/functions/diagnostic_fcns.r")
source("./chimpanzees/study1/functions/glmm_stability.r")
source("./chimpanzees/study1/functions/drop1_para.r")
source("./chimpanzees/study1/functions/boot_glmm.r")

# 2. Load data ----

# Optional: load all saved objects (models, bootstraps, plots) instead of
# re-running the long computations
# load("./chimpanzees/study1/R_objects/analysis_1.RData")

xdata <-
  read.csv2("./chimpanzees/study1/data/data_study1A_chimpanzees.csv",
    header = TRUE, na = c("NA", "")
  )

# Remove trailing spaces from all text columns
xdata <- as.data.frame(
  lapply(xdata, function(col) {
    if (is.character(col)) {
      sub("\\s+$", "", col)
    } else {
      col
    }
  }),
  stringsAsFactors = FALSE
)

# 3. Data preparation ----

## 3.1 Factors and reference levels ----
xdata$searched <-
  as.factor(xdata$searched)
xdata$condition <-
  factor(xdata$condition,
    levels = c("control", "choice", "no-choice")
  )
xdata$value.remaining.seen <-
  as.factor(xdata$value.remaining.seen)
xdata$value.remaining.seen <-
  relevel(xdata$value.remaining.seen, ref = "worse")

## 3.2 Turning away after peeking ----
# Turning away is only analysed for peeks for which it could be coded from the
# video (turned.away = yes/no). Peeks with turned.away = NA (e.g., video missing
# or chimpanzee out of view) are excluded.
xdata$turn.away.coded <- xdata$searched == "yes" & !is.na(xdata$turned.away)

# Turned away within 2 s of the last look at the remaining reward (yes/no;
# preregistered criterion)
xdata$turned.away.within.2 <- NA
xdata$turned.away.within.2[xdata$turn.away.coded] <- "no"
xdata$turned.away.within.2[xdata$turn.away.coded &
  xdata$turned.away == "yes" &
  xdata$time.from.last.gaze.to.back.of.head.to.camera <= 2] <- "yes"

## 3.3 Trial numbers ----
# trial.per.condition: running trial number per chimpanzee within each condition
# trial.total: running trial number per chimpanzee across the whole study
xdata <- xdata[order(xdata$name, xdata$day, xdata$trial), ]
xdata$trial.per.condition <-
  as.numeric(ave(xdata$name,
    list(xdata$name, xdata$condition),
    FUN = seq_along
  ))
xdata$trial.total <-
  as.numeric(ave(xdata$name,
    list(xdata$name),
    FUN = seq_along
  ))

ftable(searched ~ turned.away.within.2, xdata)

ftable(searched ~ turned.away, xdata)

# 4. Latency to turn away after peeking (Figure S1) ----
# In addition to the preregistered criterion (turning away within 2 s), we
# describe the distribution of latencies from the last look at the remaining
# reward until turning away.
# Only peeks for which turning away could be coded are included.

## 4.1 Descriptives ----
coded.peeks <- xdata[xdata$turn.away.coded, ]
sum(xdata$searched == "yes") # Number of peeks
nrow(coded.peeks) # Peeks for which turning away could be coded
sum(xdata$searched == "yes") - nrow(coded.peeks) # Peeks that could not be coded
table(coded.peeks$turned.away) # Turned away: yes / no
mean(coded.peeks$turned.away == "yes") # Proportion of coded peeks followed by turning away

# Peeks after which the chimpanzee did not turn away are treated as
# never turning away (Inf), so they count as "not within x seconds"
turn.away.latency <-
  ifelse(coded.peeks$turned.away == "yes",
    coded.peeks$time.from.last.gaze.to.back.of.head.to.camera, Inf
  )
sum(turn.away.latency <= 2) # Within 2 seconds (preregistered criterion)
mean(turn.away.latency <= 2)
latency.median <- median(turn.away.latency)
latency.p85 <- unname(quantile(turn.away.latency, 0.85, type = 1))
latency.median # Median latency
quantile(turn.away.latency, c(0.25, 0.75), type = 1) # Interquartile range
latency.p85 # 85th percentile
sum(turn.away.latency <= latency.median) # Within the median latency
mean(turn.away.latency <= latency.median)
sum(turn.away.latency <= 5) # Within 5 seconds
mean(turn.away.latency <= 5)
sum(turn.away.latency <= 10) # Within 10 seconds
mean(turn.away.latency <= 10)
sum(turn.away.latency <= latency.p85) # Within the 85th percentile
mean(turn.away.latency <= latency.p85)
max(turn.away.latency[is.finite(turn.away.latency)]) # Longest latency

## 4.2 Histogram (Figure S1) ----
# Shows only the peeks after which the chimpanzee turned away
# Vertical lines: 2 s (preregistered criterion), median, 5 s and 10 s
# (same steps in Figures S1 and S2)
latency.data <-
  data.frame(latency = turn.away.latency[is.finite(turn.away.latency)])

plot_turn_away <-
  ggplot(latency.data, aes(x = latency)) +
  geom_histogram(binwidth = 1, boundary = 0.5, fill = "grey60", color = "white") +
  geom_vline(xintercept = 2, linetype = "solid", linewidth = 0.6) +
  geom_vline(xintercept = latency.median, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = 5, linetype = "dotdash", linewidth = 0.6) +
  geom_vline(xintercept = 10, linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 12, y = Inf, vjust = 1.5, hjust = 0,
           label = paste0("solid line: 2 s, preregistered criterion (",
                          round(100 * mean(turn.away.latency <= 2), 1),
                          "% of peeks)"),
           size = 3.5) +
  annotate("text", x = 12, y = Inf, vjust = 3.3, hjust = 0,
           label = paste0("dashed line: median = ", latency.median, " s (",
                          round(100 * mean(turn.away.latency <= latency.median), 1),
                          "% of peeks)"),
           size = 3.5) +
  annotate("text", x = 12, y = Inf, vjust = 5.1, hjust = 0,
           label = paste0("dot-dashed line: 5 s (",
                          round(100 * mean(turn.away.latency <= 5), 1),
                          "% of peeks)"),
           size = 3.5) +
  annotate("text", x = 12, y = Inf, vjust = 6.9, hjust = 0,
           label = paste0("dotted line: 10 s (",
                          round(100 * mean(turn.away.latency <= 10), 1),
                          "% of peeks)"),
           size = 3.5) +
  scale_x_continuous(breaks = seq(0, 90, by = 10), expand = c(0.01, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Latency from last look at remaining reward to turning away (s)",
       y = "Number of peeks") +
  theme_classic(base_size = 12)
plot_turn_away

ggsave("./figures/Figure_S1_Study_1A_turn_away_latency.png", plot_turn_away,
       width = 7, height = 4, dpi = 300)
ftable(searched ~ name + condition, xdata)

# 5. Inter-rater reliability ----

## 5.1 Percentage agreement ----
interrater <-
  xdata %>%
  filter(!is.na(searched.reli))
# Proportion of trials coded by the second coder
nrow(interrater) / nrow(xdata)
# Number of trials on which the two coders disagreed
sum(interrater$searched.reli !=
  interrater$searched)
# Proportion of trials on which the two coders agreed
(sum(interrater$searched.reli ==
  interrater$searched)) / nrow(interrater)

## 5.2 Cohen's kappa ----
library(irr)
kappa_results <-
  kappa2(cbind(
    as.factor(interrater$searched),
    as.factor(interrater$searched.reli)
  ), "unweighted")
print(kappa_results)

# 6. Participants and descriptives ----

## 6.1 Sample ----
min(xdata$age)
max(xdata$age)
table(xdata$gender) / 54 # 54 trials per chimpanzee

## 6.2 Peeking per condition and reward received (trial level) ----
ftable(searched ~ condition + food.received, xdata)
round(prop.table(ftable(searched ~ condition, xdata), margin = 1) * 100, 2)
round(prop.table(ftable(searched ~ food.received, xdata), margin = 1) * 100, 2)

# Mean and SD of peeking per condition x reward received
round(tapply(
  (as.numeric(xdata$searched) - 1),
  list(xdata$condition, xdata$food.received), mean
), 3)

round(tapply(
  (as.numeric(xdata$searched) - 1),
  list(xdata$condition, xdata$food.received), sd
), 3)

# 7. Model preparation ----

## 7.1 Check which random slopes are identifiable ----
# fe.re.tab() also returns the data with dummy-coded factors
xx.fe.re <- fe.re.tab(
  fe.model = "searched ~
  condition*food.received + trial.per.condition +  gender",
  re = "(1|name)",
  other.vars = c("age", "trial.total"),
  data = xdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

## 7.2 Means and SDs per chimpanzee ----
# Proportion of trials with a peek per chimpanzee, then mean and SD across
# chimpanzees: by condition, by reward received, and by both
t.data$searched_num <- as.numeric(t.data$searched) - 1

per_subject <- t.data %>%
  group_by(name, condition) %>%
  summarise(prop_searched = mean(searched_num, na.rm = TRUE),
            n_trials = n(),
            .groups = "drop")
per_subject

per_subject_summary <- per_subject %>%
  group_by(condition) %>%
  summarise(mean_prop = mean(prop_searched, na.rm = TRUE),
            sd_prop   = sd(prop_searched, na.rm = TRUE),
            n_subjects = n(),
            .groups = "drop")
per_subject_summary

per_subject <- t.data %>%
  group_by(name, food.received) %>%
  summarise(prop_searched = mean(searched_num, na.rm = TRUE),
            n_trials = n(),
            .groups = "drop")
per_subject

per_subject_summary <- per_subject %>%
  group_by(food.received) %>%
  summarise(mean_prop = mean(prop_searched, na.rm = TRUE),
            sd_prop   = sd(prop_searched, na.rm = TRUE),
            n_subjects = n(),
            .groups = "drop")
per_subject_summary

per_subject <- t.data %>%
  group_by(name, condition, food.received) %>%
  summarise(prop_searched = mean(searched_num, na.rm = TRUE),
            n_trials = n(),
            .groups = "drop")
per_subject

per_subject_summary <- per_subject %>%
  group_by(condition, food.received) %>%
  summarise(mean_prop  = mean(prop_searched, na.rm = TRUE),
            sd_prop    = sd(prop_searched, na.rm = TRUE),
            n_subjects = n(),
            .groups = "drop")
per_subject_summary

## 7.3 Center dummy variables and z-transform covariates ----
# Centered dummy variables are needed for the random slopes (and for plotting)
t.data$food.received.low.code <-
  t.data$food.received.low - mean(t.data$food.received.low)
t.data$food.received.none.code <-
  t.data$food.received.none - mean(t.data$food.received.none)
t.data$condition.no.choice.code <-
  t.data$condition.no.choice - mean(t.data$condition.no.choice)
t.data$condition.choice.code <-
  t.data$condition.choice - mean(t.data$condition.choice)
t.data$gender.code <- t.data$gender.male - mean(t.data$gender.male)
t.data$z.age <- scale(as.numeric(t.data$age))
t.data$z.trial <-
  scale(as.numeric(t.data$trial.per.condition))

# 8. Main models (preregistered) ----

## 8.1 Fit full, main-effects, and null model ----
# full: condition x reward received interaction
# main: main effects only (to test condition and reward received)
# null: random effects only
contr <-
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

full <-
  glmer(
    searched ~ (food.received + condition)^2 + z.trial + gender +
      (1 + (condition.no.choice.code + condition.choice.code) +
        (food.received.low.code + food.received.none.code) + z.trial | name),
    data = t.data, control = contr, family = binomial(link = "logit")
  )

main <-
  glmer(
    searched ~ condition + food.received + z.trial + gender +
      (1 + (condition.no.choice.code + condition.choice.code) +
        (food.received.low.code + food.received.none.code) + z.trial | name),
    data = t.data, control = contr, family = binomial(link = "logit")
  )

null <-
  glmer(
    searched ~ 1 +
      (1 + (condition.no.choice.code + condition.choice.code) +
         (food.received.low.code + food.received.none.code) + z.trial | name),
    data = t.data, control = contr, family = binomial(link = "logit")
  )

summary(full)$varcor
round(summary(full)$coefficients, 3)

## 8.2 Model assumptions ----
overdisp.test(full) # Overdispersion
vif(main) # Collinearity
ranef.diagn.plot(full)

## 8.3 Model stability ----
# Refits the model leaving out one chimpanzee at a time
m.stab.b <-
  glmm.model.stab(model.res = full, contr = contr, use = c("name"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))

## 8.4 Full-null model comparison ----
anova(full, null, test = "LRT")

## 8.5 Likelihood-ratio tests of individual effects ----
tests.full <- drop1p(
  model.res = full,
  para = F, data = NULL, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL
)
round(tests.full$drop1.res, 3)
tests.main <- drop1p(
  model.res = main,
  para = F, data = NULL, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL
)
round(tests.main$drop1.res, 3)

## 8.6 Quick visual inspection of effects ----
library("effects")
plot(effect("food.received:condition", full))
plot(effect(c("condition"), main))

## 8.7 Pairwise comparisons ----
emm1 <- emmeans(full, ~ food.received * condition)
summary(emm1, type = "response")
emmeans(full, pairwise ~ food.received | condition, type = "response")

emm2 <- emmeans(main, ~condition)
summary(emm2, type = "response")
emmeans(main, pairwise ~ condition, type = "response")

# 9. Bootstrapped confidence intervals ----

## 9.1 Full model ----
boot.full <-
  boot.glmm.pred(
    model.res = full,
    excl.warnings = T,
    nboots = 1000, para = T,
    level = 0.95,
    use = c("condition", "food.received")
  )

as.data.frame(round(boot.full$ci.estimates, 3))
m.stab.plot(round(boot.full$ci.estimates, 3))
boot.full$ci.predicted

## 9.2 Condition model (for Figure 2a) ----
# Same predictors as the main model, but reward received enters as centered
# dummies, so the fitted values per condition are averaged over reward received
plot.model <-
  glmer(
    searched ~ condition + gender.code + z.trial +
      (food.received.low.code + food.received.none.code) +
      (1 + (condition.no.choice.code + condition.choice.code) +
        (food.received.low.code + food.received.none.code) + z.trial | name),
    data = t.data,
    control = contr,
    family = binomial(link = "logit")
  )

boot.plot <- boot.glmm.pred(
  model.res = plot.model, excl.warnings = T,
  nboots = 1000, para = T, level = 0.95, use = c("condition")
)

as.data.frame(round(boot.plot$ci.estimates, 3))
m.stab.plot(round(boot.plot$ci.estimates, 3))
boot.plot$ci.predicted

## 9.3 Reward model (for Figure 2b) ----
# Same as above, but condition enters as centered dummies, so the fitted values
# per reward received are averaged over condition
model.reward <-
  glmer(
    searched ~
      food.received + gender.code + z.trial +
      (condition.no.choice.code + condition.choice.code) +
      (1 + (condition.no.choice.code + condition.choice.code) +
        (food.received.low.code + food.received.none.code) + z.trial  | name),
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

boot.reward <-
  boot.glmm.pred(
    model.res = model.reward, excl.warnings = T,
    nboots = 1000, para = T, level = 0.95, use = c("food.received")
  )
round(boot.reward$ci.estimates, 3)
as.data.frame(round(boot.reward$ci.estimates, 3))
m.stab.plot(round(boot.reward$ci.estimates, 3))
boot.reward$ci.predicted

# 10. Figure 2 ----
# Small dots and violins: proportion of peeks per chimpanzee
# Large dots and error bars: model estimates with bootstrapped 95% CIs
library(gghalves)
library(ggthemes)
library(cowplot)

which(xdata$searched == "")

## 10.1 Panel b: reward received ----
# Proportion of peeks per chimpanzee and reward received
xdata.agg <- xdata %>%
  mutate(searched.2 = as.numeric(searched) - 1) %>%
  group_by(name, food.received) %>%
  summarise(mean.resp = mean(searched.2, na.rm = T)) %>%
  ungroup()

# Horizontal jitter of the individual data points
xdata.agg$food.received2 <-
  jitter(as.numeric(as.factor(xdata.agg$food.received)), amount = 0.13)
xdata.agg$mean.resp2 <-
  jitter(xdata.agg$mean.resp, amount = 0.00)

exp1_plot_rewards <-
  ggplot(data = xdata.agg, aes(
    x = factor(food.received,
      levels = c("high", "low", "none")
    ),
    y = mean.resp2
  )) +
  # Individual chimpanzees (lines connect the same individual)
  geom_line(aes(x = food.received2, y = mean.resp2, group = name),
    color = "gray", lty = 1, alpha = .5
  ) +
  geom_point(
    data = xdata.agg %>% filter(food.received == "high"),
    aes(x = food.received2), color = "chartreuse4", size = 1.5,
    alpha = .6
  ) +
  geom_point(
    data = xdata.agg %>% filter(food.received == "low"),
    aes(x = food.received2), color = "gold3", size = 1.5,
    alpha = .6
  ) +
  geom_point(
    data = xdata.agg %>% filter(food.received == "none"),
    aes(x = food.received2), color = "red3", size = 1.5,
    alpha = .4
  ) +
  # Distribution of individual proportions
  geom_violin(
    data = xdata.agg %>% filter(food.received == "high"),
    aes(x = food.received, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "chartreuse4", alpha = .2
  ) +
  geom_violin(
    data = xdata.agg %>% filter(food.received == "low"),
    aes(x = food.received, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "gold3", alpha = .2
  ) +
  geom_violin(
    data = xdata.agg %>% filter(food.received == "none"),
    aes(x = food.received, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "red3", alpha = .2
  ) +
  # Bootstrapped 95% CIs
  geom_errorbar(
    data = boot.reward$ci.predicted %>%
      filter(food.received == "high"),
    aes(
      x = as.numeric(food.received) - 0.25,
      y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ),
    color = "chartreuse4", width = 0.1, size = 1
  ) +
  geom_errorbar(
    data = boot.reward$ci.predicted %>%
      filter(food.received == "low"),
    aes(
      x = as.numeric(food.received) + 0.25,
      y = fitted, ymin = lower.cl, ymax = upper.cl
    ),
    color = "gold3", width = 0.1, size = 1
  ) +
  geom_errorbar(
    data = boot.reward$ci.predicted %>%
      filter(food.received == "none"),
    aes(
      x = as.numeric(food.received) + 0.25,
      y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ),
    color = "red3", width = 0.1, size = 1
  ) +
  # Model estimates
  geom_point(
    data = boot.reward$ci.predicted %>%
      filter(food.received == "high"),
    aes(x = as.numeric(food.received) - 0.25, y = fitted),
    color = "darkgreen", size = 3.5
  ) +
  geom_point(
    data = boot.reward$ci.predicted %>%
      filter(food.received == "low"),
    aes(x = as.numeric(food.received) + 0.25, y = fitted),
    color = "gold4", size = 3.5
  ) +
  geom_point(
    data = boot.reward$ci.predicted %>%
      filter(food.received == "none"),
    aes(x = as.numeric(food.received) + 0.25, y = fitted),
    color = "darkred", size = 3.5
  ) +
  # Axes and theme
  scale_x_discrete(
    limits = c("high", "low", "none"),
    name = "Reward received",
    labels = c(
      "High value",
      "Low value",
      "No reward"
    )
  ) +
  scale_y_continuous(
    name = "Post-decisional information search",
    labels = scales::percent
  ) +
  labs(title = "Received reward value") +
  theme_classic() +
  theme(
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    strip.text.x = element_text(size = 13),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left = element_text(vjust = 3),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_text(
      margin =
        margin(t = 10, r = 0, b = 0, l = 0)
    )
  )

exp1_plot_rewards

## 10.2 Panel a: condition ----
ftable(searched ~ name + condition, xdata)

# Proportion of peeks per chimpanzee and condition
xdata.agg <- xdata %>%
  mutate(searched2 = as.numeric(searched) - 1) %>%
  group_by(name, condition) %>%
  summarise(mean.resp = mean(searched2, na.rm = T)) %>%
  ungroup()

# Horizontal jitter of the individual data points
xdata.agg$condition2 <-
  jitter(as.numeric(as.factor(xdata.agg$condition)), amount = 0.07)

exp1_plot_condition <-
  ggplot() +
  # Distribution of individual proportions
  geom_violin(
    data = xdata.agg,
    aes(
      x = factor(condition,
        levels = c("control", "choice", "no-choice")
      ),
      y = mean.resp, fill = condition
    ),
    position = position_nudge(x = 0.00), alpha = .2
  ) +
  scale_fill_manual(values = c("dodgerblue", "darkorange", "darkorange")) +
  # Individual chimpanzees (lines connect the same individual)
  geom_point(
    data = xdata.agg %>% filter(condition == "control"),
    aes(x = condition2, y = mean.resp), color = "dodgerblue", size = 1.5,
    alpha = .4
  ) +
  geom_point(
    data = xdata.agg %>% filter(condition == "choice"),
    aes(x = condition2, y = mean.resp), color = "darkorange", size = 1.5,
    alpha = .4
  ) +
  geom_point(
    data = xdata.agg %>% filter(condition == "no-choice"),
    aes(x = condition2, y = mean.resp), color = "darkorange", size = 1.5,
    alpha = .6
  ) +
  geom_line(
    data = xdata.agg, aes(x = condition2, y = mean.resp, group = name),
    color = "gray", lty = 1, alpha = .7
  ) +
  # Bootstrapped 95% CIs
  geom_errorbar(
    data = boot.plot$ci.predicted %>%
      filter(condition == "control"),
    aes(
      x = as.numeric(condition) - 0.25, y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ), color = "dodgerblue",
    width = 0.1, size = 1
  ) +
  geom_errorbar(
    data = boot.plot$ci.predicted %>%
      filter(condition == "choice"),
    aes(
      x = as.numeric(condition) + 0.25, y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ), color = "darkorange",
    width = 0.1, size = 1
  ) +
  geom_errorbar(
    data = boot.plot$ci.predicted %>%
      filter(condition == "no-choice"),
    aes(
      x = as.numeric(condition) + 0.25, y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ), color = "darkorange",
    width = 0.1, size = 1
  ) +
  # Model estimates
  geom_point(
    data = boot.plot$ci.predicted %>% filter(condition == "control"),
    aes(x = as.numeric(condition) - 0.25, y = fitted),
    color = "dodgerblue4", size = 3.5
  ) +
  geom_point(
    data = boot.plot$ci.predicted %>% filter(condition == "choice"),
    aes(x = as.numeric(condition) + 0.25, y = fitted),
    color = "darkorange4", size = 3.5
  ) +
  geom_point(
    data = boot.plot$ci.predicted %>% filter(condition == "no-choice"),
    aes(x = as.numeric(condition) + 0.25, y = fitted),
    color = "darkorange4", size = 3.5
  ) +
  # Axes and theme
  scale_x_discrete(
    limits = c("control", "choice", "no-choice"),
    name = "Condition",
    labels = c("Control", "Choice", "No choice")
  ) +
  scale_y_continuous(
    name = "Post-decisional information search",
    labels = scales::percent
  ) +
  labs(title = "Condition") +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    strip.text.x = element_text(size = 13),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left = element_text(vjust = 3),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_text(
      margin =
        margin(t = 10, r = 0, b = 0, l = 0)
    )
  )
exp1_plot_condition

## 10.3 Combine panels ----
library(ggpubr)
# theme_set(theme_pubr())
figure <- ggarrange(exp1_plot_condition, exp1_plot_rewards,
  labels = c("(a)", "(b)"),
  hjust = -1, # Horizontal position of the panel labels
  vjust = 2.5, # Vertical position of the panel labels
  ncol = 2, nrow = 1
)
figure

## 10.4 Save Figure 2 ----
ggsave("./figures/Figure_2_Study_1A.pdf", figure,
       width = 10.625, height = 5.333)

# 11. Robustness analysis: test conditions only (without control) ----
# Same models as in Section 8, fitted to the choice and no-choice conditions
# only, to check whether the effect of reward received holds without the
# control condition

## 11.1 Subset data and fit models ----
without_contr <- subset(t.data, t.data$condition == "choice" | t.data$condition == "no-choice")

full_without_contr <-
  glmer(
    searched ~ (food.received + condition)^2 + z.trial + gender +
      (1 + (condition.no.choice.code + condition.choice.code) +
         (food.received.low.code + food.received.none.code) + z.trial | name),
    data = without_contr, control = contr, family = binomial(link = "logit")
  )

main_without_contr <-
  glmer(
    searched ~ condition + food.received + z.trial + gender +
      (1 + (condition.no.choice.code + condition.choice.code) +
         (food.received.low.code + food.received.none.code) + z.trial | name),
    data = without_contr, control = contr, family = binomial(link = "logit")
  )

null_without_contr <-
  glmer(
    searched ~ 1 + gender +
      (1 + (condition.no.choice.code + condition.choice.code) +
         (food.received.low.code + food.received.none.code) + z.trial | name),
    data = without_contr, control = contr, family = binomial(link = "logit")
  )

summary(full_without_contr)$varcor
round(summary(full_without_contr)$coefficients, 3)

## 11.2 Model assumptions ----
overdisp.test(full_without_contr) # Overdispersion
vif(main_without_contr) # Collinearity
ranef.diagn.plot(full_without_contr)

## 11.3 Model stability ----
m.stab.b <-
  glmm.model.stab(model.res = full_without_contr, contr = contr, use = c("name"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))

## 11.4 Model comparisons and tests ----

# Full-null model comparison
anova(full_without_contr, null_without_contr, test = "LRT")

# Likelihood-ratio tests of individual effects
tests.full_without_contr <- drop1p(
  model.res = full_without_contr,
  para = F, data = NULL, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL
)
round(tests.full_without_contr$drop1.res, 3)

tests.main_without_contr <- drop1p(
  model.res = main_without_contr,
  para = F, data = NULL, contr = contr,
  n.cores = c("all-1", "all"), to.del = NULL
)
round(tests.main_without_contr$drop1.res, 3)

# Quick visual inspection of effects
library("effects")
plot(effect("food.received:condition", full_without_contr))
plot(effect(c("condition"), main_without_contr))

# Pairwise comparisons
emm1 <- emmeans(full_without_contr, ~ food.received * condition)
summary(emm1, type = "response")
emmeans(full_without_contr, pairwise ~ food.received | condition, type = "response")

emm2 <- emmeans(main_without_contr, ~condition)
summary(emm2, type = "response")
emmeans(main_without_contr, pairwise ~ condition, type = "response")

## 11.5 Bootstrapped confidence intervals ----
# Saved separately from boot.full (Section 9.1, Table S2); this one is Table S3
boot.full_without_contr <-
  boot.glmm.pred(
    model.res = full_without_contr,
    excl.warnings = T,
    nboots = 1000, para = T,
    level = 0.95,
    use = c("condition", "food.received")
  )

as.data.frame(round(boot.full_without_contr$ci.estimates, 3))
m.stab.plot(round(boot.full_without_contr$ci.estimates, 3))
boot.full_without_contr$ci.predicted

# 12. Additional analyses in response to the reviewers ----
# Exploratory analyses (not preregistered), reported in the SI

## 12.1 Peeking across sessions ----
# Session within condition (1-3), taken from condition_day (e.g., "choice_2")
xdata$session.in.condition <- as.numeric(sub(".*_", "", xdata$condition_day))
# Percentage of trials with a peek per condition and session
round(100 * tapply(xdata$searched == "yes",
                   list(xdata$condition, xdata$session.in.condition), mean), 1)

## 12.2 Peeking on the first trial of each session ----
# Did chimpanzees peek more at the start of a session, when sampling for
# future sessions would be most useful?
xdata$first.trial <- ifelse(xdata$trial == 1, "first", "later")
round(100 * tapply(xdata$searched == "yes",
                   list(xdata$condition, xdata$first.trial), mean), 1)

# t.data has the same rows in the same order as xdata
stopifnot(identical(as.character(t.data$name), as.character(xdata$name)),
          all(t.data$trial.per.condition == xdata$trial.per.condition))
t.data$first.trial.code <- (xdata$trial == 1) - mean(xdata$trial == 1)

# Main-effects model of the test conditions (as in Section 11) with first
# trial of a session (dummy-coded and centered) as an additional predictor
test.data <- subset(t.data, t.data$condition == "choice" |
                      t.data$condition == "no-choice")
first.trial.model <-
  glmer(
    searched ~ condition + food.received + z.trial + gender + first.trial.code +
      (1 + (condition.no.choice.code + condition.choice.code) +
         (food.received.low.code + food.received.none.code) + z.trial | name),
    data = test.data, control = contr, family = binomial(link = "logit")
  )
round(summary(first.trial.model)$coefficients, 3)
drop1(first.trial.model, test = "Chisq") # Likelihood-ratio test of first trial

## 12.3 Latency to peek ----
# Latency (s) from the opening of the top curtains to the peek
peek.latency <- xdata$seconds.until.searched[xdata$searched == "yes"]
sum(xdata$searched == "yes") # Number of peeks
sum(is.na(peek.latency)) # Peeks without a coded latency
peek.latency <- peek.latency[!is.na(peek.latency)]
round(100 * mean(peek.latency <= 5), 1) # Within 5 s
round(100 * mean(peek.latency <= 10), 1) # Within 10 s
round(100 * mean(peek.latency <= 20), 1) # Within 20 s

## 12.4 Robustness to shorter peeking windows ----
# Peeking counted only if it occurred within the first 10, 15, or 20 s of the
# window; peeks without a coded latency are excluded. Same model structure as
# the main-effects model in Section 8; likelihood-ratio tests via drop1.
window.results <- list()
for (window in c(10, 15, 20)) {
  t.data$searched.window <- factor(ifelse(
    xdata$searched == "yes" & is.na(xdata$seconds.until.searched), NA,
    ifelse(xdata$searched == "yes" & xdata$seconds.until.searched <= window,
           "yes", "no")
  ))
  window.model <-
    glmer(
      searched.window ~ condition + food.received + z.trial + gender +
        (1 + (condition.no.choice.code + condition.choice.code) +
           (food.received.low.code + food.received.none.code) + z.trial | name),
      data = t.data, control = contr, family = binomial(link = "logit")
    )
  window.results[[paste0(window, "s")]] <- list(
    percent = round(100 * tapply(t.data$searched.window == "yes",
                                 t.data$condition, mean, na.rm = TRUE), 1),
    lrt = drop1(window.model, test = "Chisq")
  )
}
window.results

# 13. Save workspace ----
save.image("./chimpanzees/study1/R_objects/analysis_1.RData")
