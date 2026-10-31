# ==============================================================================
# Study 1B (children) - Main analysis
# ==============================================================================
#
# Purpose:  Tests whether children peek into the unchosen box after a
#           decision, and whether peeking depends on condition (choice,
#           no-choice, control), on the value of the reward received, and
#           on age.
# Input:    children/study1/data/data_study1B_children.csv
# Output:   Results of Study 1B, Figure 3 (saved to figures/),
#           additional analyses for the reviewers (Section 12, reported in the
#           manuscript and SI),
#           children/study1/R_objects/analysis_1B.RData
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
  "lme4","readxl","tidyverse","tidyselect","parallel","optimx","emmeans","car",
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
source("./children/study1/functions/diagnostic_fcns.r")
source("./children/study1/functions/glmm_stability.r")
source("./children/study1/functions/drop1_para.r")
source("./children/study1/functions/boot_glmm.r")
options(scipen = 9999) # Avoid scientific notation in the output

# 2. Load data ----

# Optional: load all saved objects (models, bootstraps, plots) instead of
# re-running the long computations
# load("./children/study1/R_objects/analysis_1B.RData")

xdata <-
  read.csv2("./children/study1/data/data_study1B_children.csv",
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

# Age (in years) uses a decimal point, which read.csv2() reads as text;
# convert it to numeric so that age enters the models as a covariate
xdata$age <- as.numeric(xdata$age)

# 3. Data preparation ----

## 3.1 Exclude familiarisation trials ----
xdata <-
  subset(xdata, xdata$condition.trial == "choice" |
           xdata$condition.trial == "no.choice" |
           xdata$condition.trial == "control")
xdata$condition <-
  droplevels(as.factor(xdata$condition))

## 3.2 Factors and reference levels ----
xdata$searched <-
  as.factor(xdata$searched)
ftable(searched ~
         condition + reward.received, xdata)
xdata$condition <-
  factor(xdata$condition,
         levels = c("control", "choice", "no.choice"),
         labels = c("control", "choice", "no.choice")
  )
xdata$value.remaining.seen <-
  as.factor(xdata$value.remaining.seen)
xdata$value.remaining.seen <-
  relevel(xdata$value.remaining.seen,
          ref = "worse"
  )

## 3.3 Trial numbers ----
# Running test-trial number per child (1-4)
xdata$trial.per.child <-
  as.numeric(ave(xdata$id,
                 list(xdata$id, xdata$id),
                 FUN = seq_along
  ))

# Did children try to get the remaining reward?
xdata$trying.to.get.remaining
ftable(searched ~ trying.to.get.remaining, xdata)

# 4. Participants ----
# Number of trials per age group, and mean, minimum and maximum age per group
table(xdata$age.group)
tapply(xdata$age, xdata$age.group, mean)
tapply(xdata$age, xdata$age.group, min)
tapply(xdata$age, xdata$age.group, max)
mean(xdata$age)
sd(xdata$age)

# 5. Inter-rater reliability ----

## 5.1 Percentage agreement ----
interrater <-
  xdata %>%
  filter(!is.na(searched.reli))
# Proportion of trials coded by the second coder (25%)
nrow(interrater) / nrow(xdata)

# Number of trials on which the two coders disagreed
sum(interrater$searched.reli !=
      interrater$searched)
# Proportion of trials on which the two coders agreed
(sum(interrater$searched.reli ==
       interrater$searched)) / nrow(interrater)

## 5.2 Cohen's kappa ----
kappa_results <-
  kappa2(cbind(
    as.factor(interrater$searched.reli),
    interrater$searched
  ), "unweighted")
print(kappa_results)

# 6. Descriptives ----
# Peeking per condition and reward received (trial level)
ftable(searched ~ condition + reward.received, xdata)
round(prop.table(ftable(searched ~ condition, xdata), margin = 1) * 100, 2)
round(prop.table(ftable(searched ~ reward.received, xdata), margin = 1) * 100, 2)

# 7. Model preparation ----

## 7.1 Check which random slopes are identifiable ----
# fe.re.tab() also returns the data with dummy-coded factors
xx.fe.re <- fe.re.tab(
  fe.model = "searched ~ age*condition*reward.received +
                   gender + trial.per.child",
  re = "(1|id)", other.vars = c("age.group"),
  data = xdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)
# Too few children have more than two observations per level of reward
# received, so the random slope of reward received cannot be included.

## 7.2 Means and SDs per child ----
# Proportion of trials with a peek per child, then mean and SD across
# children: by condition, by reward received, and by both
t.data$searched_num <- as.numeric(t.data$searched) - 1

per_subject <- t.data %>%
  group_by(id, condition) %>%
  summarise(prop_searched = mean(searched_num, na.rm = TRUE),
            n_trials = n(),
            .groups = "drop")
per_subject_summary <- per_subject %>%
  group_by(condition) %>%
  summarise(mean_prop = mean(prop_searched, na.rm = TRUE),
            sd_prop   = sd(prop_searched, na.rm = TRUE),
            n_subjects = n(),
            .groups = "drop")

per_subject <- t.data %>%
  group_by(id, reward.received) %>%
  summarise(prop_searched = mean(searched_num, na.rm = TRUE),
            n_trials = n(),
            .groups = "drop")
per_subject_summary <- per_subject %>%
  group_by(reward.received) %>%
  summarise(mean_prop = mean(prop_searched, na.rm = TRUE),
            sd_prop   = sd(prop_searched, na.rm = TRUE),
            n_subjects = n(),
            .groups = "drop")

per_subject <- t.data %>%
  group_by(id, condition, reward.received) %>%
  summarise(prop_searched = mean(searched_num, na.rm = TRUE),
            n_trials = n(),
            .groups = "drop")

per_subject_summary <- per_subject %>%
  group_by(condition, reward.received) %>%
  summarise(mean_prop  = mean(prop_searched, na.rm = TRUE),
            sd_prop    = sd(prop_searched, na.rm = TRUE),
            n_subjects = n(),
            .groups = "drop")

## 7.3 Center dummy variables and z-transform covariates ----
# Centered dummy variables are needed for the random slopes (and for plotting)
t.data$reward.received.low.code <-
  t.data$reward.received.low - mean(t.data$reward.received.low)
t.data$reward.received.none.code <-
  t.data$reward.received.none - mean(t.data$reward.received.none)
t.data$condition.no.choice.code <-
  t.data$condition.no.choice - mean(t.data$condition.no.choice)
t.data$condition.choice.code <-
  t.data$condition.choice - mean(t.data$condition.choice)
t.data$gender.code <-
  t.data$gender.male - mean(t.data$gender.male)
t.data$z.age <- scale(as.numeric(t.data$age))
t.data$z.trial <- scale(as.numeric(t.data$trial.per.child))

# 8. Main models (preregistered) ----

## 8.1 Fit full, reduced, main-effects, and null model ----
# full:  three-way interaction reward received x condition x age
# red.1: all two-way interactions (to test the three-way interaction)
# red.2: condition x age interaction only (to test the other two-way interactions)
# main:  main effects only (to test the main effects)
# null:  random effects only
# The random slopes of reward received are not identifiable (see Section 7.1)
# and are therefore left out (commented out below).
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

full <-
  glmer(
    searched ~
      (reward.received * condition * z.age)  + gender + z.trial +
      (1 + z.trial | id), #(reward.received.low.code + reward.received.none.code)
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

red.1 <-
  glmer(
    searched ~
      (reward.received + condition + z.age)^2  + gender + z.trial +
      (1 + z.trial | id), #(reward.received.low.code + reward.received.none.code)
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

red.2 <-
  glmer(
    searched ~
      reward.received + (condition + z.age)^2 + gender + z.trial +
      (1 + z.trial | id), #(reward.received.low.code + reward.received.none.code)
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

main <-
  glmer(
    searched ~
      reward.received + condition + z.age + gender + z.trial +
      (1 + z.trial | id), #(reward.received.low.code + reward.received.none.code)
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

null <-
  glmer(
    searched ~
      1 +
      (1 + z.trial | id), #(reward.received.low.code + reward.received.none.code)
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

summary(full)$varcor
round(summary(full)$coefficients, 3)

# Observed proportion of peeks per condition x reward received
tapply(
  (as.numeric(t.data$searched) - 1),
  list(t.data$condition, t.data$reward.received), mean
)

round(summary(full)$coefficients, 3)

## 8.2 Model assumptions ----
overdisp.test(full) # Overdispersion
vif(main) # Collinearity
ranef.diagn.plot(full)

## 8.3 Model stability ----
# Refits the model leaving out one child at a time
m.stab.b <-
  glmm.model.stab(model.res = full, contr = contr, use = c("id"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))
# Range of estimates, based only on refits without warnings
m.stab.b$detailed <- m.stab.b$detailed %>%
  filter(warnings == "none")
cbind(
  apply(m.stab.b$detailed, 2, min),
  apply(m.stab.b$detailed, 2, max)
)

## 8.4 Full-null model comparison ----
anova(full, null, test = "LRT")

## 8.5 Likelihood-ratio tests of individual effects ----
tests.full <- drop1p(
  model.res = full,
  contr = contr
)
round(tests.full$drop1.res, 3)

tests.red.1 <- drop1p(
  model.res = red.1,
  contr = contr
)
round(tests.red.1$drop1.res, 3)

tests.red.2 <- drop1p(
  model.res = red.2,
  contr = contr
)
round(tests.red.2$drop1.res, 3)

tests.main <- drop1p(
  model.res = main,
  contr = contr
)
round(tests.main$drop1.res, 3)

## 8.6 Quick visual inspection of effects ----
library("effects")
plot(effect("condition:z.age", red.1))
plot(effect("z.trial", main))
plot(effect("gender", main))
plot(effect(c("reward.received"), main))

## 8.7 Pairwise comparisons ----
emm1 <- emmeans(main, ~condition)
summary(emm1, type = "response")
emmeans(main, pairwise ~ condition)
summary(pairs(emm1), type = "response")

emm2 <- emmeans(main, ~reward.received)
summary(emm2, type = "response")
summary(pairs(emm2), type = "response")
emmeans(main, pairwise ~reward.received)

# 9. Bootstrapped confidence intervals ----

## 9.1 Full model ----
boot.full <- boot.glmm.pred(
  model.res = full, excl.warnings = T,
  nboots = 1000, para = T, level = 0.95,
  use = c("condition", "reward.received", "z.age")
)
round(boot.full$ci.estimates, 3)
as.data.frame(round(boot.full$ci.estimates, 3))
m.stab.plot(round(boot.full$ci.estimates, 3))
boot.full$ci.predicted

## 9.2 Condition x age model (for Figure 3a) ----
# Reward received enters as centered dummies, so the fitted values are
# averaged over reward received
model.condition.age <-
  glmer(
    searched ~ condition * z.age +
      (reward.received.low.code + reward.received.none.code) + gender.male + z.trial +
      (1 + z.trial | id), #(reward.received.low.code + reward.received.none.code)
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

boot.condition.age <- boot.glmm.pred(
  model.res = model.condition.age,
  excl.warnings = T,
  nboots = 1000, para = T, level = 0.95,
  use = c("condition", "z.age")
)
round(boot.condition.age$ci.estimates, 3)
as.data.frame(round(boot.condition.age$ci.estimates, 3))
m.stab.plot(round(boot.condition.age$ci.estimates, 3))
head(boot.condition.age$ci.predicted)

## 9.3 Reward model (for Figure 3b) ----
# Condition enters as centered dummies, so the fitted values are averaged
# over condition
model.reward <-
  glmer(
    searched ~ reward.received +
      z.age * (condition.no.choice.code + condition.choice.code) + gender.male + z.trial +
      (1 + z.trial | id),
    data = t.data, control = contr,
    family = binomial(link = "logit")
  )

summary(model.reward)$coefficients

boot.reward <-
  boot.glmm.pred(
    model.res = model.reward, excl.warnings = T,
    nboots = 1000, para = T,
    level = 0.95, use = c("reward.received")
  )
round(boot.reward$ci.estimates, 3)
as.data.frame(round(boot.reward$ci.estimates, 3))
m.stab.plot(round(boot.reward$ci.estimates, 3))
head(boot.reward$ci.predicted)

# 10. Figure 3 ----
# Small dots and violins: proportion of peeks per child
# Large dots, lines, error bars and ribbons: model estimates with
# bootstrapped 95% CIs
library(gghalves)
library(ggthemes)
library(cowplot)

## 10.1 Panel b: reward received ----
# Proportion of peeks per child and reward received
xdata.agg <- xdata %>%
  mutate(searched.numeric = as.numeric(searched) - 1) %>%
  group_by(id, reward.received) %>%
  summarise(mean.resp = mean(searched.numeric, na.rm = T)) %>%
  ungroup()

# Horizontal jitter of the individual data points
xdata.agg$reward.received2 <-
  jitter(as.numeric(as.factor(xdata.agg$reward.received)), amount = 0.13)
xdata.agg$mean.resp2 <-
  jitter(xdata.agg$mean.resp, amount = 0.00)

exp1_plot_reward <-
  ggplot(data = xdata.agg, aes(x = factor(reward.received,
                                          levels = c("high", "low", "none")
  ), y = mean.resp2)) +

  # geom_line(aes(x = reward.received2, y = mean.resp2, group = id),
  #           color = "gray", lty = 1, alpha = .3) +

  # Individual children
  geom_point(
    data = xdata.agg %>% filter(reward.received == "high"),
    aes(x = reward.received2), color = "chartreuse4", size = 1.5,
    alpha = .6
  ) +
  geom_point(
    data = xdata.agg %>% filter(reward.received == "low"),
    aes(x = reward.received2), color = "gold3", size = 1.5,
    alpha = .6
  ) +
  geom_point(
    data = xdata.agg %>% filter(reward.received == "none"),
    aes(x = reward.received2), color = "red3", size = 1.5,
    alpha = .4
  ) +
  # Distribution of individual proportions
  geom_violin(
    data = xdata.agg %>% filter(reward.received == "high"),
    aes(x = reward.received, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "chartreuse4", alpha = .2
  ) +
  geom_violin(
    data = xdata.agg %>% filter(reward.received == "low"),
    aes(x = reward.received, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "gold3", alpha = .2
  ) +
  geom_violin(
    data = xdata.agg %>% filter(reward.received == "none"),
    aes(x = reward.received, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "red3", alpha = .2
  ) +
  # Bootstrapped 95% CIs
  geom_errorbar(
    data = boot.reward$ci.predicted %>%
      filter(reward.received == "high"),
    aes(
      x = as.numeric(reward.received) + 0.25, y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ), color = "chartreuse4",
    width = 0.1, size = 1
  ) +
  geom_errorbar(
    data = boot.reward$ci.predicted %>%
      filter(reward.received == "low"),
    aes(
      x = as.numeric(reward.received) + 0.25, y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ), color = "gold3",
    width = 0.1, size = 1
  ) +
  geom_errorbar(
    data = boot.reward$ci.predicted %>%
      filter(reward.received == "none"),
    aes(
      x = as.numeric(reward.received) + 0.25, y = fitted,
      ymin = lower.cl, ymax = upper.cl
    ), color = "red3",
    width = 0.1, size = 1
  ) +
  # Model estimates
  geom_point(
    data = boot.reward$ci.predicted %>%
      filter(reward.received == "high"),
    aes(x = as.numeric(reward.received) + 0.25, y = fitted),
    color = "darkgreen", size = 2.5
  ) +
  geom_point(
    data = boot.reward$ci.predicted %>%
      filter(reward.received == "low"),
    aes(x = as.numeric(reward.received) + 0.25, y = fitted),
    color = "gold4", size = 2.5
  ) +
  geom_point(
    data = boot.reward$ci.predicted %>%
      filter(reward.received == "none"),
    aes(x = as.numeric(reward.received) + 0.25, y = fitted),
    color = "darkred", size = 2.5
  ) +
  # Axes and theme
  scale_x_discrete(
    limits =
      c(
        "high",
        "low",
        "none"
      ),
    name =
      "Reward received",
    labels =
      c(
        "High value",
        "Low value",
        "No reward"
      )
  ) +
  scale_y_continuous(
    name =
      "Post-decisional information search",
    labels = scales::percent
  ) +
  labs(
    title =
      "Received reward value"
  ) +
  theme_classic() +
  theme(
    axis.ticks.x =
      element_blank(),
    axis.title =
      element_text(size = 15),
    axis.text =
      element_text(size = 13),
    strip.text.x =
      element_text(size = 13),
    plot.margin =
      unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left =
      element_text(vjust = 3),
    plot.title =
      element_text(color = "black", size = 15, face = "bold"),
    axis.title.x =
      element_text(
        margin =
          margin(t = 10, r = 0, b = 0, l = 0)
      )
  )
exp1_plot_reward

## 10.2 Panel a: condition x age ----
xdata$z.age <- scale(as.numeric(xdata$age))

# Proportion of peeks per child (one data point per child)
xdata.agg.condition <- xdata %>%
  mutate(searched.numeric = as.numeric(searched) - 1) %>%
  group_by(id, z.age, condition) %>%
  summarise(mean.resp = mean(searched.numeric, na.rm = T)) %>%
  ungroup()

xdata.agg.condition$mean.resp2 <-
  jitter(xdata.agg.condition$mean.resp, amount = 0.00)

# Proportion of peeks per age group and condition, placed at the middle of each
# age year (only used by the commented-out layer in the plot below)
xdata.agg.age.group.condition <- xdata %>%
  mutate(searched.numeric = as.numeric(searched) - 1) %>%
  group_by(age.group, condition) %>%
  summarise(mean.resp = mean(searched.numeric, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    # Middle of the age year (e.g., 2 -> 2.5)
    age.group.mid = as.numeric(age.group) + 0.5,
    # Same z-scale as z.age
    z.age.group   = (age.group.mid - mean(as.numeric(xdata$age))) / sd(xdata$age)
  )
xdata.agg.age.group.condition$age.group <- as.numeric(xdata.agg.age.group.condition$age.group) + 0.5

exp1_plot_condition <-
  ggplot() +
  # Individual children
  geom_point(
    data = xdata.agg.condition, shape = 21,
    aes(x = z.age, y = mean.resp2, fill = condition, color = condition),
    size = 1.7, alpha = .4
  ) +
  # geom_point(
  #   data = xdata.agg.age.group.condition, shape = 23,
  #   aes(x = z.age.group, y = mean.resp, fill = condition, color = condition),
  #   size = 3, alpha = 1
  # ) +
  # Bootstrapped 95% CIs
  geom_ribbon(
    data = boot.condition.age$ci.predicted,
    aes(x = z.age, ymin = lower.cl, ymax = upper.cl,
        group = condition, fill = condition, color = condition),
    alpha = 0.3) +
  scale_fill_manual(values = c("dodgerblue", "darkorange", "darkorange")) +
  # Model estimates
  geom_line(
    data = boot.condition.age$ci.predicted,
    aes(
      x = z.age,
      y = fitted,
      color = condition), size = 0.8) +
  scale_color_manual(values = c("dodgerblue4", "darkorange4", "darkorange4")) +
  # One panel per condition
  #facet_wrap(~condition, strip.position = "bottom") +
  facet_wrap(
    ~condition, strip.position = "bottom",
    labeller = as_labeller(c(
      control = "Control",
      choice = "Choice",
      no.choice = "No choice"
    ))
  ) +
  # Axes and theme; x-axis ticks at whole years of age (on the z-scale)
  scale_x_continuous(
    name = "Condition * Age",
    breaks = c(
      (2 - mean(xdata$age)) / sd(xdata$age),
      (3 - mean(xdata$age)) / sd(xdata$age),
      (4 - mean(xdata$age)) / sd(xdata$age),
      (5 - mean(xdata$age)) / sd(xdata$age),
      (6 - mean(xdata$age)) / sd(xdata$age)
    ),
    labels = c(2, 3, 4, 5, 6)
  ) +
  scale_y_continuous(
    name = "Post-decisional information search",
    labels = scales::percent
  ) +
  labs(title = "Condition*age interaction") +
  theme_classic() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_text(
      size = 13,
      margin = margin(t = 0, r = 0, b = 8, l = 0)
    ),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left = element_text(vjust = 3),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))
  )
exp1_plot_condition

## 10.3 Combine panels ----
library(ggpubr)
# theme_set(theme_pubr())
figure <- ggarrange(exp1_plot_condition, exp1_plot_reward,
                    labels = c("(a)", "(b)"),
                    hjust = -1, # Horizontal position of the panel labels
                    vjust = 2.5, # Vertical position of the panel labels
                    ncol = 2, nrow = 1
)
figure

## 10.4 Save Figure 3 ----
ggsave("./figures/Figure_3_Study_1B.pdf", figure,
       width = 9.236, height = 4.556)

# 11. Robustness analysis: test conditions only (without control) ----
# Same models as in Section 8, fitted to the choice and no-choice conditions
# only, to check whether the results hold without the control condition

## 11.1 Subset data and fit models ----
without_contr <- subset(t.data, t.data$condition == "choice" |
                          t.data$condition == "no.choice")

full.no.cont <-
  glmer(
    searched ~
      reward.received * condition * z.age + gender + z.trial +
      (1 + z.trial | id),
    data = without_contr, control = contr,
    family = binomial(link = "logit")
  )

red.no.cont.1 <-
  glmer(
    searched ~
      (reward.received + condition + z.age)^2 + gender + z.trial +
      (1 + z.trial | id),
    data = without_contr, control = contr,
    family = binomial(link = "logit")
  )

main.no.cont <-
  glmer(
    searched ~
      reward.received + condition + z.age + gender + z.trial +
      (1 + z.trial | id),
    data = without_contr, control = contr,
    family = binomial(link = "logit")
  )

null.no.cont <-
  glmer(
    searched ~
      1 +
      (1 + z.trial | id),
    data = without_contr, control = contr,
    family = binomial(link = "logit")
  )

round(summary(full.no.cont)$coefficients, 3)

## 11.2 Model assumptions ----
overdisp.test(full.no.cont) # Overdispersion
vif(main.no.cont) # Collinearity
ranef.diagn.plot(full.no.cont)

## 11.3 Model stability ----
m.stab.b <-
  glmm.model.stab(model.res = full.no.cont, contr = contr, use = c("id"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))
# Range of estimates, based only on refits without warnings
m.stab.b$detailed <- m.stab.b$detailed %>%
  filter(warnings == "none")
cbind(
  apply(m.stab.b$detailed, 2, min),
  apply(m.stab.b$detailed, 2, max))

## 11.4 Model comparisons and tests ----

# Full-null model comparison
round(anova(full.no.cont, null.no.cont, test = "Chisq"), 3)

# Likelihood-ratio tests of individual effects
tests.full.no.cont <- drop1p(
  model.res = full.no.cont,
  contr = contr
)
round(tests.full.no.cont$drop1.res, 3)

tests.red.1.no.cont <- drop1p(
  model.res = red.no.cont.1,
  contr = contr
)
round(tests.red.1.no.cont$drop1.res, 3)

tests.main.no.cont <- drop1p(
  model.res = main.no.cont,
  contr = contr
)
round(tests.main.no.cont$drop1.res, 3)

# Quick visual inspection of effects
library("effects")
plot(effect("condition:z.age", red.no.cont.1))
plot(effect(c("reward.received"), main.no.cont))
plot(effect(c("z.trial"), main.no.cont))

## 11.5 Bootstrapped confidence intervals ----
boot.full.no.cont <- boot.glmm.pred(
  model.res = full.no.cont, excl.warnings = T,
  nboots = 1000, para = T, level = 0.95,
  use = c("condition", "reward.received", "z.age")
)
round(boot.full.no.cont$ci.estimates, 3)
as.data.frame(round(boot.full.no.cont$ci.estimates, 3))
m.stab.plot(round(boot.full.no.cont$ci.estimates, 3))
boot.full.no.cont$ci.predicted

# 12. Additional analyses in response to the reviewers ----
# Exploratory analyses (not preregistered), reported in the manuscript and SI

## 12.1 Attempts to obtain the remaining reward after peeking ----
# Did children try to get the remaining reward after peeking (e.g., by reaching
# for the unchosen box or asking for it)?
peeks <- xdata[xdata$searched == "yes", ]
nrow(peeks) # Number of peeks
table(peeks$trying.to.get.remaining, useNA = "always")
sum(peeks$trying.to.get.remaining == "no", na.rm = TRUE) # No attempt
round(100 * mean(peeks$trying.to.get.remaining == "no", na.rm = TRUE), 1)

# Attempts by the reward children received: if disappointment drove the
# search, attempts should be frequent after receiving nothing
attempts <- table(peeks$reward.received, peeks$trying.to.get.remaining)
attempts
round(100 * prop.table(attempts, 1), 1)
fisher.test(attempts)

## 12.2 Latency to peek ----
# Latency (s) from the opening of the top curtains to the peek
xdata$seconds.until.searched <- suppressWarnings(as.numeric(xdata$seconds.until.searched))
peek.latency <- xdata$seconds.until.searched[xdata$searched == "yes"]
sum(is.na(peek.latency)) # Peeks without a coded latency
peek.latency <- peek.latency[!is.na(peek.latency)]
round(100 * mean(peek.latency <= 5), 1) # Within 5 s
round(100 * mean(peek.latency <= 10), 1) # Within 10 s
round(100 * mean(peek.latency <= 20), 1) # Within 20 s

## 12.3 Robustness to shorter peeking windows ----
# Peeking counted only if it occurred within the first 10, 15, or 20 s of the
# window; peeks without a coded latency are excluded. Same model structure as
# red.2 in Section 8 (condition x age, reward received, gender, trial).
# t.data has the same rows in the same order as xdata
stopifnot(all(t.data$id == xdata$id),
          all(t.data$trial.per.child == xdata$trial.per.child))
window.results <- list()
for (window in c(10, 15, 20)) {
  t.data$searched.window <- factor(ifelse(
    xdata$searched == "yes" & is.na(xdata$seconds.until.searched), NA,
    ifelse(xdata$searched == "yes" & xdata$seconds.until.searched <= window,
           "yes", "no")
  ))
  window.model <-
    glmer(
      searched.window ~
        reward.received + (condition + z.age)^2 + gender + z.trial +
        (1 + z.trial | id),
      data = t.data, control = contr,
      family = binomial(link = "logit")
    )
  window.results[[paste0(window, "s")]] <- list(
    percent = round(100 * tapply(t.data$searched.window == "yes",
                                 t.data$condition, mean, na.rm = TRUE), 1),
    lrt = drop1(window.model, test = "Chisq")
  )
}
window.results

# 13. Save workspace ----
save.image("./children/study1/R_objects/analysis_1B.RData")
