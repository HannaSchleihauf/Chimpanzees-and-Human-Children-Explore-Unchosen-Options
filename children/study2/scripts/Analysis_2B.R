# ==============================================================================
# Study 2B (children) - Main analysis
# ==============================================================================
#
# Purpose:  Tests whether children, after choosing the middle box, peek
#           into the outer box that had been available to choose more often
#           than expected by chance (50%), rather than into the outer box
#           that never was.
# Input:    children/study2/data/data_study2B_children.csv
# Output:   Results of Study 2B, Figure 4c (plot_study2_children),
#           additional analysis for the reviewers (Section 9, reported in the
#           manuscript),
#           children/study2/R_objects/Analysis_2.RData
# Run from: Project root (open Post_Decisional_Curiosity.Rproj first)
# Runtime:  The bootstrap (Section 7.3) takes a while. To skip it, load the
#           saved workspace (see Section 2).
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
# GLMM utility functions by Roger Mundry (see README); the copy in the Study 1B
# folder is used
source("./children/study1/functions/diagnostic_fcns.r")
source("./children/study1/functions/glmm_stability.r")
source("./children/study1/functions/drop1_para.r")
source("./children/study1/functions/boot_glmm.r")
options(scipen = 9999) # Avoid scientific notation in the output

# 2. Load data ----

# Optional: load all saved objects (models, bootstraps, plots) instead of
# re-running the long computations
# load("./children/study2/R_objects/Analysis_2.RData")

xdata <-
  read.csv2("./children/study2/data/data_study2B_children.csv",
           header = TRUE, na = c("NA", "")
  )

## 2.1 Exclude familiarisation trials ----
# Trials 1-3 are familiarisation trials, trials 4-7 are test trials
xdata <-
  xdata %>%
  filter(trial == 4 | trial == 5 | trial == 6 | trial == 7)
xdata$searched.in.targeted.box <- as.factor(xdata$searched.in.targeted.box)

# Check the outcome variable
xdata$searched.in.targeted.box

# 3. Participants ----
# Divided by 4 (test trials per child) to get the number of children
table(xdata$age.group)/4
table(xdata$gender)/4
tapply(as.numeric(xdata$age), xdata$age.group, mean)
tapply(as.numeric(xdata$age), xdata$age.group, min)
tapply(as.numeric(xdata$age), xdata$age.group, max)

mean(as.numeric(xdata$age))
sd(as.numeric(xdata$age))

# Parent-reported demographics: proportion of the 48 children per category
# (education: two parents per child, hence divided by 8)
round((table(xdata$ethnic.info, useNA = "always")/4)/48, 2)
xx <- rbind(xdata$education.parent1, xdata$education.parent2)
round((table(xx, useNA = "always")/8)/48, 2)
round((table(xdata$family.income, useNA = "always")/4)/48, 2)

# 4. Inter-rater reliability ----

## 4.1 Percentage agreement ----
interrater <-
  xdata %>%
  filter(!is.na(searched.in.targeted.box.reliability))
# Proportion of trials coded by the second coder (25%)
nrow(interrater) / nrow(xdata)
interrater$searched.in.targeted.box.reliability <-
  as.factor(interrater$searched.in.targeted.box.reliability)

# Number of trials on which the two coders disagreed
sum(as.numeric(interrater$searched.in.targeted.box.reliability) !=
   (as.numeric(interrater$searched.in.targeted.box)))
# Proportion of trials on which the two coders agreed
(sum(as.numeric(interrater$searched.in.targeted.box.reliability) ==
     as.numeric(interrater$searched.in.targeted.box))) / nrow(interrater)

## 4.2 Cohen's kappa ----
library(irr)
kappa_results <-
  kappa2(cbind(
    as.factor(interrater$searched.in.targeted.box.reliability),
    interrater$searched.in.targeted.box
  ), "unweighted")
print(kappa_results)

# 5. Descriptives ----
# Proportion of peeks into the previously available box (trial level)
prop.table(table(xdata$searched.in.targeted.box))

# 6. Model preparation ----

## 6.1 Check which random slopes are identifiable ----
# fe.re.tab() also returns the data with dummy-coded factors
xx.fe.re=fe.re.tab(fe.model="searched.in.targeted.box ~ trial",
                   re="(1|id)", data=xdata)
xx.fe.re$summary
t.data=xx.fe.re$data
str(t.data)

## 6.2 z-transform trial ----
# Because trial is z-transformed (mean = 0), the intercept is the log-odds of
# peeking into the available box at the average trial; its test against 0
# is therefore a test against chance (50%)
t.data$z.trial = scale(t.data$trial)

# Mean and SD of peeking into the available box
mean(as.numeric(t.data$searched.in.targeted.box) - 1)
sd(as.numeric(t.data$searched.in.targeted.box) - 1)

# 7. Model (preregistered): comparison with chance ----

## 7.1 Fit model and model without intercept ----
# chance.test_exp2:      intercept + trial
# chance.test_exp2.null: same model without intercept (fixed at chance level)
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

chance.test_exp2 <- glmer(searched.in.targeted.box ~ 1 + z.trial +
                            (1 + z.trial | id),
                          data = t.data, family = binomial, control = contr
)

chance.test_exp2.null   <- glmer(searched.in.targeted.box ~ 0 + z.trial +
    (1 + z.trial | id),
  data = t.data, family = binomial, control = contr
)

## 7.2 Model assumptions and stability ----
overdisp.test(chance.test_exp2) # Overdispersion
ranef.diagn.plot(chance.test_exp2)

# Refits the model leaving out one child at a time
m.stab.b <-
  glmm.model.stab(model.res = chance.test_exp2, contr = contr, use = c("id"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))

## 7.3 Results ----
round(summary(chance.test_exp2)$coefficient, 3)

# Intercept on the probability scale: 62.8% of peeks into the available box
plogis(0.523)

# Additional tests of the intercept: likelihood-ratio test against the model
# without intercept, and estimated marginal mean
anova(chance.test_exp2, chance.test_exp2.null, test = "LRT")
drop1(chance.test_exp2, test = "Chisq")

library(emmeans)
emmeans(chance.test_exp2, ~ 1, type = "response")
emmeans(chance.test_exp2, ~ 1) |> test(null = 0)

# Peeks per child
ftable(searched.in.targeted.box ~ id, t.data)

# Bootstrapped 95% CI of the intercept
boot <-
  boot.glmm.pred(
    model.res = chance.test_exp2, excl.warnings = T,
    nboots = 1000, para = F,
    level = 0.95)

# Estimate, lower CI limit and upper CI limit on the probability scale
# (values taken from boot$ci.estimates)
plogis(0.523)
plogis(0.221)
plogis(0.912)

as.data.frame(round(boot$ci.estimates, 3))
m.stab.plot(round(boot$ci.estimates, 3))

# 8. Figure 4c ----

## 8.1 Plot theme ----
my_theme <-
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification='left',
    legend.background = element_blank(),
    #legend.box.background = element_rect(colour = "black"),
    legend.text = element_text(size=15),

    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),

    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13), #, family = "Arial Narrow"),
    strip.text.x = element_text(size = 15),

    #text = element_text(family = "Arial Narrow"),

    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left = element_text(vjust = 3),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_text(
      margin =
        margin(t = 10, r = 0, b = 0, l = 0)
    ))

## 8.2 Data for the plot ----
# Model estimate and bootstrapped 95% CI of the intercept (logit scale)
conf.int <- as.data.frame(round(boot$ci.estimates, 3))["(Intercept)",]

# Proportion of peeks into the available box per child
xdata$searched.in.targeted.box.nr <- as.numeric(xdata$searched.in.targeted.box)-1
xdata.agg <- xdata %>%
  group_by(id) %>%
  summarise(mean.resp =
              mean(searched.in.targeted.box.nr, na.rm = T))

## 8.3 Plot ----
# Small dots and violin: individual children
# Large dot and error bar: model estimate with bootstrapped 95% CI
# Dotted line: chance level (50%)
plot_study2_children <-
  ggplot(
    data = xdata.agg,
    aes(x = 1, y = mean.resp)
  ) +
  geom_point(
    data = xdata.agg, color = "gray30", size = 1.5,
    alpha = .4, position = position_jitter(w = 0.2, h = 0.025)
  ) +
  geom_violin(
    data = xdata.agg,
    aes(x = 1, y = mean.resp),
    position = position_nudge(x = 0.00),
    fill = "gray70", alpha = .2, color = NA
  ) +
  geom_errorbar(
    data = conf.int,
    aes(
      x = 1, y = plogis(orig),
      ymin = plogis(X2.5.),
      ymax = plogis(X97.5.)
    ), color = "black",
    width = 0.1, linewidth = 1
  ) +
  geom_point(
    data = conf.int,
    aes(x = 1, y = plogis(orig),
    color = "brown1", size = 4.5)
  )  +
  geom_hline(yintercept=0.5, linetype='dotted', col = 'blue') +
  scale_x_continuous(
    name = "Children"
  ) +
  scale_y_continuous(
    name = "Searches in the available box",
    labels = scales::percent
  ) +
  theme_light() +
  my_theme +
  theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "none")

plot_study2_children

# 9. Attempts to obtain the remaining reward after peeking ----
# Exploratory (not preregistered), reported in the manuscript: did children
# try to get the reward in the box they had peeked into?
table(xdata$trying.to.get.remaining, useNA = "always")
sum(!is.na(xdata$trying.to.get.remaining)) # Test trials with a coding
sum(xdata$trying.to.get.remaining == "no", na.rm = TRUE) # No attempt
round(100 * mean(xdata$trying.to.get.remaining == "no", na.rm = TRUE), 1)

# 10. Save workspace ----
save.image("./children/study2/R_objects/Analysis_2.RData")
