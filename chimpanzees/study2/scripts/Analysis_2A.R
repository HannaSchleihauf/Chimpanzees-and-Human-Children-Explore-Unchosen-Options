# ==============================================================================
# Study 2A (chimpanzees) - Main analysis
# ==============================================================================
#
# Purpose:  Tests whether chimpanzees, after choosing the middle box, peek
#           into the outer box that had been available to choose more often
#           than expected by chance (50%), rather than into the outer box
#           that never was.
# Input:    chimpanzees/study2/data/data_study2A_chimpanzees.csv
# Output:   Results of Study 2A, Figure 4b (plot_study2_chimpanzees),
#           Figure S2 (saved to figures/), additional analyses for the
#           reviewers (Sections 9 and 10, reported in the manuscript and SI),
#           chimpanzees/study2/R_objects/analysis_2.RData
# Run from: Project root (open Post_Decisional_Curiosity.Rproj first)
# Runtime:  The bootstrap (Section 6.3) takes a while. To skip it, load the
#           saved workspace (see Section 2).
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
# GLMM utility functions by Roger Mundry (see README); the copy in the Study 1A
# folder is used
source("./chimpanzees/study1/functions/diagnostic_fcns.r")
source("./chimpanzees/study1/functions/glmm_stability.r")
source("./chimpanzees/study1/functions/drop1_para.r")
source("./chimpanzees/study1/functions/boot_glmm.r")

# 2. Load data ----

# Optional: load all saved objects (models, bootstraps, plots) instead of
# re-running the long computations
# load("./chimpanzees/study2/R_objects/analysis_2.RData")

xdata <-
  read.csv("./chimpanzees/study2/data/data_study2A_chimpanzees.csv",
           header = TRUE, na = c("NA", "")
  )

# 3. Inter-rater reliability ----

## 3.1 Disagreements ----
# Number of trials on which the two coders disagreed
sum(xdata$searched.in.available.reli !=
  xdata$searched.in.available, na.rm = TRUE)

## 3.2 Cohen's kappa ----
library(irr)
kappa_results <-
  kappa2(cbind(
    xdata$searched.in.available.reli,
    xdata$searched.in.available
  ), "unweighted")
print(kappa_results)

# 4. Descriptives ----
# Proportion of peeks into the previously available box (trial level)
prop.table(table(xdata$searched.in.available))

# 5. Model preparation ----

## 5.1 Check which random slopes are identifiable ----
# fe.re.tab() also returns the data with dummy-coded factors
xx.fe.re <- fe.re.tab(
  fe.model = "searched.in.available ~ trial",
  re = "(1|name)", data = xdata
)
xx.fe.re$summary
t.data <- xx.fe.re$data
str(t.data)

## 5.2 z-transform trial ----
# Because trial is z-transformed (mean = 0), the intercept is the log-odds of
# peeking into the available box at the average trial; its test against 0
# is therefore a test against chance (50%)
t.data$z.trial <- scale(t.data$trial)

# Mean and SD of peeking into the available box
mean(as.numeric(t.data$searched.in.available) - 1)
sd(as.numeric(t.data$searched.in.available) - 1)

# 6. Model (preregistered): comparison with chance ----

## 6.1 Fit model and model without intercept ----
# chance.test_exp2:      intercept + trial
# chance.test_exp2.null: same model without intercept (fixed at chance level)
contr <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000000))

chance.test_exp2 <- glmer(
  searched.in.available ~ 1 + z.trial +
    (1 + z.trial | name),
  data = t.data, family = binomial, control = contr
)

chance.test_exp2.null   <- glmer(
  searched.in.available ~ 0 + z.trial +
    (1 + z.trial | name),
  data = t.data, family = binomial, control = contr
)

## 6.2 Model assumptions and stability ----
overdisp.test(chance.test_exp2) # Overdispersion
ranef.diagn.plot(chance.test_exp2)

# Refits the model leaving out one chimpanzee at a time
m.stab.b <-
  glmm.model.stab(model.res = chance.test_exp2, contr = contr, use = c("name"))
m.stab.b$detailed$warnings
as.data.frame(round(m.stab.b$summary[, -1], 3))
m.stab.plot(round(m.stab.b$summary[, -1], 3))

## 6.3 Results ----
round(summary(chance.test_exp2)$coefficient, 3)

# Intercept on the probability scale: 62.1% of peeks into the available box
plogis(0.494)

# Additional tests of the intercept: likelihood-ratio test against the model
# without intercept, and estimated marginal mean
anova(chance.test_exp2, chance.test_exp2.null, test = "LRT")
drop1(chance.test_exp2, test = "Chisq")

library(emmeans)
emmeans(chance.test_exp2, ~ 1, type = "response")
emmeans(chance.test_exp2, ~ 1) |> test(null = 0)

# Peeks per chimpanzee
ftable(searched.in.available ~ name, t.data)

# Bootstrapped 95% CI of the intercept
boot <-
  boot.glmm.pred(
    model.res = chance.test_exp2, excl.warnings = T,
    nboots = 1000, para = F,
    level = 0.95
  )

# Lower CI limit, estimate and upper CI limit on the probability scale
# (values taken from boot$ci.estimates)
plogis(0.126)
plogis(0.494)
plogis(0.909)

as.data.frame(round(boot$ci.estimates, 3))
m.stab.plot(round(boot$ci.estimates, 3))

# 7. Figure 4b ----

## 7.1 Plot theme ----
my_theme <-
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.background = element_blank(),
    # legend.box.background = element_rect(colour = "black"),
    legend.text = element_text(size = 15),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 13), # , family = "Arial Narrow"),
    strip.text.x = element_text(size = 15),

    # text = element_text(family = "Arial Narrow"),

    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    axis.title.y.left = element_text(vjust = 3),
    plot.title = element_text(color = "black", size = 15, face = "bold"),
    axis.title.x = element_text(
      margin =
        margin(t = 10, r = 0, b = 0, l = 0)
    )
  )

## 7.2 Data for the plot ----
# Model estimate and bootstrapped 95% CI of the intercept (logit scale)
conf.int <- as.data.frame(round(boot$ci.estimates, 3))["(Intercept)",]

# Outcome as 0/1
xdata$searched.in.available.nr <- NA
xdata <-
  xdata %>%
  mutate(searched.in.available.nr = ifelse(searched.in.available == "yes", 1,
    ifelse(searched.in.available == "no", 0, NA)
  ))

# Proportion of peeks into the available box per chimpanzee
xdata.agg <- xdata %>%
  group_by(name) %>%
  summarise(
    mean.resp =
      mean(searched.in.available.nr, na.rm = T)
  ) %>%
  na.omit()

## 7.3 Plot ----
# Small dots and violin: individual chimpanzees
# Large dot and error bar: model estimate with bootstrapped 95% CI
# Dotted line: chance level (50%)
plot_study2_chimpanzees <-
  ggplot(
    data = xdata.agg,
    aes(x = 1, y = mean.resp)
  ) +
  theme_light() +
  geom_point(
    data = xdata.agg, color = "gray30", size = 1.5,
    alpha = .4, position = position_jitter(w = 0.25, h = 0)
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
    aes(x = 1, y = plogis(orig), ),
    color = "brown1", size = 4.5
  ) +
  geom_hline(yintercept = 0.5, linetype = "dotted", col = "blue") +
  scale_x_continuous(
    name = "Chimpanzees"
  ) +
  scale_y_continuous(
    name = "Searches in the available box",
    limits = c(0, 1),
    labels = scales::percent,
  ) +
  my_theme +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot_study2_chimpanzees

# 8. Individual chimpanzees ----
# Proportion of peeks into the available box per chimpanzee
xdataind <- xdata %>%
  group_by(name, searched.in.available) %>%
  count(searched.in.available)
xdataind <- pivot_wider(xdataind, names_from = searched.in.available, values_from = n)
xdataind$prob <- xdataind$yes / (xdataind$yes + xdataind$no)

xdata_plot <- ggplot(data = xdataind, aes(y = prob, x = name)) +
  geom_boxplot() +
  ylab("Proportion of information search in available box") +
  xlab("") +
  geom_hline(yintercept = 0.5, linetype = "dotted", col = "blue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(axis.title = element_text(size = 15)) +
  theme(axis.text = element_text(size = 15))

xdata_plot

# 9. Additional analysis: looking at the box while the rope was removed ----
# Did chimpanzees look towards the box (within 2 s) while the experimenter
# removed its rope, and did this affect whether they later peeked into the
# previously available box? Same model as in Section 6, with looking as an
# additional predictor.

## 9.1 Select trials in which looking could be coded ----
# Same data as in Section 2; look.towards.box.while.rope.removed.2sec could
# not be coded in 11 of the 154 valid trials, which are excluded here
look.data <- xdata[!is.na(xdata$name), ]
look.data <-
  look.data[!is.na(look.data$look.towards.box.while.rope.removed.2sec), ]
look.data$searched.in.available <- as.factor(look.data$searched.in.available)
look.data$looked <- as.factor(look.data$look.towards.box.while.rope.removed.2sec)

## 9.2 Descriptives ----
# How often did the chimpanzees look at the box while the rope was removed?
nrow(look.data)
table(look.data$looked)
round(prop.table(table(look.data$looked)), 3)

# Proportion of trials with looking per chimpanzee, then mean and SD across
# chimpanzees
looked.per.subject <- tapply(look.data$looked == "yes", look.data$name, mean)
round(looked.per.subject, 2)
mean(looked.per.subject)
sd(looked.per.subject)

# Peeking into the available box, depending on looking
table(looked = look.data$looked, searched = look.data$searched.in.available)
round(prop.table(table(looked = look.data$looked,
                       searched = look.data$searched.in.available), 1), 3)

## 9.3 Model preparation ----
# Check which random slopes are identifiable
xx.fe.re.look <- fe.re.tab(
  fe.model = "searched.in.available ~ looked + trial",
  re = "(1|name)", data = look.data
)
xx.fe.re.look$summary
t.data.look <- xx.fe.re.look$data

# z-transform trial; center the dummy variable for looking (needed for the
# random slope, and so that the intercept is still the comparison with chance)
t.data.look$z.trial <- scale(t.data.look$trial)
t.data.look$looked.code <-
  t.data.look$looked.yes - mean(t.data.look$looked.yes)

## 9.4 Fit model and null model ----
# Same model as in Section 6, plus looking as fixed effect and random slope.
# The correlations between random intercept and random slopes are not
# estimated: with them, the model was singular (correlation of -1), because
# some chimpanzees (almost) never looked at the box.
# looking.model: looking + trial
# looking.null:  same model without looking
looking.model <- glmer(
  searched.in.available ~ looked.code + z.trial +
    (1 + looked.code + z.trial || name),
  data = t.data.look, family = binomial, control = contr
)

looking.null <- glmer(
  searched.in.available ~ z.trial +
    (1 + looked.code + z.trial || name),
  data = t.data.look, family = binomial, control = contr
)

summary(looking.model)$varcor
round(summary(looking.model)$coefficients, 3)

## 9.5 Model assumptions and stability ----
overdisp.test(looking.model) # Overdispersion
ranef.diagn.plot(looking.model)

# Refits the model leaving out one chimpanzee at a time
m.stab.look <-
  glmm.model.stab(model.res = looking.model, contr = contr, use = c("name"))
m.stab.look$detailed$warnings
as.data.frame(round(m.stab.look$summary[, -1], 3))
m.stab.plot(round(m.stab.look$summary[, -1], 3))

## 9.6 Results ----
# Likelihood-ratio test of looking
anova(looking.model, looking.null, test = "LRT")
drop1(looking.model, test = "Chisq")

# Estimated probability of peeking into the available box, with and without
# looking (model with looking as factor, otherwise identical)
looking.model.factor <- glmer(
  searched.in.available ~ looked + z.trial +
    (1 + looked.code + z.trial || name),
  data = t.data.look, family = binomial, control = contr
)
emmeans(looking.model.factor, ~looked, type = "response")

# Bootstrapped 95% CIs of the estimates
boot.look <-
  boot.glmm.pred(
    model.res = looking.model, excl.warnings = T,
    nboots = 1000, para = F,
    level = 0.95
  )
as.data.frame(round(boot.look$ci.estimates, 3))
m.stab.plot(round(boot.look$ci.estimates, 3))

# 10. Latency to turn away after peeking (Figure S2) ----
# Same analysis as in Study 1A: turning away within 2 s (criterion preregistered
# for Study 1A)
# and the distribution of the latencies from the last look at the box the
# chimpanzee peeked into until it turned away. Latencies could
# not be coded for 16 peeks (e.g., chimpanzee out of frame or not on video).
# In two trials, the video stopped while the chimpanzee was still looking, so
# these latencies are minimum values.

## 10.1 Descriptives ----
turn.away.latency <- xdata$turned.away.in.sec[!is.na(xdata$name)]
sum(!is.na(xdata$name)) # Number of peeks (one per valid trial)
sum(!is.na(turn.away.latency)) # Peeks for which turning away could be coded
sum(is.na(turn.away.latency)) # Peeks that could not be coded
turn.away.latency <- turn.away.latency[!is.na(turn.away.latency)]

sum(turn.away.latency <= 2) # Within 2 seconds (criterion preregistered for Study 1A)
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
max(turn.away.latency) # Longest latency

## 10.2 Histogram (Figure S2) ----
# Vertical lines: 2 s (criterion preregistered for Study 1A), median, 5 s and 10 s
# (same steps in Figures S1 and S2)
latency.data <- data.frame(latency = turn.away.latency)

plot_turn_away_2A <-
  ggplot(latency.data, aes(x = latency)) +
  geom_histogram(binwidth = 1, boundary = 0.5, fill = "grey60", color = "white") +
  geom_vline(xintercept = 2, linetype = "solid", linewidth = 0.6) +
  geom_vline(xintercept = latency.median, linetype = "dashed", linewidth = 0.6) +
  geom_vline(xintercept = 5, linetype = "dotdash", linewidth = 0.6) +
  geom_vline(xintercept = 10, linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = 12, y = Inf, vjust = 1.5, hjust = 0,
           label = paste0("solid line: 2 s, criterion preregistered for Study 1A (",
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
  scale_x_continuous(breaks = seq(0, 30, by = 5), expand = c(0.01, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Latency from last look at the box to turning away (s)",
       y = "Number of peeks") +
  theme_classic(base_size = 12)
plot_turn_away_2A

ggsave("./figures/Figure_S2_Study_2A_turn_away_latency.png", plot_turn_away_2A,
       width = 7, height = 4, dpi = 300)

# 11. Save workspace ----
save.image("./chimpanzees/study2/R_objects/analysis_2.RData")
