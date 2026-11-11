# Chimpanzees and Human Children Explore Unchosen Options

Data, analysis code, and power simulations for the manuscript *Chimpanzees and Human Children Explore Unchosen Options* (under revision).

Supplementary materials and example videos are available on OSF: <https://osf.io/nu67c/?view_only=5469d97381ae4d4f8a5bb22b462e50c6>

---

## Contents

1. [Summary](#summary)
2. [Repository structure](#repository-structure)
3. [Reproducing the analyses](#reproducing-the-analyses)
4. [Software and package versions](#software-and-package-versions)
5. [Data files and variables](#data-files-and-variables)
6. [Ethics](#ethics)
7. [License](#license)

---

## Summary

### Abstract

Post-decisional information search is a behavioral indicator of epistemic curiosity—the intrinsic desire to acquire knowledge for its own sake—and, under certain conditions, of counterfactual curiosity: the drive to understand what might have occurred had a different choice been made. In two preregistered studies, we investigated whether chimpanzees and human children (a) engage in post-decisional information search, reflecting epistemic curiosity, and (b) increase this search under conditions reflecting counterfactual curiosity (when they had control over their decision or received an unfavorable outcome). In Study 1, chimpanzees and children chose between two boxes containing hidden rewards and could then peek into the unchosen box. Both species searched post-decisionally, with children's search increasing with age. However, only children showed heightened search following negative outcomes. In Study 2, which probed the effects of decisional control, participants chose between two accessible boxes while a third was inaccessible; afterward, both species preferred to peek into the remaining accessible box. Together, these findings provide the first comparative evidence that chimpanzees and children exhibit epistemic curiosity. Additionally, the search of both species was sensitive to decisional control, a core component of counterfactual curiosity, whereas only children's search was additionally sensitive to outcome valence.

### Studies at a glance

| Study | Sample | Design | Outcome variable |
|---|---|---|---|
| **1A** | 16 chimpanzees (6 males; 14–37 years) | Within-subject: choice, no-choice, and control conditions; 18 trials per condition (54 in total) | Peeked into the unchosen box (yes/no) |
| **1B** | 144 children (72 girls; 2.05–5.98 years) | Between-subject: choice, no-choice, or control condition; 4 test trials per child | Peeked into the unchosen box (yes/no) |
| **2A** | 9 chimpanzees (2 males; 14–37 years; subset of Study 1A) | Three boxes, two of them available to choose from; 18 trials per individual | Peeked into the box that *had been* available rather than the one that never was (yes/no) |
| **2B** | 48 children (21 girls; 4.04–5.99 years) | Same as 2A; 4 test trials per child | Same as 2A |

**Preregistrations (OSF):**
Study 1 – [chimpanzees](https://osf.io/5hxcj/?view_only=9d135e8375654ffba2eb4a839e1a9cba), [children](https://osf.io/kpae4/?view_only=56d108830a2d43e8b3b19e1bf6122c9c) ·
Study 2 – [chimpanzees](https://osf.io/3ngqt/?view_only=ffa94d71c6874dc697ec9ca96d2688cc), [children](https://osf.io/4zpu3/?view_only=88af6d1761bd4260973e479169412f03)

---

## Repository structure

```
.
├── Post_Decisional_Curiosity.Rproj   # RStudio project; open this first (sets the working directory)
├── README.md
├── Video_S1.mp4, Video_S2.mp4                          # example videos (also on OSF)
├── preregistrations/                                   # the four OSF preregistrations (.docx)
│
├── figures/                                            # all figures saved by the scripts
│   ├── Figure_2_Study_1A.pdf                           # Analysis_Study_1A.R
│   ├── Figure_3_Study_1B.pdf                           # Analysis_Study_1B.R
│   ├── Figure_4bc_Study_2.pdf                          # Combine_figures.R
│   ├── Figure_S1_Study_1A_turn_away_latency.png        # Analysis_Study_1A.R
│   └── Figure_S2_Study_2A_turn_away_latency.png        # Analysis_2A.R
│                                                       # (knitting the Study 2 power simulations also saves their plots here)
│
├── chimpanzees/
│   ├── study1/                                         # STUDY 1A
│   │   ├── data/
│   │   │   └── data_study1A_chimpanzees.csv            # raw trial-level data
│   │   ├── scripts/
│   │   │   ├── Analysis_Study_1A.R                     # main analysis + Figure 2
│   │   │   ├── Study_1A_Power_Simulation_with_control.Rmd
│   │   │   └── Study_1A_Power_Simulation_test_conditions.Rmd
│   │   ├── functions/                                  # helper functions (see below)
│   │   └── R_objects/
│   │       ├── analysis_1.RData                        # all objects created by the analysis script
│   │       ├── counterfactual_power_sim_with_control.RData
│   │       └── counterfactual_power_sim_test_conditions.RData
│   └── study2/                                         # STUDY 2A
│       ├── data/
│       │   └── data_study2A_chimpanzees.csv
│       ├── scripts/
│       │   ├── Analysis_2A.R                           # main analysis + Figure 4b (panel)
│       │   ├── Study_2A_Power_Simulation.Rmd
│       │   └── Combine_figures.R                       # combines Figure 4b and 4c, saves Figure 4
│       ├── functions/
│       └── R_objects/
│           ├── analysis_2.RData
│           └── study_2_counterfactual_power_sim.RData
│
└── children/
    ├── study1/                                         # STUDY 1B
    │   ├── data/
    │   │   └── data_study1B_children.csv
    │   ├── materials/
    │   │   ├── Instructions.docx                       # experimenter script
    │   │   └── Counterbalancing_Sheet_Study1_Kids.xlsx # trial orders and reward positions
    │   ├── scripts/
    │   │   ├── Analysis_Study_1B.R                     # main analysis + Figure 3
    │   │   ├── Study_1B_Power_Simulation_with_control.Rmd
    │   │   └── Study_1B_Power_Simulation_test_conditions.Rmd
    │   ├── functions/
    │   └── R_objects/
    │       ├── analysis_1B.RData
    │       ├── counterfactual_power_sim.RData          # all three conditions
    │       └── counterfactual_power_sim_test_only.RData
    └── study2/                                         # STUDY 2B
        ├── data/
        │   └── data_study2B_children.csv
        ├── scripts/
        │   ├── Analysis_2B.R                           # main analysis + Figure 4c (panel)
        │   └── Study_2B_Power_Simulation.Rmd
        ├── functions/
        └── R_objects/
            ├── Analysis_2.RData
            └── study_2_counterfactual_power_sim.RData
```

### Helper functions (`*/functions/`)

Based on GLMM utility functions by Roger Mundry. Each study folder has an identical copy; the Study 2 scripts use the copies in the corresponding Study 1 folders.

| File | Purpose |
|---|---|
| `diagnostic_fcns.r` | Model diagnostics (e.g. overdispersion, distribution of random effects) and `fe.re.tab()` for checking which random slopes are identifiable |
| `glmm_stability.r` | Model stability: refits the model leaving out one individual at a time (`glmm.model.stab()`) |
| `drop1_para.r` | Likelihood-ratio tests for individual fixed effects (`drop1p()`) |
| `boot_glmm.r` | Parametric bootstrap of model estimates and 95% confidence intervals (`boot.glmm.pred()`), used for the tables and figures. With `para = T`, the bootstrap runs in parallel and is reproducible after `set.seed()` |

---

## Reproducing the analyses

### Workflow

1. **Open `Post_Decisional_Curiosity.Rproj`** in RStudio. All analysis scripts use paths relative to the project root (e.g. `./chimpanzees/study1/data/...`). If you're not using RStudio, use `setwd()` to set the working directory to the repository root.
2. **Install the required packages** (see [Software and package versions](#software-and-package-versions)):
   ```r
   install.packages(c("lme4", "tidyverse", "car", "irr", "emmeans", "effects", "ggpubr", "gghalves", "ggthemes", "cowplot"))
   ```
3. **Run the analysis script** for the study you're interested in, from top to bottom:

   | Study | Script | Reproduces |
   |---|---|---|
   | 1A | `chimpanzees/study1/scripts/Analysis_Study_1A.R` | Results of Study 1A, Figure 2, Figure S1 |
   | 1B | `children/study1/scripts/Analysis_Study_1B.R` | Results of Study 1B, Figure 3 |
   | 2A | `chimpanzees/study2/scripts/Analysis_2A.R` | Results of Study 2A, Figure 4b, Figure S2 |
   | 2B | `children/study2/scripts/Analysis_2B.R` | Results of Study 2B, Figure 4c |
   | 2A + 2B | `chimpanzees/study2/scripts/Combine_figures.R` | Figure 4b–c (run after the 2A and 2B scripts, or with their saved workspaces) |

   All figures are saved to the `figures/` folder.

4. **Optional – skip the long computations** by loading the saved R objects (see below).

### Saved R objects: skip the long computations

Some steps take a long time to run, especially the bootstrapped confidence intervals (1,000 resamples per model). For this reason, each study comes with an **`.RData` file containing every object the analysis script creates**: the cleaned data, all fitted models, model comparisons, stability checks, bootstraps, and plots.

Load the file at the start of the script, then run only the parts you want to inspect:

| Study | Run at the start of the script |
|---|---|
| 1A | `load("./chimpanzees/study1/R_objects/analysis_1.RData")` |
| 1B | `load("./children/study1/R_objects/analysis_1B.RData")` |
| 2A | `load("./chimpanzees/study2/R_objects/analysis_2.RData")` |
| 2B | `load("./children/study2/R_objects/Analysis_2.RData")` |

Each analysis script already has this line, commented out, in its "Load data" section; just uncomment it. The results of the power simulations are saved the same way in the `R_objects/` folders.

### What each analysis script does

1. Loads the data and excludes familiarisation trials
2. Computes inter-rater reliability (percentage agreement and Cohen's κ on the 25% of trials coded by a second coder)
3. Reports sample demographics and descriptive statistics
4. Fits the preregistered binomial GLMMs (`lme4::glmer`, `bobyqa` optimiser):
   - **Study 1:** peeking (yes/no) ~ condition × reward value (× age for children) + sex + trial, with a random intercept for individual and random slopes where identifiable. The full model is compared with a null model, followed by likelihood-ratio tests of individual effects (`drop1p`)
   - **Study 2:** intercept-only model with z-transformed trial, testing whether peeking into the previously available box differs from chance (0.5)
5. Checks model assumptions (collinearity) and model stability
6. Bootstraps confidence intervals and produces the figures (saved to `figures/`)
7. Runs robustness analyses excluding the control condition (Study 1 only)
8. Runs additional exploratory analyses requested during peer review (reported in the manuscript and SI):
   - **Study 1A:** distribution of turn-away latencies (Figure S1); peeking across sessions and on the first trial of each session; latency to peek and robustness to shorter peeking windows (10, 15, 20 s)
   - **Study 1B:** attempts to obtain the remaining reward after peeking; latency to peek and robustness to shorter peeking windows
   - **Study 2A:** looking at the available box while its rope was removed (added as a predictor to the chance model); distribution of turn-away latencies (Figure S2)
   - **Study 2B:** attempts to obtain the remaining reward after peeking
9. Saves the workspace to `R_objects/`

### How the scripts are organised

All scripts follow the same layout, so you can find the same step in every study:

- **Header block** at the top: purpose, input files, output (results, figures, `.RData`), where to run the script from, and runtime notes.
- **Numbered sections** (`# 1. Setup ----`) and **subsections** (`## 1.1 Packages ----`). In RStudio these appear in the document outline (*Code → Show Document Outline*, or Ctrl/Cmd + Shift + O), where you can jump between them. The analysis scripts use the same section names across studies (Setup, Load data, Data preparation, Inter-rater reliability, Descriptives, Model preparation, Models, Bootstrapped confidence intervals, Figure, Robustness analysis, Save workspace).
- **Comments** above a block of code explain what the block does and why; short comments at the end of a line explain a single value or call. Notes starting with `NOTE:` flag known issues.
- **Power simulations** (`.Rmd`) use the same structure in every file: 1. Setup, 2. Simulated design, 3. Fixed effects implied by the hypotheses, 4. Random effects, 5. Results table, 6. Simulation, 7. Evaluation (convergence warnings and power). The Study 2 simulations have fewer parameters, so they combine these steps into four sections.

### Power simulations

The `*_Power_Simulation*.Rmd` files contain the simulation-based power analyses reported in the manuscript and the supplementary material (section "Power Simulations"): a priori for the chimpanzee studies (1A, 2A) and post hoc, for the designs as conducted, for the children's studies (1B, 2B). They use paths relative to their own `scripts/` folder, so knit them from there. Their results are stored in the corresponding `R_objects/` folders.

---

## Software and package versions

The final analyses (the `analysis_*.RData` workspaces) were run in **R 4.5.1** on macOS. The Study 1A analysis (`analysis_1.RData`, Figure 2, Figure S1) was last rerun in November 2026 with lme4 2.1.0, starting from `set.seed(123)`; with this seed, the script reproduces all reported Study 1A values, including the bootstrapped confidence intervals. Because the parallel bootstrap seeds one random-number stream per worker, the confidence intervals are reproduced exactly only with the same number of workers (8 cores, i.e. 7 workers with the default `n.cores = "all-1"`); with a different number of cores they can differ in the third decimal. The power simulations were run earlier, in R 4.3.3, except the two Study 1B simulations (`counterfactual_power_sim.RData` with all three conditions and `counterfactual_power_sim_test_only.RData` with the two test conditions, 48 children per condition as tested), which were run in R 4.5.1 (lme4 2.1.0) in November 2026.

| Package | Version | Used for |
|---|---|---|
| `lme4` | 2.0-6 | Fitting the GLMMs (`glmer`) |
| `Matrix` | 1.7-3 | Dependency of `lme4` |
| `tidyverse` | 2.0.0 | Data wrangling (`dplyr` 1.1.4, `tidyr` 1.3.1) and figures (`ggplot2` 4.0.1, `scales` 1.4.0) |
| `car` | 3.1-3 | Collinearity checks (`vif`) |
| `irr` | 0.84.1 | Inter-rater reliability (`kappa2`) |
| `emmeans` | 1.11.1 | Estimated marginal means and pairwise comparisons |
| `effects` | 4.2-2 | Quick visual inspection of model effects |
| `ggpubr` | 0.6.1 | Combining panels into the final figures (`ggarrange`, `theme_pubr`) |
| `gghalves` | 0.1.4 | Half-violin plots in Figures 2 and 3 |
| `ggthemes` | 5.2.0 | Plot themes in Figures 2 and 3 |
| `data.table` | 1.17.8 | Power simulations |
| `kyotil` | 2024.11-01 | Power simulations (`keepWarnings`) |
| `cowplot` | 1.1.3 | Figures 2 and 3; power simulations, Study 2 |
| `knitr` / `rmarkdown` | 1.50 / 2.29 | Knitting the power-simulation `.Rmd` files |

<details>
<summary><b>Full <code>sessionInfo()</code> output</b></summary>

```
R version 4.5.1 (2025-06-13)
Platform: aarch64-apple-darwin20
Running under: macOS Tahoe 26.7.1

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1


attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] rmarkdown_2.29    knitr_1.50        kyotil_2024.11-01 data.table_1.17.8
 [5] optimx_2025-4.9   readxl_1.4.5      cowplot_1.1.3     ggthemes_5.2.0
 [9] gghalves_0.1.4    ggpubr_0.6.1      effects_4.2-2     emmeans_1.11.1
[13] irr_0.84.1        lpSolve_5.6.23    car_3.1-3         carData_3.0-5
[17] lubridate_1.9.4   forcats_1.0.0     stringr_1.5.1     dplyr_1.1.4
[21] purrr_1.1.0       readr_2.1.5       tidyr_1.3.1       tibble_3.3.0
[25] ggplot2_4.0.1     tidyverse_2.0.0   lme4_2.0-6        Matrix_1.7-3

loaded via a namespace (and not attached):
 [1] tidyselect_1.2.1    farver_2.1.2        S7_0.2.1
 [4] fastmap_1.2.0       TH.data_1.1-5       pracma_2.4.4
 [7] digest_0.6.37       estimability_1.5.1  timechange_0.3.0
[10] lifecycle_1.0.4     survival_3.8-3      magrittr_2.0.3
[13] compiler_4.5.1      rlang_1.1.6         tools_4.5.1
[16] ggsignif_0.6.4      RColorBrewer_1.1-3  multcomp_1.4-32
[19] abind_1.4-8         withr_3.0.2         numDeriv_2016.8-1.1
[22] nnet_7.3-20         grid_4.5.1          xtable_1.8-4
[25] colorspace_2.1-1    scales_1.4.0        MASS_7.3-65
[28] dichromat_2.0-0.1   insight_1.5.4       cli_3.6.5
[31] mvtnorm_1.3-3       survey_4.4-2        reformulas_0.4.4
[34] generics_0.1.4      tzdb_0.5.0          minqa_1.2.8
[37] DBI_1.2.3           splines_4.5.1       parallel_4.5.1
[40] cellranger_1.1.0    mitools_2.4         vctrs_0.6.5
[43] boot_1.3-31         sandwich_3.1-1      hms_1.1.3
[46] rstatix_1.1.0       Formula_1.2-5       glue_1.8.0
[49] nloptr_2.2.1        codetools_0.2-20    stringi_1.8.7
[52] gtable_0.3.6        pillar_1.10.2       htmltools_0.5.8.1
[55] R6_2.6.1            Rdpack_2.6.4        evaluate_1.0.3
[58] lattice_0.22-7      rbibutils_2.3       backports_1.5.0
[61] broom_1.0.8         Rcpp_1.1.2          coda_0.19-4.1
[64] nlme_3.1-168        xfun_0.52           zoo_1.8-14
[67] pkgconfig_2.0.3
```

</details>

---

## Data files and variables

Each data file has **one row per trial**. Empty cells and `NA` both mean that no value was recorded, for example because the measure didn't apply on that trial (e.g. latency to peek when the participant did not peek).

| File | Delimiter | Read with | Rows |
|---|---|---|---|
| `chimpanzees/study1/data/data_study1A_chimpanzees.csv` | `;` | `read.csv2()` | 864 (16 chimpanzees × 54 trials) |
| `children/study1/data/data_study1B_children.csv` | `;` | `read.csv2()` | 1,008 (144 children × 3 familiarisation + 4 test trials) |
| `chimpanzees/study2/data/data_study2A_chimpanzees.csv` | `,` | `read.csv()` | 154 valid trials (9 chimpanzees × 18 trials, except Pasa: 10), followed by empty rows |
| `children/study2/data/data_study2B_children.csv` | `;` | `read.csv2()` | 336 (48 children × 3 familiarisation + 4 test trials) |

**Conventions used in all files**

- **Reward values:** `high` (chimpanzees: three banana pieces; children: a small toy or sticker), `low` (chimpanzees: one carrot piece; children: a plain dot sticker), `none` (empty box).
- **Columns ending in `.reli`** (or `.reliability`) hold the second coder's coding of the variable of the same name, for the randomly selected 25% of trials that were double-coded. They are empty for all other trials.
- Some files contain trailing columns without a header or any values. They can be ignored.

### Study 1A – chimpanzees (`data_study1A_chimpanzees.csv`)

| Variable | Description | Values |
|---|---|---|
| `id` | Chimpanzee identifier | 1–16 |
| `name` | Chimpanzee name | text |
| `gender` | Sex of the chimpanzee | `male`, `female` |
| `birth_year` | Year of birth | year |
| `age` | Age at testing (years) | 14–37 |
| `day` | Test session (each on a different day) | 1–9 |
| `condition_day` | Condition and session number within that condition | e.g. `choice_1`, `no-choice_3`, `control_2` |
| `condition` | Experimental condition | `choice`, `no-choice`, `control` |
| `trial` | Trial number within the session | 1–6 |
| `left`, `right` | Reward in the left and right box | `high`, `low`, `none` |
| `side.chosen` | Box the chimpanzee pulled (`none` = no box pulled) | `left`, `right`, `none` |
| `repetition` | Whether the trial was a repetition of an invalid trial | `yes`, `no`, empty |
| `repetition.reason` | Why the trial was repeated | e.g. `experimenter error`, `pulled both` |
| `food.received` | Reward in the chosen box | `high`, `low`, `none` |
| `food.remaining` | Reward in the unchosen box | `high`, `low`, `none` |
| **`searched`** | **Outcome: whether the chimpanzee peeked behind the occluder into the unchosen box** | `yes`, `no` |
| `seconds.until.searched` | Latency (s) until the chimpanzee peeked | 0–30 |
| `value.remaining.seen` | Value of the remaining reward compared with the reward received | `better`, `same`, `worse` |
| `turned.away` | Whether the chimpanzee turned away from the remaining reward after peeking | `yes`, `no` |
| `time.from.last.gaze.to.back.of.head.to.camera` | Time (s) from the chimpanzee's last look at the remaining reward until it turned away (back of the head facing the camera) | seconds |
| `emotional.displays` | Whether the chimpanzee showed an emotional display after peeking | `yes`, `no` |
| `comments` | Free-text notes by the coder or experimenter | text |
| `*.reli` | Second coder's coding of `side.chosen`, `food.received`, `food.remaining`, `searched`, `seconds.until.searched`, `value.remaining.seen`, `turned.away`, and `time.from.last.gaze...` | as above |

### Study 1B – children (`data_study1B_children.csv`)

| Variable | Description | Values |
|---|---|---|
| `id` | Child identifier | number |
| `condition` | Experimental condition the child was assigned to | `choice`, `no.choice`, `control` |
| `age.group` | Age in whole years | 2, 3, 4, 5 |
| `age` | Exact age (years, decimal) | 2.05–5.98 |
| `gender` | Gender of the child | `female`, `male` |
| `ethnic.info` | Child's ethnicity, as reported by the parent | text; `NA` = not reported |
| `education.parent1`, `education.parent2` | Highest level of education of each parent, as reported by the parent | text; `NA` = not reported |
| `family.income` | Annual family income bracket (USD), as reported by the parent | text; `NA` = not reported |
| `condition.trial` | Phase of the trial: familiarisation (`Fam_1`, `Fam_2`) or test (named after the condition). **Familiarisation trials are excluded from the analyses** | `Fam_1`, `Fam_2`, `choice`, `no.choice`, `control` |
| `trial` | Trial number within the phase (familiarisation: 1–3; test: 1–4) | 1–4 |
| `trial.total` | Running trial number across the session | 1–7 |
| `left`, `right` | Reward in the left and right box | `high`, `low`, `none` |
| `side.chosen` | Box the child pulled | `left`, `right` |
| `reward.received` | Reward in the chosen box | `high`, `low`, `none` |
| `reward.remaining` | Reward in the unchosen box | `high`, `low`, `none` |
| **`searched`** | **Outcome: whether the child peeked over the panel into the unchosen box** | `yes`, `no` |
| `trying.to.get.remaining` | Whether the child tried to obtain the remaining reward | `yes`, `no`, `maybe` |
| `seconds.until.searched` | Latency (s) until the child peeked | 0–30 |
| `value.remaining.seen` | Value of the remaining reward compared with the reward received | `better`, `same`, `worse` |
| `emotions.after.seeing.remaining` | Child's emotional reaction after seeing the remaining reward | e.g. `neutral`, `positive`, `negative`, `surprised` |
| `searched.reli`, `seconds.until.searched.reli` | Second coder's coding | as above |

### Study 2A – chimpanzees (`data_study2A_chimpanzees.csv`)

| Variable | Description | Values |
|---|---|---|
| `id` | Chimpanzee identifier | 1–9 |
| `name` | Chimpanzee name (same individuals as in Study 1A) | text |
| `birth_year` | Year of birth | year |
| `age` | Age at testing (years) | 14–37 |
| `gender` | Sex of the chimpanzee | `male`, `female` |
| `session` | Test session | 1, 2 |
| `trial` | Running number of the valid trials | 1–18 |
| `left`, `middle`, `right` | Reward in each box. The middle box was never covered and always contained a high-value reward | `high`, `low`, `none` |
| `side.available` | Which outer box was available to choose alongside the middle box (both outer boxes had a rope attached, but only the rope of the available box reached into the chimpanzee's room) | `left`, `right` |
| **`searched.in.available`** | **Outcome: whether the chimpanzee peeked into the outer box that had been available (rather than the one that never was)** | `yes`, `no` |
| `searched.in.available.reli` | Second coder's coding | `yes`, `no` |
| `look.towards.box.while.rope.removed.2sec` | Whether the chimpanzee looked towards the previously available box within 2 s while the experimenter removed its rope (additional analysis, `Analysis_2A.R` Section 9) | `yes`, `no`, `NA` = could not be coded |
| `turned.away.in.sec` | Latency (s) until the chimpanzee turned away after peeking | seconds |
| `notes` | Notes of the video coder | text |

Trials in which the chimpanzee chose an outer box or did not peek were repeated (see manuscript).

### Study 2B – children (`data_study2B_children.csv`)

| Variable | Description | Values |
|---|---|---|
| `id` | Child identifier | 1–48 |
| `age.group` | Age in whole years | 4, 5 |
| `age` | Exact age (years, decimal) | 4.04–5.99 |
| `gender` | Gender of the child | `female`, `male` |
| `testday` | Date of testing | M/D/YY |
| `ethnic.info`, `education.parent1`, `education.parent2`, `family.income` | Parent-reported demographics (as in Study 1B) | text; `NA` = not reported |
| `condition.trial` | Phase of the trial: familiarisation (`Fam_1`, `Fam_2`) or test (`Choice`). **Only test trials are analysed** | `Fam_1`, `Fam_2`, `Choice` |
| `additional.instruction.to.look.in.Fam.2` | Whether the child needed an extra prompt to peek during familiarisation 2 | `yes`, `no` |
| `trial` | Running trial number (familiarisation: 1–3; test: 4–7) | 1–7 |
| `left`, `middle`, `right` | Reward in each box. **`(R)` marks a box whose rope was placed within the child's reach, i.e. a box that was available to choose.** The middle box always contained a high-value reward | e.g. `high`, `low (R)`, `none (R)` |
| `side.chosen` | Box the child pulled | `left`, `middle`, `right` |
| `reward.received` | Reward in the chosen box | `high`, `low` |
| `trial.repeated` | Whether the test trial had to be repeated (e.g. because the child chose an outer box); the row contains the final, valid attempt | `yes`, `no` |
| `count.of.repeated.trials` | How many times the trial was repeated | 1–3 |
| `searched.in.targeted.box.before` | Familiarisation 2 only: whether the child peeked into the boxes before choosing | `yes`, `no` |
| **`searched.in.targeted.box`** | **Outcome (test trials): whether the child peeked into the outer box that had been available (rather than the one that never was)** | `yes`, `no` |
| `searched.in.targeted.box.reliability` | Second coder's coding of the outcome | `yes`, `no` |
| `trying.to.get.remaining` | Whether the child tried to obtain the remaining reward | `yes`, `no` |
| `value.remaining.seen` | Reward in the box the child peeked into | `high`, `low`, `none` |

---

## Ethics

The chimpanzee studies were approved by the relevant institutional ethics committee, the sanctuary's management trust, and the national wildlife and science authorities. They followed the PASA Primate Veterinary Healthcare Manual and the guidelines of the Association for the Study of Animal Behaviour. Chimpanzees were never deprived of food or water and participated voluntarily. The child studies were approved by the institutional ethics committee; parents gave written consent and children gave verbal assent. Child data are de-identified.

---

## License

[![CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

This repository, including data, code, and materials, is licensed under a [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/). You are free to share and adapt the material for any purpose, provided you give appropriate credit by citing the associated publication.
