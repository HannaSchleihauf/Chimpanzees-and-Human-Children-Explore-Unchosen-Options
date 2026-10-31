# ==============================================================================
# Study 2A and 2B - Combine figures
# ==============================================================================
#
# Purpose:  Combines the Study 2 plots of chimpanzees (Figure 4b) and
#           children (Figure 4c) into one figure.
# Input:    Saved workspaces of Analysis_2A.R (plot_study2_chimpanzees) and
#           Analysis_2B.R (plot_study2_children)
# Output:   figures/Figure_4bc_Study_2.pdf
# Run from: Project root (open Post_Decisional_Curiosity.Rproj first)
#
# ==============================================================================

# 1. Load saved workspaces ----
# Study 2A workspace (plot_study2_chimpanzees), then Study 2B workspace
# (plot_study2_children)
load("./chimpanzees/study2/R_objects/analysis_2.RData")
load("./children/study2/R_objects/Analysis_2.RData")


# 2. Combine plots ----
library(ggpubr)
# theme_set(theme_pubr())
figure <- ggarrange(plot_study2_chimpanzees, plot_study2_children,
                    labels = c(
                      "(a)",
                      "(b)"
                    ),
                    # label.x = 0,    # X position 0 for left
                    # label.y = 1,    # Y position 1 for top
                    hjust = -1, # Horizontal position of the panel labels
                    vjust = 2.5, # Vertical position of the panel labels
                    ncol = 2, nrow = 1
)
figure

# 3. Save Figure 4b-c ----
ggsave("./figures/Figure_4bc_Study_2.pdf", figure,
       width = 8, height = 5)
