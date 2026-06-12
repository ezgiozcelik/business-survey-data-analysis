
#BEEPS IV-V PANEL DATA VISUALIZATION
#MARGINAL EFFECTS PLOTS
#Author: Ezgi Ozcelik
#Date: 09/21/2025

# SETUP----

## opt out of scientific notation----
options(scipen = 999)

## set working directory----
setwd("/Users/ezgiozcelik/Desktop/businessgovernment")

## upload libraries----
library(ggplot2)
library(marginaleffects)
library(patchwork)
library(grid)

# VISUALIZATION----

## marginal effects (slopes)----

# for effects of r&d effots on access to government contracts
me_rd <- slopes(
  hre3,
  variables = "rd_binary",
  by = "regime_ca"
)
me_rd_df <- as.data.frame(me_rd)

# for effects of number of employees on access to government contracts
me_empl <- slopes(
  hre3,
  variables = "empl_w_s",
  by = "regime_ca"
)
me_empl_df <- as.data.frame(me_empl)

# for effects of fixed assets on access to government contracts
me_fix <- slopes(
  hre3,
  variables = "fixed_w_s",
  by = "regime_ca"
)
me_fix_df <- as.data.frame(me_fix)

# for effects of r&d efforts on access to government loans
mf_rd <- slopes(
  hre23,
  variables = "rd_binary",
  by = "regime_ca"
)
mf_rd_df <- as.data.frame(mf_rd)

# for effects of exporting on access to government loans
mf_exp <- slopes(
  hre23,
  variables = "expo_binary",
  by = "regime_ca"
)
mf_exp_df <- as.data.frame(mf_exp)

# for effects of number of employees on access to government loans
mf_empl <- slopes(
  hre23,
  variables = "empl_w_s",
  by = "regime_ca"
)
mf_empl_df <- as.data.frame(mf_empl)

# for effects of fixed assets on access to government loans
mf_fix <- slopes(
  hre23,
  variables = "fixed_w_s",
  by = "regime_ca"
)
mf_fix_df <- as.data.frame(mf_fix)

# for effects of r&d efforts on freedom from inspection
mg_rd <- slopes(
  hre33,
  variables = "rd_binary",
  by = "regime_ca"
)
mg_rd_df <- as.data.frame(mg_rd)

# for effects of exporting on freedom from inspection
mg_exp <- slopes(
  hre33,
  variables = "expo_binary",
  by = "regime_ca"
)
mg_exp_df <- as.data.frame(mg_exp)

# for effects of number of employees on freedom from inspection
mg_empl <- slopes(
  hre33,
  variables = "empl_w_s",
  by = "regime_ca"
)
mg_empl_df <- as.data.frame(mg_empl)

# for effects of fixed assets on freedom from inspection
mg_fix <- slopes(
  hre33,
  variables = "fixed_w_s",
  by = "regime_ca"
)
mg_fix_df <- as.data.frame(mg_fix)

## plots----
# choosing regime colors
regime_colors <- c(
  "Competitive Authoritarian" = "#C00000",
  "Full Autocracy"            = "#0072B2",
  "Democracy"                 = "#1B7F3B"
)

plot1 <- ggplot(me_rd_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "R&D's Effects on Access to Contracts",
       x = NULL,
       y = "Effect of R&D") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot2 <- ggplot(me_empl_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Firm Size's Effects on Access to Contracts",
       x = NULL,
       y = "Effect of # of Employee") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot3 <- ggplot(me_fix_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Fixed Assets' Effects on Access to Contracts",
       x = NULL,
       y = "Effect of Fixed Assets") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot4 <- ggplot(mf_rd_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "R&D's Effects on Access to Loans",
       x = NULL,
       y = "Effect of R&D") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot5 <- ggplot(mf_exp_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Exporting's Effects on Access to Loans",
       x = NULL,
       y = "Effect of Exporting") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot6 <- ggplot(mf_empl_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Firm Size's Effects on Access to Loans",
       x = NULL,
       y = "Effect of # of Employee") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot7 <- ggplot(mf_fix_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Fixed Assets' Effects on Access to Loans",
       x = NULL,
       y = "Effect of Fixed Assets") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot8 <- ggplot(mg_rd_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "R&D's Effects on Inspection Pressure",
       x = NULL,
       y = "Effect of R&D") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot9 <- ggplot(mg_exp_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Exporting's Effects on Inspection Pressure",
       x = NULL,
       y = "Effect of Exporting") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot10 <- ggplot(mg_empl_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Firm Size's Effects on Inspection Pressure",
       x = NULL,
       y = "Effect of # of Employee") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

plot11 <- ggplot(mg_fix_df, aes(x = regime_ca, y = estimate, color = regime_ca)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  scale_color_manual(values = regime_colors) +
  labs(title = "Fixed Assets' Effects on Inspection Pressure",
       x = NULL,
       y = "Effect of Fixed Assets") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

## final plot (grid) ----

row_title_grob <- function(text) {
  wrap_elements(
    full = textGrob(
      text,
      gp = gpar(fontsize = 11, fontface = "bold"),
      just = "center",
      y = unit(0.65, "npc")
    )
  )
}

final_plot <-
  row_title_grob("Access to Government Contracts") /
  ((plot1 | plot2 | plot3) +
     plot_layout(widths = c(1, 1, 1))) /
  row_title_grob("Access to Government Loans") /
  ((plot4 | plot5 | plot6 | plot7) +
     plot_layout(widths = c(0.75, 0.75, 0.75, 0.75))) /
  row_title_grob("Freedom from Inspection Pressure") /
  ((plot8 | plot9 | plot10 | plot11) +
     plot_layout(widths = c(0.75, 0.75, 0.75, 0.75))) &
  theme(
    legend.position = "none",
    plot.title = element_blank(),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6)
  )

final_plot
