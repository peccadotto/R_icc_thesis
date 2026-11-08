# ==============================================================================
# RELIABILITY AND AGREEMENT ANALYSIS SCRIPT (ICC, COHEN'S KAPPA, BLAND-ALTMAN)
# ==============================================================================

# --- 1. CHECK AND INSTALL REQUIRED PACKAGES ---

packages <- c(
  "dplyr",
  "ggplot2",
  "ICC",
  "irr",
  "janitor",
  "openxlsx",
  "readxl",
  "svglite",
  "tidyr"
)

check_packages <- function(pkg_list) {
  new_packages <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    message("The following missing packages will be installed: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages)
  } else {
    message("All required packages are already installed")
  }
  invisible(lapply(pkg_list, library, character.only = TRUE))
  message("All packages have been successfully loaded")
}

check_packages(packages)


# --- 2. CHECK REQUIRED FOLDERS AND FILES ---

if (!dir.exists("data")) {
  dir.create("data")
  message("Folder 'data' created")
}

if (!dir.exists("export")) {
  dir.create("export")
  message("Folder 'export' created")
}

excel_files <- list.files(
  path = "data", 
  pattern = "(?i)\\.xlsx?$", 
  full.names = TRUE
)
excel_files <- excel_files[!grepl("^~\\$", basename(excel_files))]
num_files <- length(excel_files)

if (num_files == 1) {
  my_data <- read_excel(excel_files)
} else if (num_files == 0) {
  stop("Error: No Excel file found in the 'data' folder.")
} else {
  stop(paste(
    "Error: Found", num_files, "Excel files in the 'data' folder.",
    "Please remove unnecessary files."
  ))
}


# --- 3. PROJECT PARAMETERS & SAMPLE SIZE ESTIMATION ---

expected_icc <- 0.80     # Expected ICC value to find (based on pilot studies or literature)
num_raters <- 2          # Number of raters/observers (e.g., op1 and op2)
desired_width <- 0.10    # Desired total width of the Confidence Interval (e.g., 0.10 -> +/- 0.05)
alpha <- 0.05            # Significance level (for a 95% CI)

# Calculation of the required number of subjects (patients) based on Bonett's (2002) approach:
# N = 8 * z^2 * (1 - r)^2 * [1 + (k - 1)*r]^2 / [k * (k - 1) * w^2]
# https://pubmed.ncbi.nlm.nih.gov/12111881/
# https://onlinelibrary.wiley.com/doi/10.1002/sim.1108
z <- qnorm(1 - alpha / 2)
w <- desired_width

n_subjects <- (8 * z^2 * (1 - expected_icc)^2 * (1 + (num_raters - 1) * expected_icc)^2) / 
  (num_raters * (num_raters - 1) * w^2)

# Round up to the next integer
n_required <- ceiling(n_subjects)

cat("Estimated number of subjects/patients required:", n_required, "\n")


# --- 4. DATABASE IMPORT AND DATA MANIPULATION ---

df <- read_excel(excel_files) |> clean_names() |>
  mutate(
    
    # --- Operator 1 Round 1 ---
    cat_op1_round1 = case_when(
      rx_op1_round1 < 30 ~ "Stable (< 30%)",
      rx_op1_round1 >= 30 & rx_op1_round1 < 40 ~ "Alarm zone (30-39%)",
      rx_op1_round1 >= 40 & rx_op1_round1 < 50 ~ "Preventive surgery (40-49%)",
      rx_op1_round1 >= 50 & rx_op1_round1 < 90 ~ "Reconstructive surgery (50-89%)",
      rx_op1_round1 >= 90 ~ "Salvage surgery (>= 90%)"
    ),
    cat_op1_round1 = factor(
      cat_op1_round1,
      levels = c(
        "Stable (< 30%)",
        "Alarm zone (30-39%)",
        "Preventive surgery (40-49%)",
        "Reconstructive surgery (50-89%)",
        "Salvage surgery (>= 90%)"
      )
    ),
    
    # --- Operator 1 Round 2 ---
    cat_op1_round2 = case_when(
      rx_op1_round2 < 30 ~ "Stable (< 30%)",
      rx_op1_round2 >= 30 & rx_op1_round2 < 40 ~ "Alarm zone (30-39%)",
      rx_op1_round2 >= 40 & rx_op1_round2 < 50 ~ "Preventive surgery (40-49%)",
      rx_op1_round2 >= 50 & rx_op1_round2 < 90 ~ "Reconstructive surgery (50-89%)",
      rx_op1_round2 >= 90 ~ "Salvage surgery (>= 90%)"
    ),
    cat_op1_round2 = factor(
      cat_op1_round2,
      levels = c(
        "Stable (< 30%)",
        "Alarm zone (30-39%)",
        "Preventive surgery (40-49%)",
        "Reconstructive surgery (50-89%)",
        "Salvage surgery (>= 90%)"
      )
    ),
    
    # --- Operator 2 Round 1 ---
    cat_op2_round1 = case_when(
      rx_op2_round1 < 30 ~ "Stable (< 30%)",
      rx_op2_round1 >= 30 & rx_op2_round1 < 40 ~ "Alarm zone (30-39%)",
      rx_op2_round1 >= 40 & rx_op2_round1 < 50 ~ "Preventive surgery (40-49%)",
      rx_op2_round1 >= 50 & rx_op2_round1 < 90 ~ "Reconstructive surgery (50-89%)",
      rx_op2_round1 >= 90 ~ "Salvage surgery (>= 90%)"
    ),
    cat_op2_round1 = factor(
      cat_op2_round1,
      levels = c(
        "Stable (< 30%)",
        "Alarm zone (30-39%)",
        "Preventive surgery (40-49%)",
        "Reconstructive surgery (50-89%)",
        "Salvage surgery (>= 90%)"
      )
    ),
    
    # --- Operator 2 Round 2 ---
    cat_op2_round2 = case_when(
      rx_op2_round2 < 30 ~ "Stable (< 30%)",
      rx_op2_round2 >= 30 & rx_op2_round2 < 40 ~ "Alarm zone (30-39%)",
      rx_op2_round2 >= 40 & rx_op2_round2 < 50 ~ "Preventive surgery (40-49%)",
      rx_op2_round2 >= 50 & rx_op2_round2 < 90 ~ "Reconstructive surgery (50-89%)",
      rx_op2_round2 >= 90 ~ "Salvage surgery (>= 90%)"
    ),
    cat_op2_round2 = factor(
      cat_op2_round2,
      levels = c(
        "Stable (< 30%)",
        "Alarm zone (30-39%)",
        "Preventive surgery (40-49%)",
        "Reconstructive surgery (50-89%)",
        "Salvage surgery (>= 90%)"
      )
    )
  )


# --- 5. CALCULATION OF AGREEMENT COEFFICIENTS ---

df_op1 <- df |> select(rx_op1_round1, rx_op1_round2)
df_op2 <- df |> select(rx_op2_round1, rx_op2_round2)
df_inter <- df |> select(rx_op2_round1, rx_op1_round1)

cat_op1_df <- df |> select(cat_op1_round1, cat_op1_round2)
cat_op2_df <- df |> select(cat_op2_round1, cat_op2_round2)
cat_inter_df <- df |> select(cat_op2_round1, cat_op1_round1)

icc_op1 <- icc(df_op1, model = "twoway", type = "agreement")
icc_op2 <- icc(df_op2, model = "twoway", type = "agreement")
icc_inter <- icc(df_inter, model = "twoway", type = "agreement")

kappa_op1 <- kappa2(cat_op1_df)
kappa_op2 <- kappa2(cat_op2_df)
kappa_inter <- kappa2(cat_inter_df)

recap_var_cont <- tibble(
  Parameter = c("ICC (Agreement)", "Lower CI (95%)", "Upper CI (95%)", "F-value", "p-value"),
  Recap_op1 = round(c(icc_op1$value, icc_op1$lbound, icc_op1$ubound, icc_op1$Fvalue, icc_op1$p.value), 3),
  Recap_op2 = round(c(icc_op2$value, icc_op2$lbound, icc_op2$ubound, icc_op2$Fvalue, icc_op2$p.value), 3),
  Recap_inter = round(c(icc_inter$value, icc_inter$lbound, icc_inter$ubound, icc_inter$Fvalue, icc_inter$p.value), 3)
)

recap_var_cat <- tibble(
  Parameter = c("Cohen's Kappa", "z-value", "p-value"),
  Recap_op1 = round(c(kappa_op1$value, kappa_op1$statistic, kappa_op1$p.value), 3),
  Recap_op2 = round(c(kappa_op2$value, kappa_op2$statistic, kappa_op2$p.value), 3),
  Recap_inter = round(c(kappa_inter$value, kappa_inter$statistic, kappa_inter$p.value), 3)
)


# --- 6. BLAND-ALTMAN PLOTS, RESIDUALS, AND NORMALITY TESTS ---

df_complete <- df |>
  mutate(
    ba_op1_mean = (rx_op1_round1 + rx_op1_round2) / 2,
    ba_op1_diff = rx_op1_round2 - rx_op1_round1,
    ba_op2_mean = (rx_op2_round1 + rx_op2_round2) / 2,
    ba_op2_diff = rx_op2_round2 - rx_op2_round1,
    ba_inter_mean = (rx_op2_round1 + rx_op1_round1) / 2,
    ba_inter_diff = rx_op2_round1 - rx_op1_round1
  )

ba_op1_bias <- mean(df_complete$ba_op1_diff)
ba_op1_sd_diff <- sd(df_complete$ba_op1_diff)
ba_op1_loa_sup <- ba_op1_bias + (1.96 * ba_op1_sd_diff)
ba_op1_loa_inf <- ba_op1_bias - (1.96 * ba_op1_sd_diff)

ba_op2_bias <- mean(df_complete$ba_op2_diff)
ba_op2_sd_diff <- sd(df_complete$ba_op2_diff)
ba_op2_loa_sup <- ba_op2_bias + (1.96 * ba_op2_sd_diff)
ba_op2_loa_inf <- ba_op2_bias - (1.96 * ba_op2_sd_diff)

ba_inter_bias <- mean(df_complete$ba_inter_diff)
ba_inter_sd_diff <- sd(df_complete$ba_inter_diff)
ba_inter_loa_sup <- ba_inter_bias + (1.96 * ba_inter_sd_diff)
ba_inter_loa_inf <- ba_inter_bias - (1.96 * ba_inter_sd_diff)

recap_ba <- tibble(
  Analysis = c("Intra-operator 1", "Intra-operator 2", "Inter-rater (Op. 2 vs Op. 1)"),
  Bias = c(ba_op1_bias, ba_op2_bias, ba_inter_bias),
  SD_Diff = c(ba_op1_sd_diff, ba_op2_sd_diff, ba_inter_sd_diff),
  LoA_Inf = c(ba_op1_loa_inf, ba_op2_loa_inf, ba_inter_loa_inf),
  LoA_Sup = c(ba_op1_loa_sup, ba_op2_loa_sup, ba_inter_loa_sup)
)

sw_op1 <- shapiro.test(df_complete$ba_op1_diff)
sw_op2 <- shapiro.test(df_complete$ba_op2_diff)
sw_inter <- shapiro.test(df_complete$ba_inter_diff)

recap_normality <- tibble(
  Analysis = c("Intra-operator 1", "Intra-operator 2", "Inter-rater (Op. 2 vs Op. 1)"),
  W_Statistic = round(c(sw_op1$statistic, sw_op2$statistic, sw_inter$statistic), 3),
  p_value = round(c(sw_op1$p.value, sw_op2$p.value, sw_inter$p.value), 3)
)

df_ba_long <- bind_rows(
  df_complete |> transmute(mean = ba_inter_mean, diff = ba_inter_diff, Analysis = "Inter-rater (Op2 vs Op1)"),
  df_complete |> transmute(mean = ba_op1_mean, diff = ba_op1_diff, Analysis = "Intra-rater (Op1)"),
  df_complete |> transmute(mean = ba_op2_mean, diff = ba_op2_diff, Analysis = "Intra-rater (Op2)")
) |> filter(Analysis != "Inter-rater (Op2 vs Op1)")

theme_ba <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "italic", hjust = 0.5),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black")
  )

plot_ba_inter <- ggplot(df_complete, aes(x = ba_inter_mean, y = ba_inter_diff)) +
  geom_point(alpha = 0.6, size = 5, color = "black", stroke = 1.5) +
  geom_hline(yintercept = ba_inter_bias, color = "red", linetype = "solid", linewidth = 1.25) +
  geom_hline(yintercept = ba_inter_loa_sup, color = "blue", linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = ba_inter_loa_inf, color = "blue", linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = max(df_complete$ba_inter_mean), y = ba_inter_bias, label = round(ba_inter_bias, 2), vjust = -0.5, hjust = 1, color = "red", fontface = "bold") +
  annotate("text", x = max(df_complete$ba_inter_mean), y = ba_inter_loa_sup, label = round(ba_inter_loa_sup, 2), vjust = -0.5, hjust = 1, color = "blue", fontface = "bold") +
  annotate("text", x = max(df_complete$ba_inter_mean), y = ba_inter_loa_inf, label = round(ba_inter_loa_inf, 2), vjust = 1.5, hjust = 1, color = "blue", fontface = "bold") +
  labs(
    title = "Bland-Altman Plot | Operator 2 vs Operator 1",
    x = "Mean",
    y = "Difference"
  ) +
  theme_ba

plot_ba_op1 <- ggplot(df_complete, aes(x = ba_op1_mean, y = ba_op1_diff)) +
  geom_point(alpha = 0.6, size = 5, color = "black", stroke = 1.5) +
  geom_hline(yintercept = ba_op1_bias, color = "red", linetype = "solid", linewidth = 1.25) +
  geom_hline(yintercept = ba_op1_loa_sup, color = "blue", linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = ba_op1_loa_inf, color = "blue", linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = max(df_complete$ba_op1_mean), y = ba_op1_bias, label = round(ba_op1_bias, 2), vjust = -0.5, hjust = 1, color = "red", fontface = "bold") +
  annotate("text", x = max(df_complete$ba_op1_mean), y = ba_op1_loa_sup, label = round(ba_op1_loa_sup, 2), vjust = -0.5, hjust = 1, color = "blue", fontface = "bold") +
  annotate("text", x = max(df_complete$ba_op1_mean), y = ba_op1_loa_inf, label = round(ba_op1_loa_inf, 2), vjust = 1.5, hjust = 1, color = "blue", fontface = "bold") +
  labs(
    title = "Bland-Altman Plot | Operator 1",
    x = "Mean",
    y = "Difference"
  ) +
  theme_ba

plot_ba_op2 <- ggplot(df_complete, aes(x = ba_op2_mean, y = ba_op2_diff)) +
  geom_point(alpha = 0.6, size = 5, color = "black", stroke = 1.5) +
  geom_hline(yintercept = ba_op2_bias, color = "red", linetype = "solid", linewidth = 1.25) +
  geom_hline(yintercept = ba_op2_loa_sup, color = "blue", linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = ba_op2_loa_inf, color = "blue", linetype = "dashed", linewidth = 0.75) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  annotate("text", x = max(df_complete$ba_op2_mean), y = ba_op2_bias, label = round(ba_op2_bias, 2), vjust = -0.5, hjust = 1, color = "red", fontface = "bold") +
  annotate("text", x = max(df_complete$ba_op2_mean), y = ba_op2_loa_sup, label = round(ba_op2_loa_sup, 2), vjust = -0.5, hjust = 1, color = "blue", fontface = "bold") +
  annotate("text", x = max(df_complete$ba_op2_mean), y = ba_op2_loa_inf, label = round(ba_op2_loa_inf, 2), vjust = 1.5, hjust = 1, color = "blue", fontface = "bold") +
  labs(
    title = "Bland-Altman Plot | Operator 2",
    x = "Mean",
    y = "Difference"
  ) +
  theme_ba

df_bias <- df_ba_long %>%
  group_by(Analysis) %>%
  summarise(
    bias = mean(diff),
    loa_inf = bias - 1.96 * sd(diff),
    loa_sup = bias + 1.96 * sd(diff)
  )

plot_ba_combined <- ggplot(df_ba_long, aes(x = mean, y = diff, color = Analysis)) +
  geom_point(alpha = 0.6, size = 5, stroke = 1.5) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1.25) +
  geom_hline(data = df_bias, aes(yintercept = bias, color = Analysis), linetype = "solid", linewidth = 1.25) +
  geom_hline(data = df_bias, aes(yintercept = loa_inf), linetype = "dashed", linewidth = 0.75) +
  geom_hline(data = df_bias, aes(yintercept = loa_sup), linetype = "dashed", linewidth = 0.75) +
  facet_wrap(~ Analysis, scales = "free_x") +
  scale_color_manual(values = c(
    "Intra-rater (Op1)" = "red", 
    "Intra-rater (Op2)" = "green4"
  )) +
  theme_minimal(base_size = 18) +
  labs(
    title = "Bland-Altman Plot | Operator 1 vs Operator 2",
    x = "Mean",
    y = "Difference"
  ) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    axis.text.x = element_text(face = "bold", color = "black"),
    axis.text.y = element_text(face = "bold", color = "black"),
    strip.text = element_text(size = 18, face = "bold", color = "black")
  )

print(plot_ba_inter)
print(plot_ba_op1)
print(plot_ba_op2)
print(plot_ba_combined)


# --- 7. HEATMAPS OF CONFUSION MATRICES ---

conf_matrix_op1 <- table(df$cat_op1_round1, df$cat_op1_round2)
conf_matrix_op2 <- table(df$cat_op2_round1, df$cat_op2_round2)
conf_matrix_inter <- table(df$cat_op1_round1, df$cat_op2_round1)

conf_df_op1 <- as.data.frame(conf_matrix_op1)
conf_df_op2 <- as.data.frame(conf_matrix_op2)
conf_df_inter <- as.data.frame(conf_matrix_inter)

colnames(conf_df_op1) <- c("op1_1", "op1_2", "Frequency")
colnames(conf_df_op2) <- c("op2_1", "op2_2", "Frequency")
colnames(conf_df_inter) <- c("op1_1", "op2_1", "Frequency")

theme_heat <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, face = "italic", hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(face = "bold", color = "black", angle = -90, hjust = 0),
    axis.text.y = element_text(face = "bold", color = "black")
  )

plot_heat_op1 <- ggplot(conf_df_op1, aes(x = op1_1, y = op1_2, fill = Frequency)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequency), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "red", high = "green4") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Confusion Matrix | Operator 1",
    subtitle = paste("Cohen's k =", round(as.numeric(recap_var_cat$Recap_op1[1]), 3)),
    x = "",
    y = "",
    fill = "Number of cases"
  ) +
  theme_heat

plot_heat_op2 <- ggplot(conf_df_op2, aes(x = op2_1, y = op2_2, fill = Frequency)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequency), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "red", high = "green4") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Confusion Matrix | Operator 2",
    subtitle = paste("Cohen's k =", round(as.numeric(recap_var_cat$Recap_op2[1]), 3)),
    x = "",
    y = "",
    fill = "Number of cases"
  ) +
  theme_heat

plot_heat_inter <- ggplot(conf_df_inter, aes(x = op1_1, y = op2_1, fill = Frequency)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequency), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "red", high = "green4") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Confusion Matrix | Inter-rater",
    subtitle = paste("Cohen's k =", round(as.numeric(recap_var_cat$Recap_op1[1]), 3)),
    x = "",
    y = "",
    fill = "Number of cases"
  ) +
  theme_heat

print(plot_heat_op1)
print(plot_heat_op2)
print(plot_heat_inter)


# --- 8. EXPORT TABLES AND PLOTS FOR QUARTO/REPORTS ---

list_tabs <- c(
  "df_complete",
  "conf_matrix_inter",
  "conf_matrix_op1",
  "conf_matrix_op2",
  "recap_normality",
  "recap_var_cont",
  "recap_ba",
  "recap_var_cat"
)

list_plots <- c(
  "plot_ba_combined",
  "plot_ba_inter",
  "plot_ba_op1",
  "plot_ba_op2",
  "plot_heat_inter",
  "plot_heat_op1",
  "plot_heat_op2"
)

# Export tables as RDS files
for (table_name in list_tabs) {
  current_table <- get(table_name)
  table_path <- file.path("export", paste0(table_name, ".rds"))
  saveRDS(current_table, file = table_path)
}

# Export tables as XLSX file (multiple sheets)
write.xlsx(
  setNames(lapply(list_tabs, get), list_tabs),
  file = "export/FINAL_ANALYSIS.xlsx"
)

# Export ggplot graphs as SVG and PDF
for (plot_name in list_plots) {
  current_plot <- get(plot_name)
  
  # Export as SVG
  plot_path_svg <- file.path("export", paste0(plot_name, ".svg"))
  ggsave(
    filename = plot_path_svg,
    plot = current_plot,
    width = 8,
    height = 6,
    units = "in"
  )
  
  # Export as PDF
  plot_path_pdf <- file.path("export", paste0(plot_name, ".pdf"))
  ggsave(
    filename = plot_path_pdf,
    plot = current_plot,
    width = 8,
    height = 6,
    units = "in"
  )
}

message("All tables have been successfully exported to the 'export' folder")
message("All plots have been successfully exported to the 'export' folder")

quarto::quarto_render("report.qmd")
utils::browseURL("report.pdf")