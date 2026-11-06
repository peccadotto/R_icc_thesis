library(readxl)
library(dplyr)
library(janitor)
library(irr)
library(ggplot2)

# Importazione e normalizzazione automatica degli header (tutto minuscolo, underscore)
{
  dati_rx <- read_excel("dati.xlsx") |> clean_names() |>
    mutate(
      
      # --- Junior Round 1 ---
      esito_junior_round1 = case_when(
        op_junior_round1 < 30                    ~ "Stabile (< 30%)",
        op_junior_round1 >= 30 & op_junior_round1 < 40 ~ "Zona d'allarme (30-39%)",
        op_junior_round1 >= 40 & op_junior_round1 < 50 ~ "Chirurgia preventiva (40-49%)",
        op_junior_round1 >= 50 & op_junior_round1 < 90 ~ "Chirurgia ricostruttiva (50-89%)",
        op_junior_round1 >= 90                   ~ "Chirurgia di salvataggio (>= 90%)",
        TRUE                               ~ NA_character_
      ),
      esito_junior_round1 = factor(
        esito_junior_round1,
        levels = c(
          "Stabile (< 30%)",
          "Zona d'allarme (30-39%)",
          "Chirurgia preventiva (40-49%)",
          "Chirurgia ricostruttiva (50-89%)",
          "Chirurgia di salvataggio (>= 90%)"
        )
      ),
      
      # --- Junior Round 2 ---
      esito_junior_round2 = case_when(
        op_junior_round2 < 30                    ~ "Stabile (< 30%)",
        op_junior_round2 >= 30 & op_junior_round2 < 40 ~ "Zona d'allarme (30-39%)",
        op_junior_round2 >= 40 & op_junior_round2 < 50 ~ "Chirurgia preventiva (40-49%)",
        op_junior_round2 >= 50 & op_junior_round2 < 90 ~ "Chirurgia ricostruttiva (50-89%)",
        op_junior_round2 >= 90                   ~ "Chirurgia di salvataggio (>= 90%)",
        TRUE                               ~ NA_character_
      ),
      esito_junior_round2 = factor(
        esito_junior_round2,
        levels = c(
          "Stabile (< 30%)",
          "Zona d'allarme (30-39%)",
          "Chirurgia preventiva (40-49%)",
          "Chirurgia ricostruttiva (50-89%)",
          "Chirurgia di salvataggio (>= 90%)"
        )
      ),
      
      # --- Senior Round 1 ---
      esito_senior_round1 = case_when(
        op_senior_round1 < 30                    ~ "Stabile (< 30%)",
        op_senior_round1 >= 30 & op_senior_round1 < 40 ~ "Zona d'allarme (30-39%)",
        op_senior_round1 >= 40 & op_senior_round1 < 50 ~ "Chirurgia preventiva (40-49%)",
        op_senior_round1 >= 50 & op_senior_round1 < 90 ~ "Chirurgia ricostruttiva (50-89%)",
        op_senior_round1 >= 90                   ~ "Chirurgia di salvataggio (>= 90%)",
        TRUE                               ~ NA_character_
      ),
      esito_senior_round1 = factor(
        esito_senior_round1,
        levels = c(
          "Stabile (< 30%)",
          "Zona d'allarme (30-39%)",
          "Chirurgia preventiva (40-49%)",
          "Chirurgia ricostruttiva (50-89%)",
          "Chirurgia di salvataggio (>= 90%)"
        )
      ),
      
      # --- Senior Round 2 ---
      esito_senior_round2 = case_when(
        op_senior_round2 < 30                    ~ "Stabile (< 30%)",
        op_senior_round2 >= 30 & op_senior_round2 < 40 ~ "Zona d'allarme (30-39%)",
        op_senior_round2 >= 40 & op_senior_round2 < 50 ~ "Chirurgia preventiva (40-49%)",
        op_senior_round2 >= 50 & op_senior_round2 < 90 ~ "Chirurgia ricostruttiva (50-89%)",
        op_senior_round2 >= 90                   ~ "Chirurgia di salvataggio (>= 90%)",
        TRUE                               ~ NA_character_
      ),
      esito_senior_round2 = factor(
        esito_senior_round2,
        levels = c(
          "Stabile (< 30%)",
          "Zona d'allarme (30-39%)",
          "Chirurgia preventiva (40-49%)",
          "Chirurgia ricostruttiva (50-89%)",
          "Chirurgia di salvataggio (>= 90%)"
        )
      )
    )
}

# --- CONCORDANZA INTER-OPERATORE (Round 1) ---
{
# Selezioniamo solo le due colonne di interesse
dati_inter <- dati_rx %>%
  select(op_senior_round1, op_junior_round1)

# Calcolo ICC
icc_inter <- icc(dati_inter, model = "twoway", type = "agreement", unit = "single")
print(icc_inter)
}

# --- CONCORDANZA INTRA-OPERATORE (Senior) ---
{
  dati_intra <- dati_rx %>%
  select(op_senior_round1, op_senior_round2)

icc_intra <- icc(dati_intra, model = "twoway", type = "agreement", unit = "single")
print(icc_intra)
}
# --- GRAFICO BLAND-ALTMAN CONCORDANZA INTER-OPERATORE ---
{
# Preparazione dati per il plot

ba_dati <- dati_rx %>%
  mutate(
    media_mis = (op_senior_round1 + op_junior_round1) / 2,
    differenza = op_senior_round1 - op_junior_round1
  ) %>%
  filter(!is.na(differenza)) # Rimuove eventuali NA

# Calcolo dei parametri chiave
bias <- mean(ba_dati$differenza)
sd_diff <- sd(ba_dati$differenza)
loa_sup <- bias + (1.96 * sd_diff)
loa_inf <- bias - (1.96 * sd_diff)

# Creazione del grafico
ggplot(ba_dati, aes(x = media_mis, y = differenza)) +
  geom_point(alpha = 0.6, size = 2.5, color = "#2C3E50") +
  # Linea del Bias (Errore medio sistematico)
  geom_hline(yintercept = bias, color = "#C0392B", linetype = "solid", linewidth = 1) +
  # Limiti di Confidenza (LoA) al 95%
  geom_hline(yintercept = loa_sup, color = "#2980B9", linetype = "dashed", linewidth = 0.8) +
  geom_hline(yintercept = loa_inf, color = "#2980B9", linetype = "dashed", linewidth = 0.8) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Bland-Altman Plot: Concordanza Inter-operatore",
    subtitle = paste("Bias:", round(bias, 2), "| LoA: [", round(loa_inf, 2), ",", round(loa_sup, 2), "]"),
    x = "Media delle misurazioni (Gradi/mm)",
    y = "Differenza (Senior - Junior)"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
}

# --- GRAFICO BLAND-ALTMAN CONCORDANZA INTRA-OPERATORE senior ---
{
# Preparazione dati per il plot

  ba_dati <- dati_rx %>%
    mutate(
      media_mis = (op_senior_round2 + op_senior_round1) / 2,
      differenza = op_senior_round1 - op_senior_round2
    ) %>%
    filter(!is.na(differenza)) # Rimuove eventuali NA
  
  # Calcolo dei parametri chiave
  bias <- mean(ba_dati$differenza)
  sd_diff <- sd(ba_dati$differenza)
  loa_sup <- bias + (1.96 * sd_diff)
  loa_inf <- bias - (1.96 * sd_diff)
  
  # Creazione del grafico
  ggplot(ba_dati, aes(x = media_mis, y = differenza)) +
    geom_point(alpha = 0.6, size = 2.5, color = "#2C3E50") +
    # Linea del Bias (Errore medio sistematico)
    geom_hline(yintercept = bias, color = "#C0392B", linetype = "solid", linewidth = 1) +
    # Limiti di Confidenza (LoA) al 95%
    geom_hline(yintercept = loa_sup, color = "#2980B9", linetype = "dashed", linewidth = 0.8) +
    geom_hline(yintercept = loa_inf, color = "#2980B9", linetype = "dashed", linewidth = 0.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Bland-Altman Plot: Concordanza Intra-operatore (senior)",
      subtitle = paste("Bias:", round(bias, 2), "| LoA: [", round(loa_inf, 2), ",", round(loa_sup, 2), "]"),
      x = "Media delle misurazioni (Gradi/mm)",
      y = "Differenza (Senior - Junior)"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# --- GRAFICO BLAND-ALTMAN CONCORDANZA INTRA-OPERATORE senior ---
{
# Preparazione dati per il plot

  ba_dati <- dati_rx %>%
    mutate(
      media_mis = (op_junior_round2 + op_junior_round1) / 2,
      differenza = op_junior_round1 - op_junior_round2
    ) %>%
    filter(!is.na(differenza)) # Rimuove eventuali NA
  
  # Calcolo dei parametri chiave
  bias <- mean(ba_dati$differenza)
  sd_diff <- sd(ba_dati$differenza)
  loa_sup <- bias + (1.96 * sd_diff)
  loa_inf <- bias - (1.96 * sd_diff)
  
  # Creazione del grafico
  ggplot(ba_dati, aes(x = media_mis, y = differenza)) +
    geom_point(alpha = 0.6, size = 2.5, color = "#2C3E50") +
    # Linea del Bias (Errore medio sistematico)
    geom_hline(yintercept = bias, color = "#C0392B", linetype = "solid", linewidth = 1) +
    # Limiti di Confidenza (LoA) al 95%
    geom_hline(yintercept = loa_sup, color = "#2980B9", linetype = "dashed", linewidth = 0.8) +
    geom_hline(yintercept = loa_inf, color = "#2980B9", linetype = "dashed", linewidth = 0.8) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Bland-Altman Plot: Concordanza Intra-operatore (junior)",
      subtitle = paste("Bias:", round(bias, 2), "| LoA: [", round(loa_inf, 2), ",", round(loa_sup, 2), "]"),
      x = "Media delle misurazioni (Gradi/mm)",
      y = "Differenza (Senior - Junior)"
    ) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}


# --- CONCORDANZA CATEGORIALE: KAPPA DI COHEN (Inter-operatore Round 1) ---
{
# 1. Selezione e conversione in factor con livelli identici
esito_inter_df <- dati_rx %>%
  select(esito_senior_round1, esito_junior_round1) %>%
  mutate(across(everything(), as.factor))

# 2. Calcolo statistico del Kappa di Cohen
kappa_inter <- kappa2(esito_inter_df, weight = "unweighted")
print(kappa_inter)


# --- GRAFICO: HEATMAP DELLA MATRICE DI CONFUSIONE  ---

# 1. Creazione della tabella di contingenza trasformata in dataframe per ggplot2
conf_matrix <- table(dati_rx$esito_senior_round1, dati_rx$esito_junior_round1)
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Senior", "Junior", "Frequenza")

# 2. Plot della Heatmap
ggplot(conf_df, aes(x = Junior, y = Senior, fill = Frequenza)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequenza), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1B4F72") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Matrice di Confusione: Accordo Categoriale Inter-operatore",
    subtitle = paste("Kappa di Cohen =", round(kappa_inter$value, 3), 
                     "| p =", format.pval(kappa_inter$p.value, digits = 3)),
    x = "Operatore Junior (Round 1)",
    y = "Operatore Senior (Round 1)",
    fill = "N. Casi"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )
}


# --- CONCORDANZA CATEGORIALE: KAPPA DI COHEN (Intra-operatore junior) ---
{
# 1. Selezione e conversione in factor con livelli identici
esito_inter_df <- dati_rx %>%
  select(esito_junior_round1, esito_junior_round2) %>%
  mutate(across(everything(), as.factor))

# 2. Calcolo statistico del Kappa di Cohen
kappa_inter <- kappa2(esito_inter_df, weight = "unweighted")
print(kappa_inter)


# --- GRAFICO: HEATMAP DELLA MATRICE DI CONFUSIONE  ---

# 1. Creazione della tabella di contingenza trasformata in dataframe per ggplot2
conf_matrix <- table(dati_rx$esito_junior_round1, dati_rx$esito_junior_round2)
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Junior1", "Junior2", "Frequenza")

# 2. Plot della Heatmap
ggplot(conf_df, aes(x = Junior1, y = Junior2, fill = Frequenza)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequenza), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1B4F72") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Matrice di Confusione: Accordo Categoriale Inter-operatore",
    subtitle = paste("Kappa di Cohen =", round(kappa_inter$value, 3), 
                     "| p =", format.pval(kappa_inter$p.value, digits = 3)),
    x = "Operatore Junior (Round 1)",
    y = "Operatore Senior (Round 1)",
    fill = "N. Casi"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )
}

# --- CONCORDANZA CATEGORIALE: KAPPA DI COHEN (Intra-operatore senior) ---
{
# 1. Selezione e conversione in factor con livelli identici
esito_inter_df <- dati_rx %>%
  select(esito_senior_round1, esito_senior_round2) %>%
  mutate(across(everything(), as.factor))

# 2. Calcolo statistico del Kappa di Cohen
kappa_inter <- kappa2(esito_inter_df, weight = "unweighted")
print(kappa_inter)


# --- GRAFICO: HEATMAP DELLA MATRICE DI CONFUSIONE  ---

# 1. Creazione della tabella di contingenza trasformata in dataframe per ggplot2
conf_matrix <- table(dati_rx$esito_senior_round1, dati_rx$esito_senior_round2)
conf_df <- as.data.frame(conf_matrix)
colnames(conf_df) <- c("Senior1", "Senior2", "Frequenza")

# 2. Plot della Heatmap
ggplot(conf_df, aes(x = Senior1, y = Senior2, fill = Frequenza)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Frequenza), size = 6, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#AED6F1", high = "#1B4F72") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Matrice di Confusione: Accordo Categoriale Inter-operatore",
    subtitle = paste("Kappa di Cohen =", round(kappa_inter$value, 3), 
                     "| p =", format.pval(kappa_inter$p.value, digits = 3)),
    x = "Operatore Junior (Round 1)",
    y = "Operatore Senior (Round 1)",
    fill = "N. Casi"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )
}