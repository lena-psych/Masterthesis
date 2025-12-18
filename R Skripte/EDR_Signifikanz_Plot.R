library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)


# Daten einlesen
options(scipen = 999)
df <- read_excel("Tabelle_Rohdaten.xlsx")

# Spaltennamen prüfen 
names(df)


# Variable für Signifikanz des Originaleffekts
df <- df |>
  mutate(
    orig_sig = ifelse(`p value` < .05, "significant", "not significant"),
    orig_sig = factor(orig_sig, levels = c("significant", "not significant"))
  )




# Boxplots pro Modell, eingefärbt nach Signifikanz

ggplot(df, aes(x = Model, y = EDR, fill = orig_sig)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(
    values = c(
      "significant" = "#A8D5BA",      # z. B. Blau
      "not significant" = "#F4B6A6"   # z. B. Rot
    )
  ) +
  labs(
    x = "Model",
    y = "Empirical Detection Rate (EDR)",
    fill = "Original Effect",
    title = "EDR for each statistical model",
    subtitle = "Colored by significance of the original study effect (p < .05)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


### Mittelwert aufgetielt nach signifkanten und nicht signfikinaten Originaleffekte

df %>%
  group_by(orig_sig) %>%
  summarise(
    median_EDR = median(EDR, na.rm = TRUE),
    sd_EDR   = sd(EDR, na.rm = TRUE),
    n        = n()
  )

### Mittelwert der EDR aufgeteilt nach adjustierten und unadjustierten Modellen
adjusted_models <- c("SEM (Baseline)", "LMM (Baseline)", "T-Test (Change Scores)")

df %>%
  mutate(
    adjusted = ifelse(Model %in% adjusted_models, "adjusted", "unadjusted")
  ) %>%
  group_by(adjusted) %>%
  summarise(
    mean_EDR = mean(EDR, na.rm = TRUE),
    sd_EDR   = sd(EDR, na.rm = TRUE),
    n        = n()
  )


### Boxplot für Coverage

# Reihenfolge der Messzeitpunkte festlegen
df <- df |>
  mutate(`Time Point` = factor(`Time Point`, levels = c("Post", "FU")))

boxplot_coverage <- ggplot(df, aes(x = Model, y = Coverage, fill = `Time Point`)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  scale_y_continuous(limits = c(0.85, 1)) +
  scale_fill_manual(
    values = c(
      "Post" = "#FDB863",
      "FU"   = "#B2DFEE"
    ),
    drop = FALSE
  ) +
  labs(
    x = "Model",
    y = "Coverage",
    fill = "Time point",
    title = "Coverage"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



### Boxplot für RMSE

boxplot_rmse <- ggplot(df, aes(x = Model, y = RMSE, fill = `Time Point`)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(
    values = c(
      "Post" = "#FDB863",
      "FU"   = "#B2DFEE"
    ),
    drop = FALSE
    ) +
  labs(
    x = "Model",
    y = "RMSE",
    fill = "Time point",
    title = "RMSE"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

### Boxplot für Bias

boxplot_bias <- ggplot(df, aes(x = Model, y = Bias, fill = `Time Point`)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(
    values = c(
      "Post" = "#FDB863",
      "FU"   = "#B2DFEE"
    ),
    drop = FALSE
  ) +
  labs(x = "Model", y = "Bias", fill = "Time point", title = "Bias") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### patchwork für Boxplots (Coverage, RMSE, Bias)
(boxplot_coverage | boxplot_rmse) /
  (boxplot_bias     | plot_spacer()) +
  plot_annotation(
    title = "Boxplots for Coverage, RMSE and Bias separated by time point (Post vs. Follow-up)"
  )

