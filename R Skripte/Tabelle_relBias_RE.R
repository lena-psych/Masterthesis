# Pakete
library(readxl)
library(dplyr)
library(readr)
library(stringr)

# Excel einlesen
df <- read_excel("Tabelle_Rohdaten.xlsx")


# Referenz-RMSE (T-Test) pro Study x Time Point holen
ref <- df %>%
  filter(Model == "T-Test") %>%
  select(Study, `Time Point`, RMSE_ttest = RMSE)

# Join + RE% berechnen
df_out <- df %>%
  left_join(ref, by = c("Study", "Time Point")) %>%
  mutate(
    RE_percent = (RMSE / RMSE_ttest) * 100
  )

# RelBias in %

library(dplyr)

df_out<- df_out %>%
  mutate(
    RelBias_percent = round((Bias / abs(`Original Effect`)) * 100,2)
  )


# Datensatz anpassen für APA

df_neu <- df_out %>%
  select(Study, Model, `Time Point`, `Original Effect`, `RelBias_percent`, RE_percent) %>%
  mutate(`Original Effect` = round(`Original Effect`, 2)) %>%
  rename(`RE(%)` = RE_percent,
         `RelBias (%)` = RelBias_percent)

df_neu_2 <- df_out %>%
  select(Study, Model, `Time Point`, `Original Effect`, Bias, RMSE, `RelBias_percent`, RE_percent) %>%
  mutate(`Original Effect` = round(`Original Effect`, 2), Bias = round (Bias, 2), RMSE = round (RMSE,2)) %>%
  rename(`RE(%)` = RE_percent,
         `RelBias (%)` = RelBias_percent)

df_post <- df_neu_2 %>%
  filter(`Time Point` == "Post")

df_fu <- df_neu_2 %>%
  filter(`Time Point` == "FU")

# Median RelBias

library(dplyr)

df_neu_2 %>%
  group_by(Model) %>%
  summarise(
    median_RelBias = median(`RelBias (%)`, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(median_RelBias)



# Speichern
library(writexl)
write_xlsx(df_neu_2, "Tabelle_Bias_RMSE_RelBias_RE.xlsx")
