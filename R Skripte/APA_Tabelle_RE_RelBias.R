library(flextable)
library(officer)
library(dplyr)

tab <- df_fu   # ist schon FU

ft <- flextable(tab)
ft <- theme_apa(ft)

# kompakter Stil
ft <- fontsize(ft, size = 9, part = "all")
ft <- padding(ft, padding = 2, part = "all")

# Spaltenbreiten anpassen
ft <- width(ft, j = "Study", width = 1.2)
ft <- width(ft, j = "Model", width = 1.0)
ft <- width(ft, j = "Original Effect", width = 1.0)
ft <- width(ft, j = "RelBias (%)",        width = 1.0)

ft <- width(ft, j = "Bias",           width = 0.7)
ft <- width(ft, j = "Rmse",           width = 0.7)
ft <- width(ft, j = "RE(%)",             width = 0.7)



# auf Seitenbreite fitten
ft <- fit_to_width(ft, max_width = 6.5)

# Caption
ft <- set_caption(ft, "Table 2\nOriginal Effect, Relative Bias and Relative Efficiency for every Study at Post Time Point")

# Export nach Word
doc <- read_docx()
doc <- body_add_flextable(doc, value = ft)
print(doc, target = "Table2_RE_fu_3.docx")

ft
