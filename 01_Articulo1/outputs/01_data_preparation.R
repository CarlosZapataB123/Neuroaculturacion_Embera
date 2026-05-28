# ============================================================================
# ARTICULO 1 - Embera Chami Neuropsychological Profile
# PASO 1: PREPARACION DE DATOS
# Autor: Carlos E. Zapata Bohorquez
# Co-investigador metodologico: Claude (Anthropic)
# Fecha: 2026
# R version: >= 4.3
# ============================================================================
# OBJETIVO
# Cargar la base original, crear compuestos, invertir indicadores de
# direccion negativa, computar indice de dominancia linguistica L1/L2,
# computar puntaje de aculturacion, y generar el dataset analitico final.
# NO se ejecutan descriptivos ni inferencia en este paso.
# ============================================================================

# ----------------------------------------------------------------------------
# 0. SETUP
# ----------------------------------------------------------------------------

# Limpiar entorno (opcional, comentar si trabajas en sesion compartida)
# rm(list = ls())

# Paquetes necesarios. Se instalan si no estan presentes.
.pkgs <- c("readxl", "dplyr", "tidyr", "stringr", "purrr")
.missing <- setdiff(.pkgs, rownames(installed.packages()))
if (length(.missing)) install.packages(.missing, dependencies = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
})

# Semilla para reproducibilidad (no critica en Paso 1, util en pasos siguientes)
set.seed(20260101)

# Reportar version de R y sesion (para Methods del paper)
cat("R version:", R.version.string, "\n")
cat("Fecha de ejecucion:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n")

# ----------------------------------------------------------------------------
# 1. CARGA DE DATOS
# ----------------------------------------------------------------------------

# Ruta al archivo. Ajustar segun ubicacion local.
path_xlsx <- "Database_indígenas_colombia_ORIGIN.xlsx"

# Lectura. Forzamos lectura como character primero para inspeccion,
# pero luego usamos read_excel con na = c("", "NA") porque las S_I_
# tienen "NA" como CADENA, no como celda vacia.
df_raw <- read_excel(
  path = path_xlsx,
  sheet = "Total Bateria",
  na    = c("", "NA", "Na", "na", "NaN", "nan", "."),
  guess_max = 200
)

# Las ultimas filas estan vacias (109 filas en total, 88 con datos).
# Filtramos por ID no NA. Esto se reportara en flujo STROBE.
df_raw <- df_raw %>% filter(!is.na(ID))

stopifnot("La base no tiene los 88 casos esperados" = nrow(df_raw) == 88)
cat("Filas con datos (ID no NA):", nrow(df_raw), "\n")
cat("Columnas totales en archivo original:", ncol(df_raw), "\n\n")

# ----------------------------------------------------------------------------
# 2. RENOMBRADO ANALITICO
# ----------------------------------------------------------------------------
# Pasamos las columnas clave a snake_case sin tildes para que el resto del
# pipeline (psych, lm, ggplot) no requiera comillas invertidas. Mantenemos
# AC_* tal cual porque ya son limpias. Los nombres ESPANOLES originales
# se conservan en df_raw para auditoria.

df <- df_raw %>%
  rename(
    # Sociodemograficos clave
    edad      = `Edad`,
    genero    = `Género`,
    educacion = `Años de educacion`,
    bilingue  = `BILINGÜE`,
    # Competencia linguistica - primer idioma (P_I = Embera)
    pi_expresion   = `Comp_Linguistica_P_I_Expresión`,
    pi_comprension = `Comp_Linguistica_P_I_Comprensión`,
    pi_lectura     = `Comp_Linguistica_P_I_Lectura`,
    pi_escritura   = `Comp_Linguistica_P_I_Escritura`,
    pi_acento      = `Comp_Linguistica_P_I_Acento`,
    # Competencia linguistica - segundo idioma (S_I = Español)
    si_expresion   = `Comp_Linguistica_S_I_Expresión`,
    si_comprension = `Comp_Linguistica_S_I_Comprensión`,
    si_lectura     = `Comp_Linguistica_S_I_Lectura`,
    si_escritura   = `Comp_Linguistica_S_I_Escritura`,
    si_acento      = `Comp_Linguistica_S_I_Acento`,
    # Cribados
    mmse   = MMSE,
    phq9   = `PHQ-9`,
    bartel = Bartel,
    # Bateria neuropsicologica - Figura de Rey
    rey_copia    = `Figura de rey puntuación copia`,
    rey_memoria  = `Figura de rey puntuación memoria inmediata`,
    # Fluidez
    flu_fon_M       = `Fluidez FyS verbal M`,
    flu_fon_R       = `Fluidez FyS verbal R`,
    flu_fon_P       = `Fluidez FyS verbal P`,
    flu_sem_animales = `Fluidez FyS semantica animales`,
    flu_sem_frutas   = `Fluidez FyS semantica frutas`,
    # HVLT-R
    hvlt_e1         = `HLVRT-R ensayo 1`,
    hvlt_e2         = `HLVRT-R ensayo 2`,
    hvlt_e3         = `HLVRT-R ensayo 3`,
    hvlt_total      = `HLVRT-R Total recuerdo inmediato`,
    hvlt_tardia     = `HLVRT-R evocación tardía`,
    hvlt_recon      = `HLVRT-R Reconocimiento total correctas`,
    hvlt_fp_rel     = `HLVRT-R Falsos positivos semanticamente relacionados`,
    hvlt_fp_norel   = `HLVRT-R Falsos positivos no relacionados semanticamente`,
    hvlt_errores    = `HLVRT-R Número total errores`,
    # Stroop
    stroop_p       = `STROOP P`,
    stroop_c       = `STROOP C`,
    stroop_pc      = `STROOP PC`,
    stroop_pc_calc = `STROOP pc Calculado`,
    stroop_interf  = `STROOP Interferencia`,
    # M-WCST
    wcst_cat       = `M-WCST categorías correctas`,
    wcst_persev    = `M-WCST perseveraciones`,
    wcst_err       = `M-WCST errores`,
    wcst_err_total = `M-WCST total errores`,
    # TMT
    tmt_a = `TMT A tiempo (sg)`,
    tmt_b = `TMT B tiempo (sg)`,
    # BTA
    bta_n     = `BTA-N`,
    bta_l     = `BTA-L`,
    bta_total = `BTA Total`,
    # SDMT
    sdmt_correct = `SDMT Aciertos`,
    sdmt_err     = `SDMT Errores`
  )

# ----------------------------------------------------------------------------
# 3. TIPIFICACION DE VARIABLES
# ----------------------------------------------------------------------------
# Genero como factor con etiquetas explicitas. Educacion y edad numericas.

df <- df %>%
  mutate(
    genero    = factor(genero, levels = c("H", "M"),
                       labels = c("Male", "Female")),
    edad      = as.numeric(edad),
    educacion = as.numeric(educacion)
  )

# Verificacion rapida de tipos
stopifnot(
  "genero no es factor con 2 niveles" = is.factor(df$genero) && nlevels(df$genero) == 2,
  "edad no es numerico"               = is.numeric(df$edad),
  "educacion no es numerico"          = is.numeric(df$educacion)
)

# ----------------------------------------------------------------------------
# 4. COMPUESTOS NEUROPSICOLOGICOS
# ----------------------------------------------------------------------------

df <- df %>%
  mutate(
    # Fluidez fonologica total (M + R + P)
    flu_fon_total = flu_fon_M + flu_fon_R + flu_fon_P,
    # Fluidez semantica total (animales + frutas)
    flu_sem_total = flu_sem_animales + flu_sem_frutas
  )

# ----------------------------------------------------------------------------
# 5. CHEQUEO DEFENSIVO DE REDUNDANCIAS ESTRUCTURALES
# ----------------------------------------------------------------------------
# Confirmamos las redundancias documentadas en hallazgos exploratorios:
#   hvlt_total  = hvlt_e1 + hvlt_e2 + hvlt_e3
#   bta_total   = bta_n + bta_l
#   wcst_err_total = wcst_persev + wcst_err

.diag <- df %>%
  summarise(
    hvlt_ok = all(abs((hvlt_e1 + hvlt_e2 + hvlt_e3) - hvlt_total) < 1e-6,
                  na.rm = TRUE),
    bta_ok  = all(abs((bta_n + bta_l) - bta_total) < 1e-6, na.rm = TRUE),
    wcst_ok = all(abs((wcst_persev + wcst_err) - wcst_err_total) < 1e-6,
                  na.rm = TRUE)
  )

cat("Chequeo de redundancias estructurales:\n")
print(.diag)
cat("\n")
stopifnot(
  "HVLT total != suma ensayos"        = isTRUE(.diag$hvlt_ok),
  "BTA total != N + L"                = isTRUE(.diag$bta_ok),
  "WCST err_total != persev + err"    = isTRUE(.diag$wcst_ok)
)

# ----------------------------------------------------------------------------
# 6. INVERSION DE INDICADORES DE DIRECCION NEGATIVA
# ----------------------------------------------------------------------------
# TMT y errores: a menor tiempo / menor error, mejor rendimiento.
# Invertimos signo para que TODOS los indicadores primarios cumplan
# "mayor = mejor", facilitando interpretacion uniforme en correlaciones,
# regresiones y forest plots.
# Nota: la inversion preserva la escala (no z-estandariza); el signo solo
# se voltea para alinear direccion.

df <- df %>%
  mutate(
    tmt_a_inv      = -1 * tmt_a,
    tmt_b_inv      = -1 * tmt_b,
    wcst_persev_inv = -1 * wcst_persev,  # disponible para sensibilidad
    wcst_err_inv    = -1 * wcst_err      # disponible para sensibilidad
  )

# ----------------------------------------------------------------------------
# 7. PUNTAJE DE ACULTURACION
# ----------------------------------------------------------------------------
# Suma de los 16 items AC_ (cada uno Likert 1-5, escala ascendente:
# mayor = mas integracion a cultura mayoritaria).
# Rango teorico: 16-80.

ac_items <- c(
  "AC_Hablar","AC_Leer","AC_Escribir","AC_Idioma_Infancia",
  "AC_Idioma_casa","AC_Idioma_trabajo","AC_Idioma_amigos",
  "AC_Idioma_pensamiento","AC_Amigos_cercanos","AC_Fiestas",
  "AC_Visitas","AC_Amigos_hijos","AC_Rituales","AC_Religion",
  "AC_Vestimenta","AC_Se_considera"
)

# Verificar que las columnas existen y son numericas
stopifnot(all(ac_items %in% names(df)))
df <- df %>% mutate(across(all_of(ac_items), as.numeric))

df <- df %>%
  mutate(acculturation_total = rowSums(across(all_of(ac_items)), na.rm = FALSE))

# Sanity check: ningun NA y rango esperable
cat("Aculturacion - NA:", sum(is.na(df$acculturation_total)),
    "| min:", min(df$acculturation_total),
    "| max:", max(df$acculturation_total),
    "| mean:", round(mean(df$acculturation_total), 2), "\n\n")

# ----------------------------------------------------------------------------
# 8. INDICE DE DOMINANCIA LINGUISTICA L1/L2
# ----------------------------------------------------------------------------
# Definicion: diferencia de la media de dominio en las 4 habilidades
# basicas (Expresion, Comprension, Lectura, Escritura) en L1 (Embera)
# vs L2 (Espanol), ambas medidas en Likert 1-5.
#
#   dom_L1L2 = mean(P_I_Expr, P_I_Comp, P_I_Lect, P_I_Esc) -
#              mean(S_I_Expr, S_I_Comp, S_I_Lect, S_I_Esc)
#
# Rango teorico: -4 a +4.
# Interpretacion: > 0  --> dominancia L1 (Embera).
#                 < 0  --> dominancia L2 (Espanol).
#                 ~ 0  --> bilinguismo equilibrado.
#
# Decision sobre missing: los 88 casos tienen las 8 variables completas
# (verificado en inspeccion). No se imputa. Si en re-analisis aparece
# missing, usar imputacion simple (median per item) - codigo comentado.

pi_vars <- c("pi_expresion","pi_comprension","pi_lectura","pi_escritura")
si_vars <- c("si_expresion","si_comprension","si_lectura","si_escritura")

# Forzar numerico (por si quedaron caracter desde el Excel)
df <- df %>% mutate(across(all_of(c(pi_vars, si_vars)), as.numeric))

df <- df %>%
  rowwise() %>%
  mutate(
    l1_proficiency = mean(c_across(all_of(pi_vars)), na.rm = FALSE),
    l2_proficiency = mean(c_across(all_of(si_vars)), na.rm = FALSE),
    dom_L1L2       = l1_proficiency - l2_proficiency
  ) %>%
  ungroup()

# (Alternativa con imputacion - dejada comentada para usos futuros)
# df <- df %>% mutate(across(all_of(pi_vars),
#   ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))

cat("Dominancia L1/L2 - NA:", sum(is.na(df$dom_L1L2)),
    "| min:", round(min(df$dom_L1L2, na.rm = TRUE), 2),
    "| max:", round(max(df$dom_L1L2, na.rm = TRUE), 2),
    "| mean:", round(mean(df$dom_L1L2, na.rm = TRUE), 2), "\n\n")

# ----------------------------------------------------------------------------
# 9. DATASET ANALITICO FINAL
# ----------------------------------------------------------------------------
# Seleccionamos el conjunto que entra a Pasos 2-5: 13 indicadores primarios
# + predictores fijos + identificacion.

primary_outcomes_13 <- c(
  "hvlt_total","hvlt_tardia","hvlt_recon",
  "rey_memoria","rey_copia",
  "flu_fon_total","flu_sem_total",
  "bta_total",
  "sdmt_correct",
  "tmt_a_inv","tmt_b_inv",
  "wcst_cat",
  "stroop_pc_calc"
)

predictors_fixed <- c(
  "edad","educacion","genero","dom_L1L2","acculturation_total"
)

sociodem_descriptive <- c(
  "ID","edad","genero","educacion","bilingue",
  "l1_proficiency","l2_proficiency"
)

# df_analytic: subset publicable y limpio
df_analytic <- df %>%
  select(all_of(unique(c(
    sociodem_descriptive,
    predictors_fixed,
    primary_outcomes_13,
    # Indicadores secundarios utiles para sensibilidad/figuras:
    "wcst_persev","wcst_err","wcst_persev_inv","wcst_err_inv",
    "hvlt_fp_rel","hvlt_fp_norel","hvlt_errores",
    "stroop_interf","sdmt_err","tmt_a","tmt_b",
    "mmse","phq9","bartel"
  ))))

# Confirmacion de N en el dataset analitico
stopifnot(nrow(df_analytic) == 88)

# Casos con datos completos en los 13 outcomes primarios + 5 predictores
df_complete <- df_analytic %>%
  filter(if_all(all_of(c(primary_outcomes_13, predictors_fixed)),
                ~ !is.na(.x)))

cat("N en df_analytic:", nrow(df_analytic), "\n")
cat("N con datos completos (13 outcomes + 5 predictores):",
    nrow(df_complete), "\n\n")

# ----------------------------------------------------------------------------
# 10. GUARDADO REPRODUCIBLE
# ----------------------------------------------------------------------------
# Persistimos los objetos clave en .rds para los pasos siguientes.

saveRDS(df_raw,      file = "df_raw.rds")
saveRDS(df,          file = "df_full.rds")
saveRDS(df_analytic, file = "df_analytic.rds")
saveRDS(df_complete, file = "df_complete.rds")

# Y un Rdata con vectores de nombres para reusar:
saveRDS(
  list(
    primary_outcomes_13 = primary_outcomes_13,
    predictors_fixed    = predictors_fixed,
    ac_items            = ac_items,
    pi_vars             = pi_vars,
    si_vars             = si_vars
  ),
  file = "vars_lookup.rds"
)

# ----------------------------------------------------------------------------
# 11. SUMMARY DE VERIFICACION
# ----------------------------------------------------------------------------

cat("=====================================================================\n")
cat("RESUMEN ESTRUCTURAL - df_analytic\n")
cat("=====================================================================\n")
cat("N filas:", nrow(df_analytic), "| N columnas:", ncol(df_analytic), "\n\n")

cat("--- Sociodemograficos ---\n")
print(summary(df_analytic[, c("edad","educacion")]))
cat("\nGenero:\n"); print(table(df_analytic$genero, useNA = "always"))
cat("\nBilingue:\n"); print(table(df_analytic$bilingue, useNA = "always"))

cat("\n--- Predictores construidos ---\n")
print(summary(df_analytic[, c("acculturation_total","dom_L1L2",
                              "l1_proficiency","l2_proficiency")]))

cat("\n--- 13 outcomes primarios (direccion: mayor = mejor) ---\n")
print(summary(df_analytic[, primary_outcomes_13]))

cat("\n--- Chequeo NA por variable analitica (esperado = 0) ---\n")
.na_count <- df_analytic %>%
  select(all_of(c(primary_outcomes_13, predictors_fixed))) %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  tidyr::pivot_longer(everything(), names_to = "var", values_to = "n_NA")
print(.na_count, n = Inf)

cat("\n=====================================================================\n")
cat("PASO 1 COMPLETADO. Objetos disponibles en sesion:\n")
cat("  df_raw       : base original sin tocar (88 filas, 147 cols)\n")
cat("  df           : base completa con todos los derivados\n")
cat("  df_analytic  : subset analitico para Pasos 2-5\n")
cat("  df_complete  : casos completos en 13 outcomes + 5 predictores\n")
cat("Archivos .rds escritos al working directory.\n")
cat("=====================================================================\n")

# Info de sesion para reportar en Methods
cat("\n")
print(sessionInfo())
