#library(AQLSchemes)
#library(AcceptanceSampling)
#planS_1 <- AASingle('Normal')
#planS_1
#planS_2<-AASingle('Normal')
#planS_2
#planS_3<-AASingle('Normal')
#planS_3
#planE_1<-AASingle('Tightened')
#planE_1
#planE_2<-AASingle('Tightened')
#planE_2
#planE_3<-AASingle('Tightened')
#planE_3
#planr_1<-AASingle('Reduced')
#planr_1
#planr_2<-AASingle('Reduced')
#planr_2
#planr_3<-AASingle('Reduced')
#planr_3
# Crear el data frame sin columna Plan
tabla_simple <- data.frame(
  Nivel = c("I", "II", "III",
            "I", "II", "III",
            "I", "II", "III"),
  n = c(50, 125, 200, 50, 125, 200, 20, 50, 80),
  c = c(3, 7, 10, 2, 5, 8, 1, 3, 5),
  r = c(4, 8, 11, 3, 6, 9, 4, 6, 8)
)

library(knitr)
library(kableExtra)

tabla_simple %>%
  kbl(
    caption = "Planes de muestreo simple según nivel de inspección",
    col.names = c("Nivel", "n", "c", "r"),
    align = "c",
    booktabs = TRUE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    position = "center",
    font_size = 12  # Reducido para mejor ajuste
  ) %>%
  group_rows("Normal", 1, 3, bold = TRUE, background = "#f0f0f0") %>%
  group_rows("Estricto", 4, 6, bold = TRUE, background = "#f0f0f0") %>%
  group_rows("Reducido", 7, 9, bold = TRUE, background = "#f0f0f0") %>%
  column_spec(1:4, width = "2cm") %>%  # Anchos fijos iguales
  add_header_above(c(" " = 1, "Parámetros del plan" = 3)) %>%
  footnote(general = "n: tamaño de muestra; c: número de aceptación; r: número de rechazo")
library(AcceptanceSampling)
library(tidyverse)
colores_manuales <- c(
  "Normal (I)"="#003f8a",
  "Normal (II)"="#0072CF",
  "Normal (III)"="#66B2FF",
  "Estricta (I)"="#8B0000",
  "Estricta (II)"="#CC0000",
  "Estricta (III)"="#FF6666",
  "Reducida (I)"="#1a5200",
  "Reducida (II)"="#2E8B00",
  "Reducida (III)"="#7FCC00"
)
tabla_planes <- tribble(
  ~Tipo,      ~Plan, ~n,  ~c,
  "Normal",   "I",   50,  3,
  "Estricta", "I",   50,  2,
  "Reducida", "I",   20,  1,
  "Normal",   "II",  125, 7,
  "Estricta", "II",  125, 5,
  "Reducida", "II",  50,  3,
  "Normal",   "III", 200, 10,
  "Estricta", "III", 200, 8,
  "Reducida", "III", 80,  5
)

pd_vector <- seq(0,0.20,length.out=100)

datos_grafico <- tabla_planes %>%
  group_by(Tipo,Plan) %>%
  do({
    plan <- OC2c(n=.$n,c=.$c,type="binomial",pd=pd_vector)
    tibble(pd=pd_vector, pa=plan@paccept)
  }) %>%
  ungroup()

# colores estilo gráfico base
colores <- c(
  "Normal"="#003f8a",
  "Estricta"="#CC0000",
  "Reducida"="#2E8B00"
)

ggplot(datos_grafico,
       aes(x=pd,y=pa,color=Tipo,linetype=Plan))+
  
  geom_line(linewidth=1.2)+
  
  scale_color_manual(values=colores)+
  
  scale_linetype_manual(values=c(
    "I"="solid",
    "II"="dashed",
    "III"="dotted"
  ))+
  
  scale_x_continuous(
    breaks=seq(0,0.2,0.02),
    limits=c(0,0.2)
  )+
  
  scale_y_continuous(
    breaks=seq(0,1,0.1),
    limits=c(0,1)
  )+
  
  labs(
    title="Curvas OC",
    x="Proporción defectuosa",
    y="P(aceptación)",
    color="Tipo de inspección",
    linetype="Plan"
  )+
  
  theme_minimal(base_size=13)+
  theme(
    panel.grid.major=element_line(color="grey80"),
    panel.grid.minor=element_line(color="grey90"),
    axis.text.x=element_text(angle=90,vjust=0.5),
    plot.title=element_text(hjust=0.5,face="bold")
  )
# Calcular AOQ
N_lote <- 3000

datos_aoq <- tabla_planes %>%
  group_by(Tipo, Plan) %>%
  do({
    plan <- OC2c(n = .$n, c = .$c, type = "binomial", pd = pd_vector)
    aoq_calculado <- plan@paccept * pd_vector * ((N_lote - .$n) / N_lote)
    tibble(pd = pd_vector, aoq = aoq_calculado)
  }) %>%
  ungroup()

colores <- c(
  "Normal"="#003f8a",
  "Estricta"="#CC0000",
  "Reducida"="#2E8B00"
)

ggplot(datos_aoq,
       aes(x = pd, y = aoq, color = Tipo, linetype = Plan)) +
  
  geom_line(linewidth = 1.2) +
  
  scale_color_manual(values = colores) +
  
  scale_linetype_manual(values = c(
    "I" = "solid",
    "II" = "dashed",
    "III" = "dotted"
  )) +
  
  scale_x_continuous(
    breaks = seq(0, 0.2, 0.02),
    limits = c(0, 0.2)
  ) +
  
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, max(datos_aoq$aoq))
  ) +
  
  labs(
    title = "Curvas AOQ ",
    x = "Proporcion defectuosa",
    y = "AOQ",
    color = "Tipo de inspeccion",
    linetype = "Plan"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
# ATI
N_lote <- 3000 

datos_ati <- tabla_planes %>%
  group_by(Tipo, Plan) %>%
  do({
    plan <- OC2c(n = .$n, c = .$c, type = "binomial", pd = pd_vector)
    ati_val <- .$n + (1 - plan@paccept) * (N_lote - .$n)
    tibble(pd = pd_vector, ati = ati_val)
  }) %>%
  ungroup()

colores <- c(
  "Normal"="#003f8a",
  "Estricta"="#CC0000",
  "Reducida"="#2E8B00"
)

ggplot(datos_ati,
       aes(x = pd, y = ati, color = Tipo, linetype = Plan)) +
  
  geom_line(linewidth = 1.2) +
  
  scale_color_manual(values = colores) +
  
  scale_linetype_manual(values = c(
    "I" = "solid",
    "II" = "dashed",
    "III" = "dotted"
  )) +
  
  scale_x_continuous(
    breaks = seq(0, 0.20, 0.02),
    limits = c(0, 0.20)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 3000, 500),
    limits = c(0, 3100)
  ) +
  
  labs(
    title = paste("Curvas ATI"),
    x = "Proporcion defectuosa",
    y = "Unidades inspeccionadas",
    color = "Tipo de inspeccion",
    linetype = "Plan"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
library(AcceptanceSampling)
library(knitr)
library(kableExtra)

N_lote <- 3000
AQL    <- 0.025

# Calcular metricas manualmente
planes <- data.frame(
  Inspeccion = c(rep("Normal",3), rep("Estricta",3), rep("Reducida",3)),
  Nivel      = rep(c("I","II","III"), 3),
  n          = c(50, 125, 200, 50, 125, 200, 20, 50, 80),
  c          = c(3,  7,   10,  2,  5,   8,   1,  3,  5)
)

planes$Pa_AQL <- sapply(1:nrow(planes), function(i) {
  OC2c(n = planes$n[i], c = planes$c[i], 
       type = "binomial", pd = AQL)@paccept
})

planes$AOQL <- sapply(1:nrow(planes), function(i) {
  pv   <- seq(0, 0.20, length.out = 500)
  pa_v <- OC2c(n = planes$n[i], c = planes$c[i], 
               type = "binomial", pd = pv)@paccept
  max(pa_v * pv * (N_lote - planes$n[i]) / N_lote)
})

planes$ATI_AQL <- planes$n + (1 - planes$Pa_AQL) * (N_lote - planes$n)

planes$Conveniencia <- c(
  "Balance costo-proteccion",
  "Balance costo-proteccion",
  "Balance costo-proteccion",
  "Alta proteccion, mayor costo",
  "Alta proteccion, mayor costo",
  "Alta proteccion, mayor costo",
  "Economico, historial confiable",
  "Economico, historial confiable",
  "Economico, historial confiable"
)

# Formatear columnas
planes$Pa_AQL  <- paste0(round(planes$Pa_AQL * 100, 1), "%")
planes$AOQL    <- paste0(round(as.numeric(planes$AOQL) * 100, 2), "%")
planes$ATI_AQL <- round(planes$ATI_AQL, 0)

kbl(planes,
    caption   = "Tabla comparativa de planes simples (AQL = 2.5%, N = 3000)",
    col.names = c("Inspeccion", "Nivel", "n", "c",
                  "Pa (p=AQL)", "AOQL", "ATI (p=AQL)", "Conveniencia"),
    align     = c("l","c","c","c","c","c","c","l"),
    booktabs  = TRUE,
    row.names = FALSE) %>%
  kable_styling(
    font_size     = 10,
    latex_options = c("hold_position", "scale_down")
  ) %>%
  pack_rows("Normal",   1, 3) %>%
  pack_rows("Estricta", 4, 6) %>%
  pack_rows("Reducida", 7, 9)
#planDN_1<-AADouble('Normal')
#planDN_1
#planDN_2<-AADouble('Normal')
#planDN_2
#planDN_3<-AADouble('Normal')
#planDN_3
#planDE_1<-AADouble('Tightened')
#planDE_1
#planDE_2<-AADouble('Tightened')
#planDE_2
#planDE_3<-AADouble('Tightened')
#planDE_3
#planDr_1<-AADouble('Reduced')
#planDr_1
#planDr_2<-AADouble('Reduced')
#planDr_2
#planDr_3<-AADouble('Reduced')
#planDr_3
library(knitr)
library(kableExtra)

planes_d <- data.frame(
  Inspeccion = c(rep("Normal",3), rep("Estricta",3), rep("Reducida",3)),
  Nivel      = rep(c("I","II","III"), 3),
  n1  = c(32,  80,  125, 32,  80,  125, 13,  32,  50),
  c1  = c(1,   3,   5,   0,   2,   3,   0,   2,   3),
  r1  = c(4,   7,   9,   3,   5,   7,   3,   4,   6),
  n2  = c(32,  80,  125, 32,  80,  125, 13,  32,  50),
  c2  = c(4,   8,   12,  3,   6,   11,  3,   5,   7),
  r2  = c(5,   9,   13,  4,   7,   12,  4,   6,   8)
)

kbl(planes_d[, c("Inspeccion","Nivel","n1","c1","r1","n2","c2","r2")],
    caption   = "Planes de muestreo doble (AQL = 2.5%, N = 3000)",
    col.names = c("Inspeccion","Nivel","n1","c1","r1","n2","c2","r2"),
    align     = c("l","c","c","c","c","c","c","c"),
    booktabs  = TRUE,
    row.names = FALSE) %>%
  kable_styling(
    font_size     = 10,
    latex_options = c("hold_position")
  ) %>%
  pack_rows("Normal",   1, 3) %>%
  pack_rows("Estricta", 4, 6) %>%
  pack_rows("Reducida", 7, 9)
pd_b <- seq(0, 0.15, 0.001)
N_lote <- 3000

Pa_doble_b <- function(n1, c1, r1, n2, c2, r2, pd) {
  Pa1 <- pbinom(c1, n1, pd)
  Pa2 <- numeric(length(pd))
  for (i in seq_along(pd)) {
    pa2_i <- 0
    for (d1 in (c1 + 1):(r1 - 1)) {
      for (d2 in 0:(r2 - d1 - 1)) {
        if ((d1 + d2) <= c2) {
          pa2_i <- pa2_i + dbinom(d1, n1, pd[i]) * dbinom(d2, n2, pd[i])
        }
      }
    }
    Pa2[i] <- pa2_i
  }
  return(Pa1 + Pa2)
}

# Normal
Pa_NI_Nor   <- Pa_doble_b(32,  1, 4, 32,  4,  5,  pd_b)
Pa_NII_Nor  <- Pa_doble_b(80,  3, 7, 80,  8,  9,  pd_b)
Pa_NIII_Nor <- Pa_doble_b(125, 5, 9, 125, 12, 13, pd_b)

# Estricto
Pa_NI_Est   <- Pa_doble_b(32,  0, 3, 32,  3,  4,  pd_b)
Pa_NII_Est  <- Pa_doble_b(80,  2, 5, 80,  6,  7,  pd_b)
Pa_NIII_Est <- Pa_doble_b(125, 3, 7, 125, 11, 12, pd_b)

# Reducido
Pa_NI_Red   <- Pa_doble_b(13, 0, 3, 13, 3, 4, pd_b)
Pa_NII_Red  <- Pa_doble_b(32, 2, 4, 32, 5, 6, pd_b)
Pa_NIII_Red <- Pa_doble_b(50, 3, 6, 50, 7, 8, pd_b)
#
colores_b <- c("Normal" = "#003f8a", 
               "Estricto" = "#CC0000", 
               "Reducido" = "#2E8B00")

df_doble_oc <- tibble(
  pd   = rep(pd_b, 9),
  Pa   = c(Pa_NI_Nor, Pa_NII_Nor, Pa_NIII_Nor,
           Pa_NI_Est, Pa_NII_Est, Pa_NIII_Est,
           Pa_NI_Red, Pa_NII_Red, Pa_NIII_Red),
  Tipo = rep(c("Normal","Normal","Normal",
               "Estricto","Estricto","Estricto",
               "Reducido","Reducido","Reducido"),
             each = length(pd_b)),
  Plan = rep(c("I","II","III"), 
             each = length(pd_b), times = 3)
)

ggplot(df_doble_oc, aes(x = pd, y = Pa, 
                        color = Tipo, linetype = Plan)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = colores_b) +
  scale_linetype_manual(values = c("I"   = "solid",
                                   "II"  = "dashed",
                                   "III" = "dotted")) +
  scale_x_continuous(breaks = seq(0, 0.15, 0.03),
                     labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 1)) +
  labs(title    = "Curva OC – Planes Dobles",
       x        = "Proporción defectuosa",
       y        = "P(aceptación)",
       color    = "Tipo de inspección",
       linetype = "Plan")
df_doble_aoq <- tibble(
  pd  = rep(pd_b, 9),
  AOQ = c(Pa_NI_Nor   * pd_b * (N_lote - 32)  / N_lote,
          Pa_NII_Nor  * pd_b * (N_lote - 80)  / N_lote,
          Pa_NIII_Nor * pd_b * (N_lote - 125) / N_lote,
          Pa_NI_Est   * pd_b * (N_lote - 32)  / N_lote,
          Pa_NII_Est  * pd_b * (N_lote - 80)  / N_lote,
          Pa_NIII_Est * pd_b * (N_lote - 125) / N_lote,
          Pa_NI_Red   * pd_b * (N_lote - 13)  / N_lote,
          Pa_NII_Red  * pd_b * (N_lote - 32)  / N_lote,
          Pa_NIII_Red * pd_b * (N_lote - 50)  / N_lote),
  Tipo = rep(c("Normal","Normal","Normal",
               "Estricto","Estricto","Estricto",
               "Reducido","Reducido","Reducido"),
             each = length(pd_b)),
  Plan = rep(c("I","II","III"), each = length(pd_b), times = 3)
)

ggplot(df_doble_aoq, aes(x = pd, y = AOQ, color = Tipo, linetype = Plan)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = colores_b) +
  scale_linetype_manual(values = c("I" = "solid", "II" = "dashed", "III" = "dotted")) +
  scale_x_continuous(breaks = seq(0, 0.15, 0.03), labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title    = "Curva AOQ – Planes Dobles",
       x        = "Proporción defectuosa",
       y        = "AOQ (calidad de salida promedio)",
       color    = "Tipo de inspección",
       linetype = "Plan")
library(AcceptanceSampling)
library(tidyverse)

N_lote <- 3000
pd     <- seq(0, 0.15, length.out = 200)

# --- Pa simple ---
pa_simple <- OC2c(n = 125, c = 7, type = "binomial", pd = pd)@paccept

# --- Pa doble ---
Pa_doble <- function(n1, c1, r1, n2, c2, p) {
  pa1 <- pbinom(c1, n1, p)
  pa2 <- numeric(length(p))
  for (i in seq_along(p)) {
    s <- 0
    for (d1 in (c1 + 1):(r1 - 1)) {
      for (d2 in 0:(c2 - d1)) {
        if (d2 >= 0 && d2 <= n2)
          s <- s + dbinom(d1, n1, p[i]) * dbinom(d2, n2, p[i])
      }
    }
    pa2[i] <- s
  }
  pa1 + pa2
}
pa_doble <- Pa_doble(80, 3, 7, 80, 8, pd)

# --- ASN doble ---
ASN_doble <- function(n1, c1, r1, n2, p) {
  p_segunda <- numeric(length(p))
  for (i in seq_along(p)) {
    s <- 0
    for (d1 in (c1 + 1):(r1 - 1))
      s <- s + dbinom(d1, n1, p[i])
    p_segunda[i] <- s
  }
  n1 + p_segunda * n2
}
asn <- ASN_doble(80, 3, 7, 80, pd)

# --- AOQ ---
aoq_simple <- pa_simple * pd * (N_lote - 125) / N_lote
aoq_doble  <- pa_doble  * pd * (N_lote - asn)  / N_lote

# --- Data frames para ggplot ---
df_oc  <- tibble(pd, Simple = pa_simple, Doble = pa_doble) %>%
  pivot_longer(-pd, names_to = "Plan", values_to = "Pa")

df_aoq <- tibble(pd, Simple = aoq_simple, Doble = aoq_doble) %>%
  pivot_longer(-pd, names_to = "Plan", values_to = "AOQ")

df_asn <- tibble(pd,
                 `Simple (n fijo)` = rep(125, length(pd)),
                 `Doble (ASN)`     = asn) %>%
  pivot_longer(-pd, names_to = "Plan", values_to = "Muestra")

cols <- c("Simple" = "#185FA5", "Doble" = "#3B6D11",
          "Simple (n fijo)" = "#185FA5", "Doble (ASN)" = "#3B6D11")
ggplot(df_oc, aes(x = pd, y = Pa, color = Plan, linetype = Plan)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0.025, linetype = "dotted",
             color = "#BA7517", size = 0.8) +
  annotate("text", x = 0.027, y = 0.3, label = "AQL = 2.5%",
           color = "#BA7517", size = 3.2, hjust = 0) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("Simple" = "solid", "Doble" = "dashed")) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Curva OC – Simple vs. Doble (Normal Nivel II)",
       x = "Proporción defectuosa (p)",
       y = "Probabilidad de aceptación",
       color = "Plan", linetype = "Plan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "brown"),
        legend.position = "bottom")
ggplot(df_aoq, aes(x = pd, y = AOQ, color = Plan, linetype = Plan)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0.025, linetype = "dotted",
             color = "#BA7517", size = 0.8) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("Simple" = "solid", "Doble" = "dashed")) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Curva AOQ – Simple vs. Doble (Normal Nivel II)",
       x = "Proporción defectuosa (p)",
       y = "Calidad de salida promedio (AOQ)",
       color = "Plan", linetype = "Plan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "brown"),
        legend.position = "bottom")
ggplot(df_asn, aes(x = pd, y = Muestra, color = Plan, linetype = Plan)) +
  geom_line(size = 1.1) +
  geom_vline(xintercept = 0.025, linetype = "dotted",
             color = "#BA7517", size = 0.8) +
  scale_color_manual(values = cols) +
  scale_linetype_manual(values = c("Simple (n fijo)" = "solid",
                                   "Doble (ASN)"     = "dashed")) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Tamaño de muestra promedio (ASN) – Simple vs. Doble",
       x = "Proporción defectuosa (p)",
       y = "Unidades inspeccionadas en promedio",
       color = "Plan", linetype = "Plan") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", color = "brown"),
        legend.position = "bottom")
library(knitr)
library(kableExtra)

# Precalcular valores antes del data.frame
pa_aql_simple <- round(OC2c(125, 7, type = "binomial", 
                            pd = 0.025)@paccept * 100, 1)
pa_aql_doble  <- round(Pa_doble(80, 3, 7, 80, 8, 0.025) * 100, 1)
aoql_simple   <- round(max(aoq_simple) * 100, 2)
aoql_doble    <- round(max(aoq_doble) * 100, 2)
asn_aql       <- round(ASN_doble(80, 3, 7, 80, 0.025), 0)

tabla_c <- data.frame(
  Criterio = c("Tamaño de muestra", 
               "Pa en AQL (p=2.5%)",
               "AOQL", 
               "ASN en AQL", 
               "Complejidad operativa",
               "Psicología del proveedor",
               "Recomendado cuando..."),
  Simple = c("Fijo: n=125",
             paste0(pa_aql_simple, "%"),
             paste0(aoql_simple, "%"),
             "125 (siempre)",
             "Baja — una sola decisión",
             "Neutral",
             "Proceso variable o errático"),
  Doble  = c("Variable: 80–160",
             paste0(pa_aql_doble, "%"),
             paste0(aoql_doble, "%"),
             paste0("~", asn_aql),
             "Media — dos etapas posibles",
             "Favorable (segunda oportunidad)",
             "Proceso estable, costo inspección alto")
)

kbl(tabla_c,
    caption   = "Comparación del mejor plan simple vs. el mejor plan doble (Normal Nivel II)",
    col.names = c("Criterio", "Simple (n=125, c=7)", "Doble (n1=80, c1=3)"),
    align     = c("l","c","c"),
    booktabs  = TRUE) %>%
  kable_styling(font_size = 10,
                latex_options = c("hold_position", "scale_down"))