library(AQLSchemes)
library(AcceptanceSampling)
  
planS_1 <- AASingle('Normal')
planS_1

planS_2<-AASingle('Normal')
planS_2


planS_3<-AASingle('Normal')
planS_3

planE_1<-AASingle('Tightened')
planE_1
planE_2<-AASingle('Tightened')
planE_2
planE_3<-AASingle('Tightened')
planE_3

planr_1<-AASingle('Reduced')
planr_1
planr_2<-AASingle('Reduced')
planr_2
planr_3<-AASingle('Reduced')
planr_3

planDN_1<-AADouble('Normal')
planDN_1
planDN_2<-AADouble('Normal')
planDN_2
planDN_3<-AADouble('Normal')
planDN_3

library(AcceptanceSampling)
library(tidyverse)

# 1. Definir la tabla de planes (ajustada para que coincida con la lógica de la imagen)
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

pd_vector <- seq(0, 0.20, length.out = 100)
datos_grafico <- tabla_planes %>%
  group_by(Tipo, Plan) %>%
  do({
    plan <- OC2c(n = .$n, c = .$c, type = "binomial", pd = pd_vector)
    tibble(pd = pd_vector, pa = plan@paccept)
  }) %>%
  ungroup() %>%
  mutate(Etiqueta = paste0(Tipo, " (", Plan, ")"))
colores_manuales <- c(
  "Normal (I)"   = "#2E4053", "Estricta (I)" = "#943126", "Reducida (I)" = "#5D6D7E",
  "Normal (II)"  = "#2874A6", "Estricta (II)"= "#1E8449", "Reducida (II)" = "#A04000",
  "Normal (III)" = "#5499C7", "Estricta (III)"= "#82E0AA", "Reducida (III)"= "#D98880"
)

ggplot(datos_grafico, aes(x = pd, y = pa, color = Etiqueta)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 0.2, 0.02), limits = c(0, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  scale_color_manual(values = colores_manuales) +
  labs(
    title = "Curvas de operación (OC)",
    x = "Proporción de defectuosos",
    y = "Probabilidad de aceptación",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", color = "brown")
  )
N_lote <- 3000 
# Calcular AOQ

datos_aoq <- tabla_planes %>%
  group_by(Tipo, Plan) %>%
  do({
    plan <- OC2c(n = .$n, c = .$c, type = "binomial", pd = pd_vector)
    aoq_calculado <- plan@paccept * pd_vector * ((N_lote - .$n) / N_lote)
    tibble(pd = pd_vector, aoq = aoq_calculado)
  }) %>%
  ungroup() %>%
  mutate(Etiqueta = factor(paste0(Tipo, " (", Plan, ")"), 
                           levels = names(colores_manuales)))
ggplot(datos_aoq, aes(x = pd, y = aoq, color = Etiqueta)) +
  geom_line(size = 1.1) +
  scale_x_continuous(breaks = seq(0, 0.2, 0.02)) +
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(values = colores_manuales) +
  labs(title = "Curvas de Calidad de Salida Promedio (AOQ)",
       x = "Proporción de defectuosos (p)",
       y = "AOQ (Calidad saliente)",
       color = "Plan") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#ati


datos_ati <- tabla_planes %>%
  group_by(Tipo, Plan) %>%
  do({
    plan <- OC2c(n = .$n, c = .$c, type = "binomial", pd = pd_vector)
    ati_val <- .$n + (1 - plan@paccept) * (N_lote - .$n)
    tibble(pd = pd_vector, ati = ati_val)
  }) %>%
  ungroup() %>%
  mutate(Etiqueta = factor(paste0(Tipo, " (", Plan, ")"), 
                           levels = names(colores_manuales))) 
ggplot(datos_ati, aes(x = pd, y = ati, color = Etiqueta)) +
  geom_line(size = 1.2) +
  scale_y_continuous(breaks = seq(0, 3000, 500), limits = c(0, 3100)) +
  scale_x_continuous(breaks = seq(0, 0.20, 0.02)) +
  scale_color_manual(values = colores_manuales) +
  labs(
    title = paste("Inspección Total Promedio (ATI) para N =", N_lote),
    x = "Proporción de defectuosos (p)",
    y = "Número de unidades inspeccionadas",
    color = "Plan de Muestreo"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "brown"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )
