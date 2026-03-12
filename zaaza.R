library(AcceptanceSampling)
library(tidyverse)

# 1. Definir la tabla de planes
tabla_planes <- tribble(
  ~Tipo,      ~Plan, ~n, ~c,
  "Normal",   1,     50,  3,
  "Normal",   2,     125, 7,
  "Normal",   3,     200, 10,
  "Estricto", 1,     50,  2,
  "Estricto", 2,     125, 5,
  "Estricto", 3,     200, 8,
  "Reducido", 1,     20,  1,
  "Reducido", 2,     50,  3,
  "Reducido", 3,     80,  5
)

# 2. Configurar el vector de proporción defectuosa (eje X)
pd_vector <- seq(0, 0.20, by = 0.005)

# 3. Preparar el gráfico base (vacío)
plot(NULL, xlim = c(0, 0.20), ylim = c(0, 1),
     xlab = "Proporción defectuosa (pd)", 
     ylab = "Probabilidad de Aceptación P(a)",
     main = "Comparación de Curvas OC: Normal, Estricto y Reducido, plan simple")

# 4. Colores para diferenciar los tipos
colores <- c("Normal" = "blue", "Estricto" = "red", "Reducido" = "darkgreen")
tipos_linea <- c("Normal" = 1, "Estricto" = 2, "Reducido" = 3)

# 5. Bucle para calcular y graficar cada plan
for(i in 1:nrow(tabla_planes)) {
  # Calcular el plan usando la distribución Binomial
  plan <- OC2c(n = tabla_planes$n[i], 
               c = tabla_planes$c[i], 
               type = "binomial", 
               pd = pd_vector)
  
  # Extraer datos
  pa <- plan@paccept
  tipo <- tabla_planes$Tipo[i]
  
  # Añadir la línea al gráfico existente
  lines(pd_vector, pa, 
        col = colores[tipo], 
        lty = tipos_linea[tipo], 
        lwd = 2)
}

# 6. Añadir leyenda para entender el gráfico
legend("topright", 
       legend = names(colores), 
       col = colores, 
       lty = tipos_linea, 
       lwd = 2, 
       title = "Tipo de Plan")

grid() # Añadir cuadrícula para facilitar la lectura
