library(tidyverse)
library(ggthemes)
# Definir los argumentos del plan d muestreo 
N=150
n1=100
c1=5
P1 <- seq(0.000 , 0.140, 0.001)
# Curva caracteristica de operacion

# Funcion para seleccionar la distribucion a seleccionar

distribucion <-function(n1,N){
  r<- n1 / N
  #1) Hipergeométrica: sin/N>0.1
  if(r> 0.1){
    return("Hipergeométrica")
  }
  #2) Poisson:sin/N<=0.1yn*(n/N)>1
  #(esta reglaseevalúaantesquelabinomial,porqueesuncasoespecial)
  if(r<=0.1 &&n *r > 1) {
    return("Poisson")
  }
  #3) Binomial: si n/N<=0.1
  if(r<=0.1){
    return("Binomial")
  }
}

# Prueba
distribucion(n1,N)

#Vector eje y

m1<- round(P1 * N)

Pa1 <- phyper(c1, m1, N - m1, n1, lower.tail=T, log.p = F)

plot(P1, Pa1, type = "l", lwd = 3, col = "steelblue",
     xlab = "Proporción defectuosa en el lote (p)",
     ylab = "Probabilidad de aceptación (Pa)",
     main = "Curva CO - Muestreo de Aceptación",
     sub = paste0("Distribución Hipergeométrica: N = ", N, 
                  ", n = ", n1, ", c = ", c1),
     ylim = c(0, 1), xlim = c(0, 0.20),
     panel.first = grid())

AQL=0.05
LTPD= 0.1
m1AQL=round(AQL* N)
m1LTPD=round(LTPD*N)

#riesgo del productor

alpha <- 1-phyper(c1, m1AQL, N - m1AQL, n1, lower.tail=T, log.p = F)
alpha

#riesgo del consumidor

betha <- phyper(c1, m1LTPD, N - m1LTPD, n1, lower.tail = T,log.p = FALSE)
betha

#-------------------------------------------------------------------------------

# Definir los argumentos del plan d muestreo 
N=150
n2=80
c2=4
P1 <- seq(0.000 , 0.140, 0.001)
# Curva caracteristica de operacion



# Prueba
distribucion(n2,N)

#Vector eje y

m1<- round(P1 * N)

Pa2<- phyper(c2, m1, N - m1, n2, lower.tail=T, log.p = F)

plot(P1, Pa2, type = "l", lwd = 3, col = "steelblue",
     xlab = "Proporción defectuosa en el lote (p)",
     ylab = "Probabilidad de aceptación (Pa)",
     main = "Curva CO - Muestreo de Aceptación",
     sub = paste0("Distribución Hipergeométrica: N = ", N, 
                  ", n = ", n2, ", c = ", c2),
     ylim = c(0, 1), xlim = c(0, 0.20),
     panel.first = grid())

AQL=0.05
LTPD= 0.1
m1AQL=round(AQL* N)
m1LTPD=round(LTPD*N)

#riesgo del productor

alpha2 <- 1-phyper(c2, m1AQL, N - m1AQL, n2, lower.tail=T, log.p = F)
alpha2

#riesgo del consumidor

betha2 <- phyper(c2, m1LTPD, N - m1LTPD, n2, lower.tail = T,log.p = FALSE)
betha2

#-------------------------------------------------------------------------------

# Definir los argumentos del plan d muestreo 
N=150
n3=40
c3=2
P1 <- seq(0.000 , 0.140, 0.001)
# Curva caracteristica de operacion



# Prueba
distribucion(n3,N)

#Vector eje y

m1<- round(P1 * N)

Pa3 <- phyper(c3, m1, N - m1, n3, lower.tail=T, log.p = F)

plot(P1, Pa3, type = "l", lwd = 3, col = "steelblue",
     xlab = "Proporción defectuosa en el lote (p)",
     ylab = "Probabilidad de aceptación (Pa)",
     main = "Curva CO - Muestreo de Aceptación",
     sub = paste0("Distribución Hipergeométrica: N = ", N, 
                  ", n = ", n3, ", c = ", c3),
     ylim = c(0, 1), xlim = c(0, 0.20),
     panel.first = grid())

AQL=0.05
LTPD= 0.1
m1AQL=round(AQL* N)
m1LTPD=round(LTPD*N)

#riesgo del productor

alpha3 <- 1-phyper(c3, m1AQL, N - m1AQL, n3, lower.tail=T, log.p = F)
alpha3

#riesgo del consumidor

betha3 <- phyper(c3, m1LTPD, N - m1LTPD, n3, lower.tail = T,log.p = FALSE)
betha3

Base=data.frame(P1,Pa1,Pa2,Pa3)

ggplot(Base, aes(x=P1))+geom_line(aes(y=Pa1), color="blue")+
  geom_line(aes(y=Pa2), color="red")+geom_line(aes(y=Pa3), color="green")+
  theme_economist_white()+la