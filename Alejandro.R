planD_Normal   <- AADouble('Normal')
planD_Normal
planD_Estricto <- AADouble('Tightened')
planD_Estrict
planD_Reducido <- AADouble('Reduced')
planD_Reducido

pd <- seq(0, 0.15, 0.001)

# Función para calcular Pa doble
Pa_doble <- function(n1, c1, r1, n2, c2, pd) {
  Pa1 <- pbinom(c1, n1, pd)
  Pa2 <- numeric(length(pd))
  for (i in seq_along(pd)) {
    pa2_i <- 0
    for (d1 in (c1 + 1):(r1 - 1)) {
      pa2_i <- pa2_i + dbinom(d1, n1, pd[i]) * pbinom(c2 - d1, n2, pd[i])
    }
    Pa2[i] <- pa2_i
  }
  return(Pa1 + Pa2)
}

#--------------------------------------#
# Normal
#--------------------------------------#
Pa_NI_Nor   <- Pa_doble(n1=32,  c1=1, r1=4, n2=32,  c2=4,  pd)
Pa_NII_Nor  <- Pa_doble(n1=80,  c1=3, r1=7, n2=80,  c2=8,  pd)
Pa_NIII_Nor <- Pa_doble(n1=125, c1=5, r1=9, n2=125, c2=12, pd)

#--------------------------------------#
# Estricto
#--------------------------------------#
Pa_NI_Est   <- Pa_doble(n1=32,  c1=0, r1=3, n2=32,  c2=3,  pd)
Pa_NII_Est  <- Pa_doble(n1=80,  c1=2, r1=5, n2=80,  c2=6,  pd)
Pa_NIII_Est <- Pa_doble(n1=125, c1=3, r1=7, n2=125, c2=11, pd)

#--------------------------------------#
# Reducido
#--------------------------------------#
Pa_NI_Red   <- Pa_doble(n1=13, c1=0, r1=4, n2=13, c2=1, pd)
Pa_NII_Red  <- Pa_doble(n1=32, c1=1, r1=4, n2=32, c2=4, pd)
Pa_NIII_Red <- Pa_doble(n1=50, c1=2, r1=6, n2=50, c2=6, pd)

#--------------------------------------#
# Gráficas
#--------------------------------------#
x11()
par(mfrow = c(2, 3))

# --- Curvas OC ---
plot(pd, Pa_NI_Nor, type='l', main="OC - Nivel I",
     xlab="Proporción defectuosa", ylab="P(aceptación)", ylim=c(0,1))
lines(pd, Pa_NI_Est, lty=2, col="red")
lines(pd, Pa_NI_Red, lty=3, col="blue")
legend("topright", legend=c("Normal","Estricto","Reducido"),
       lty=c(1,2,3), col=c("black","red","blue"))
grid()

plot(pd, Pa_NII_Nor, type='l', main="OC - Nivel II",
     xlab="Proporción defectuosa", ylab="P(aceptación)", ylim=c(0,1))
lines(pd, Pa_NII_Est, lty=2, col="red")
lines(pd, Pa_NII_Red, lty=3, col="blue")
legend("topright", legend=c("Normal","Estricto","Reducido"),
       lty=c(1,2,3), col=c("black","red","blue"))
grid()

plot(pd, Pa_NIII_Nor, type='l', main="OC - Nivel III",
     xlab="Proporción defectuosa", ylab="P(aceptación)", ylim=c(0,1))
lines(pd, Pa_NIII_Est, lty=2, col="red")
lines(pd, Pa_NIII_Red, lty=3, col="blue")
legend("topright", legend=c("Normal","Estricto","Reducido"),
       lty=c(1,2,3), col=c("black","red","blue"))
grid()

# --- Curvas AOQ ---
plot(pd, Pa_NI_Nor*pd, type='l', main="AOQ - Nivel I",
     xlab="Proporción defectuosa", ylab="AOQ",
     ylim=c(0, max(Pa_NI_Nor*pd, Pa_NI_Est*pd, Pa_NI_Red*pd)*1.2))
lines(pd, Pa_NI_Est*pd, lty=2, col="red")
lines(pd, Pa_NI_Red*pd, lty=3, col="blue")
abline(h=max(Pa_NI_Nor*pd), lty=2, col="black")
abline(h=max(Pa_NI_Est*pd), lty=2, col="red")
abline(h=max(Pa_NI_Red*pd), lty=2, col="blue")
legend("topright", legend=c("Normal","Estricto","Reducido"),
       lty=c(1,2,3), col=c("black","red","blue"))
grid()

plot(pd, Pa_NII_Nor*pd, type='l', main="AOQ - Nivel II",
     xlab="Proporción defectuosa", ylab="AOQ",
     ylim=c(0, max(Pa_NII_Nor*pd, Pa_NII_Est*pd, Pa_NII_Red*pd)*1.2))
lines(pd, Pa_NII_Est*pd, lty=2, col="red")
lines(pd, Pa_NII_Red*pd, lty=3, col="blue")
abline(h=max(Pa_NII_Nor*pd), lty=2, col="black")
abline(h=max(Pa_NII_Est*pd), lty=2, col="red")
abline(h=max(Pa_NII_Red*pd), lty=2, col="blue")
legend("topright", legend=c("Normal","Estricto","Reducido"),
       lty=c(1,2,3), col=c("black","red","blue"))
grid()

plot(pd, Pa_NIII_Nor*pd, type='l', main="AOQ - Nivel III",
     xlab="Proporción defectuosa", ylab="AOQ",
     ylim=c(0, max(Pa_NIII_Nor*pd, Pa_NIII_Est*pd, Pa_NIII_Red*pd)*1.2))
lines(pd, Pa_NIII_Est*pd, lty=2, col="red")
lines(pd, Pa_NIII_Red*pd, lty=3, col="blue")
abline(h=max(Pa_NIII_Nor*pd), lty=2, col="black")
abline(h=max(Pa_NIII_Est*pd), lty=2, col="red")
abline(h=max(Pa_NIII_Red*pd), lty=2, col="blue")
legend("topright", legend=c("Normal","Estricto","Reducido"),
       lty=c(1,2,3), col=c("black","red","blue"))
grid()
##############################
azules <- c("#003f8a", "#0072CF", "#66B2FF")
rojos  <- c("#8B0000", "#CC0000", "#FF6666")
verdes <- c("#1a5200", "#2E8B00", "#7FCC00")

# lty: 1=sólida, 2=punteada, 3=punto-raya
# Nivel I = sólida, Nivel II = punteada, Nivel III = punto-raya

x11()
plot(pd, Pa_NI_Nor, type='l', lwd=2, lty=1, col=azules[1],
     main="Curva OC - Planes Dobles",
     xlab="Proporción defectuosa", ylab="P(aceptación)", ylim=c(0,1))

# Normal
lines(pd, Pa_NII_Nor,  lwd=2, lty=2, col=azules[2])
lines(pd, Pa_NIII_Nor, lwd=2, lty=3, col=azules[3])

# Estricto
lines(pd, Pa_NI_Est,   lwd=2, lty=1, col=rojos[1])
lines(pd, Pa_NII_Est,  lwd=2, lty=2, col=rojos[2])
lines(pd, Pa_NIII_Est, lwd=2, lty=3, col=rojos[3])

# Reducido
lines(pd, Pa_NI_Red,   lwd=2, lty=1, col=verdes[1])
lines(pd, Pa_NII_Red,  lwd=2, lty=2, col=verdes[2])
lines(pd, Pa_NIII_Red, lwd=2, lty=3, col=verdes[3])

legend("topright",
       legend=c("Normal I","Normal II","Normal III",
                "Estricto I","Estricto II","Estricto III",
                "Reducido I","Reducido II","Reducido III"),
       col=c(azules, rojos, verdes),
       lwd=2, lty=c(1,2,3, 1,2,3, 1,2,3), cex=0.75)
grid()

