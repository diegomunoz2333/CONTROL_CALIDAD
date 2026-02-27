N <- 100 ; n <- 50 ; c <- 2
P <- seq(0.000 , 0.200, 0.001)
Pa_binom <- pbinom(c, n, P, lower.tail=T)
Pa_poisson <- ppois(c, lambda = n*P)
Pa_hiper <- numeric(length(P))

for(i in 1:length(P)){
  D <- round(N * P[i])
  Pa_hiper[i] <- phyper(c, D, N-D, n)
}
plot(P, Pa_hiper, type="l", lwd=2,
     xlab="Proporción de defectos (Calidad de entrada)",
     ylab="Pa",
     main="Curva CO - Comparación")

lines(P, Pa_binom, col="blue", lwd=2)
lines(P, Pa_poisson, col="red", lwd=2)

legend("topright",
       legend=c("Hipergeométrica","Binomial","Poisson"),
       col=c("black","blue","red"),
       lwd=2)

N <- 2000 ; n <- 50 ; c <- 2
P <- seq(0.000 , 0.200, 0.001)
Pa_binom <- pbinom(c, n, P, lower.tail=T)
Pa_poisson <- ppois(c, lambda = n*P)
Pa_hiper <- numeric(length(P))

for(i in 1:length(P)){
  D <- round(N * P[i])
  Pa_hiper[i] <- phyper(c, D, N-D, n)
}
plot(P, Pa_hiper, type="l", lwd=2,
     xlab="Proporción de defectos (Calidad de entrada)",
     ylab="Pa",
     main="Curva CO - Comparación")

lines(P, Pa_binom, col="blue", lwd=2)
lines(P, Pa_poisson, col="red", lwd=2)

legend("topright",
       legend=c("Hipergeométrica","Binomial","Poisson"),
       col=c("black","blue","red"),
       lwd=2)


