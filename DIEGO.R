# Parámetros
N <- 100
n <- 50
c <- 2

# Vector de proporciones defectuosas
P <- seq(0.001, 0.20, 0.001)

# 1. Binomial
Pa_bin <- pbinom(c, n, P)

# 2. Poisson
Pa_pois <- ppois(c, n*P)

# 3. Hipergeométrica
Pa_hiper <- sapply(P, function(p){
  D <- round(N*p)
  phyper(c, D, N-D, n)
})

# Gráfica
plot(P, Pa_hiper, type="l", lwd=2,
     ylab="Pa", xlab="Proporción defectuosa",
     main="Curva CO - N=100")

lines(P, Pa_bin, lwd=2, lty=2)
lines(P, Pa_pois, lwd=2, lty=3)

legend("top right",
       legend=c("Hipergeométrica","Binomial","Poisson"),
       lwd=2, lty=c(1,2,3))


N <- 2000

Pa_hiper2 <- sapply(P, function(p){
  D <- round(N*p)
  phyper(c, D, N-D, n)
})

plot(P, Pa_hiper2, type="l", lwd=2,
     ylab="Pa", xlab="Proporción defectuosa",
     main="Curva CO - N=2000")

lines(P, Pa_bin, lwd=2, lty=2)
lines(P, Pa_pois, lwd=2, lty=3)

legend("topright",
       legend=c("Binomial","Poisson","Hipergeométrica"),
       lty=1)
