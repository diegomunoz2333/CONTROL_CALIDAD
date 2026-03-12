library(AQLSchemes) 
library(AcceptanceSampling)

planS_1<-AASingle('Normal')
planS_1
vectorS_1 <- seq(0, 0.20, by = 0.005)
planPS_1 <- OC2c(n = 50, c = 3, type = "binomial", pd = vectorS_1)
print(planPS_1)
pa_1<-planPS_1@paccept
pd<-planPS_1@pd
plot(pd, pa_1, 
     type = 'l',          
     col = "blue",         
     lwd = 2,              
     xlab = "Proporción defectuosa (pd)",
     ylab = "P(aceptación)",
     main = "Curva Característica de Operación (CO)")


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