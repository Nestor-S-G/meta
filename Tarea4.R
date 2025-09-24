library(metafor)
library(boot)

res0 <- rma.uni(yi = T, vi = Vd, method = "REML", data=Tarea3)


# 1 y 2 QM
res_mod <- rma.uni(yi = T ~ tipo_hospital, vi = Vd, method = "REML", data=Tarea3)
res_mod

#Eliminando la interseccion
summary(rma.uni(yi = T ~ tipo_hospital -1 , vi = Vd, method = "REML", data=Tarea3))

#4

res0 <-rma.uni(yi = T, vi = Vd, method = "ML", data=Tarea3)
res1 <- rma.uni(yi = T ~ tipo_hospital, vi = Vd, method = "ML", data=Tarea3)

anova(res0,res1)

#5 QE


#EDAD

#1 y 2

res_mod_cont <- rma.uni(yi = T ~ edad_media, vi = Vd, method = "REML", data=Tarea3)
res_mod_cont

Tarea3$c_edad_media<-scale(Tarea3$edad_media, center = TRUE, scale = FALSE)
Tarea3$c_edad_media2<-Tarea3$edad_media - mean(Tarea3$edad_media)

res_mod_cont <- rma.uni(yi = T ~ c_edad_media, vi = Vd, method = "REML", data=Tarea3)
res_mod_cont


#4

res0 <-rma.uni(yi = T, vi = Vd, method = "ML", data=Tarea3)
res1 <- rma.uni(yi = T ~ c_edad_media, vi = Vd, method = "ML", data=Tarea3)

anova(res0,res1)
