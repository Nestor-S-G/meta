library(metafor)
library(boot)


#Para instalar el paquete dmetar, ejecutar este codigo:

if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("MathiasHarrer/dmetar")

library(dmetar)

#Para este ejemplo, vamos a utilizar la base de datos dat.raudenbush1985, que ya viene integrada en el paquete de metafor

#Imprimimos las primeras observaciones
head(dat.raudenbush1985)

#yi = d de Cohen; vi = varianza muestral de la d de Cohen; weeks: semanas de contacto antes de la induccion de la expectativa;
#setting: si las pruebas fueron administradas en grupo o individualmente;
#tester: si el administrador de la prueba era consciente (aware) o ciego (blind).



# Aplicamos un modelo de efectos aleatorios
res0<-(rma.uni(yi , vi, method = "REML", data=dat.raudenbush1985))


###################    ***   Analisis de moderadores: Una variable categorica    ***   #####################

# 1. Analisis de moderadores: TESTER
res_mod <- rma.uni(yi ~ tester, vi , method = "REML", data=dat.raudenbush1985)
res_mod  

anova(res0,res_mod)
#Mismos analisis, pero eliminando el intercept para poder obtener el tamano del efecto combinado en ambas categor?as
summary(rma.uni(yi ~ tester -1 , vi , method = "REML", data=dat.raudenbush1985))


# Introducimos artificalmente una nueva categoria para haber como se analizaría cuando hay más de dos categorías
data<-dat.raudenbush1985
data$tester[c(1:3, 7, 8)] <- "Unknown"

summary(rma.uni(yi ~ tester , vi , method = "REML", data=data))

#Cambiamos la categoría de referencia para que sea blind
data$tester <- relevel(factor(data$tester), ref="blind")
summary(rma.uni(yi ~ tester , vi , method = "REML", data=data))


# 3. R2 y los intervalos de confianza a través del bootstrap
#R2
(1-(res_mod0$tau2/res0$tau2))*100

#Creamos la función que se va a utilizar para general los intervalos de confianza
boot.func <- function(dat, indices, formula) {
  res <- try(suppressWarnings(rma(yi, vi, mods = formula, data=dat[indices,])), silent=TRUE)
  if (inherits(res, "try-error")) NA else res$R2
}

#Una vez creada la funcion, en el codigo de abajo solo hay que sustituir data (nombre de la base de datos)
#y res1 por el objeto donde hayas guardado los resultados

set.seed(1234)
res.boot <- boot(dat.raudenbush1985, boot.func, R=10000, formula=formula(res_mod))

#Por ultimo, obtenemos los resultados con varios tipos de intervalos de confianza
boot.ci(res.boot, type=c("norm", "basic", "perc", "bca"))


#4.Para poder hacer una prueba de razon de verosimilitudes, es necesario ajustar el modelo con ML en lugar de con REML
res0 <-rma.uni(yi , vi, method = "ML", data=dat.raudenbush1985)
res1 <- rma.uni(yi ~ tester, vi , method = "ML", data=dat.raudenbush1985)

anova(res0,res1)



###################    ***   Analisis de moderadores: Una variable continua    ***   #####################

# Analisis de moderadores: WEEKS

#1.Ejecutamos el analisis de moderadores
res_mod_cont <- rma.uni(yi ~ weeks, vi , method = "REML", data=dat.raudenbush1985)
res_mod_cont  

#Primero centramos la variable week
dat.raudenbush1985$c_weeks<-scale(dat.raudenbush1985$weeks, center = TRUE, scale = FALSE)
dat.raudenbush1985$c_weeks2<-dat.raudenbush1985$weeks - mean(dat.raudenbush1985$weeks)

#Ejecutamos el analisis de moderadores
res_mod_cont <- rma.uni(yi ~ c_weeks, vi , method = "REML", data=dat.raudenbush1985)
res_mod_cont  

regplot(res_mod_cont, xlab="Weeks (centered)", refline=0,
        atransf=transf.ztor, digits=1, las=1, bty="l",
        labsize=0.9)

# R2 y los inetrvalos de confianza a través del bootstrap
#R2
(1-(res_mod_cont$tau2/res0$tau2))*100


boot.func <- function(dat, indices, formula) {
  res <- try(suppressWarnings(rma(yi, vi, mods = formula, data=dat[indices,])), silent=TRUE)
  if (inherits(res, "try-error")) NA else res$R2
}

#Una vez creada la funcion, en el codigo de abajo solo hay que sustituir data (nombre de la base de datos)
#y res1 por el objeto donde hayas guardado los resultados

set.seed(1234)
res.boot <- boot(dat.raudenbush1985, boot.func, R=10000, formula=formula(res_mod_cont))

#Por ultimo, obtenemos los resultados con varios tipos de intervalos de confianza
boot.ci(res.boot, type=c("norm", "basic", "perc", "bca"))


# Prueba de razon de verosimilitudes 
res0<-rma.uni(yi ~ 1, vi , method = "ML", data=dat.raudenbush1985)
res1<-rma.uni(yi ~ c_weeks , vi , method = "ML", data=dat.raudenbush1985)
anova(res0, res1)