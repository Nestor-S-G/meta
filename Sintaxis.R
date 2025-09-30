library(VGAM)
library(lavaan)
library(psych)
library(semTools)

data <- read.table('datos.dat', header = TRUE)

#1---------------------
m1 <- vglm(GRUPO ~ item1 + item2 + item3 + item4 + item5 + item6 + item7, data=data, family = cumulative(link = 'logit', parallel = T))
summary(m1)


#2-------------------

mod <- ' fisica  =~ item1 + item2 + item3 + item4
         cognitiva =~ item5 + item6 + item7
          fisica ~~ cognitiva'

# Ejecutar el análisis factorial confirmatorio usando WLSMV como método de estimación
fit <- cfa(mod, data = data, ordered = names(data)[4:10], estimator = "WLSMV")

# Mostrar resultados, incluyendo medidas de ajuste del modelo
summary(fit, fit.measures = TRUE, standardized = TRUE)

#3------------------

#alfa
data$sumafi <- rowSums(data[4:7])
data$sumafiSq <- data$sumafi**2
num_alfa_fi <- var(data[,4])+var(data[,5])+var(data[,6])+var(data[,7])
den_alfa_fi <- sum(data$sumafiSq/574)-mean(data$sumafi)**2
alfa_fi <- (4/3)*(num_alfa_fi/den_alfa_fi)
alfa_fi

data$sumacog <- rowSums(data[8:10])
data$sumacogSq <- data$sumacog**2
num_alfa_co <- var(data[,8])+var(data[,9])+var(data[,10])
den_alfa_co <- sum(data$sumacogSq/nrow(data))-mean(data$sumacog)**2
alfa_cog <- (3/2)*(num_alfa_co/den_alfa_co)
alfa_cog

#alfasexos

data_hom <- subset(data, GENERO == 1)
num_alfa_fi_hom <- var(data_hom[,4])+var(data_hom[,5])+var(data_hom[,6])+var(data_hom[,7])
den_alfa_fi_hom <- sum(data_hom$sumafiSq/nrow(data_hom))-mean(data_hom$sumafi)**2
alfa_fi_hom <- (4/3)*(num_alfa_fi_hom/den_alfa_fi_hom)
alfa_fi_hom

num_alfa_cog_hom <- var(data_hom[,8])+var(data_hom[,9])+var(data_hom[,10])
den_alfa_cog_hom <- sum(data_hom$sumacogSq/nrow(data_hom))-mean(data_hom$sumacog)**2
alfa_cog_hom <- (4/3)*(num_alfa_cog_hom/den_alfa_cog_hom)
alfa_cog_hom

data_muj <- subset(data, GENERO == 2)
num_alfa_fi_muj <- var(data_muj[,4])+var(data_muj[,5])+var(data_muj[,6])+var(data_muj[,7])
den_alfa_fi_muj <- sum(data_muj$sumafiSq/nrow(data_muj))-mean(data_muj$sumafi)**2
alfa_fi_muj <- (4/3)*(num_alfa_fi_muj/den_alfa_fi_muj)
alfa_fi_muj

num_alfa_cog_muj <- var(data_muj[,8])+var(data_muj[,9])+var(data_muj[,10])
den_alfa_cog_muj <- sum(data_muj$sumacogSq/nrow(data_muj))-mean(data_muj$sumacog)**2
alfa_cog_muj <- (4/3)*(num_alfa_cog_muj/den_alfa_cog_muj)
alfa_cog_muj


#omega

model <- '
  # Especificar los factores
  física =~ item1 + item2 + item3 + item4
  cognitiva =~ item5 + item6 + item7
  #correlacion entre factores
  física ~~ cognitiva
'
fit <- cfa(model, data)

summary(fit, fit.measures=TRUE)

# Modelo del AFC
model <- '
  Física =~ item1 + item2 + item3 + item4
  Cognitiva =~ item5 + item6 + item7
'

# Ajustar el modelo a tus datos
fit <- cfa(model, data, std.lv = TRUE)

# Obtener estimaciones de parámetros
params <- parameterEstimates(fit, standardized = TRUE)
print(params)

#hombres
fit_h <- cfa(model, data_hom, std.lv = TRUE)

# Obtener estimaciones de parámetros
params_h <- parameterEstimates(fit_h, standardized = TRUE)
print(params_h)

#mujeres

fit_m <- cfa(model, data_muj, std.lv = TRUE)

# Obtener estimaciones de parámetros
params_m <- parameterEstimates(fit_m, standardized = TRUE)
print(params_m)


#4------------

model_bifactor <- '
  # Factor general
  G =~ item1 + item2 + item3 + item4 + item5 + item6 + item7
  
  # Factores específicos
  Física =~ item1 + item2 + item3 + item4
  Cognitiva =~ item5 + item6 + item7
  
  # Configurando ortogonalidad entre los factores específicos y el general
  G ~~ 0*Física
  G ~~ 0*Cognitiva
  Física ~~ 0*Cognitiva
'
fit_bifactor <- cfa(model_bifactor, data, std.lv=TRUE)

summary(fit_bifactor, fit.measures=TRUE, standardized=TRUE)

params_bifactor <- parameterEstimates(fit_bifactor, standardized=TRUE)
print(params_bifactor)

#Hombres

fit_bifactor_hombres <- cfa(model_bifactor, data_hom, std.lv=TRUE)

summary(fit_bifactor_hombres, fit.measures=TRUE, standardized=TRUE)

params_bifactor_hombres <- parameterEstimates(fit_bifactor_hombres, standardized=TRUE)
print(params_bifactor_hombres)

#Mujeres
fit_bifactor_mujeres <- cfa(model_bifactor, data_muj, std.lv=TRUE)

summary(fit_bifactor_mujeres, fit.measures=TRUE, standardized=TRUE)

params_bifactor_mujeres <- parameterEstimates(fit_bifactor_mujeres, standardized=TRUE)
print(params_bifactor_mujeres)


#5---------------
dataitems <- data.frame(data$item1, data$item2, data$item3, data$item4, data$item5, data$item6, data$item7)
describe(dataitems)

polychoric(dataitems)

cor(dataitems, y=NULL, use="complete.obs", method=c("pearson"))

cov(dataitems, y=NULL, use="complete.obs", method=c("pearson"))

skew(dataitems)
kurtosi(dataitems)
scree(dataitems)

polymatrix <- polychoric(dataitems)

alpha(polymatrix$rho)

alpha(dataitems)

fa(dataitems, nfactors = 2)

fa(polymatrix$rho)

guttman(polymatrix$rho)

guttman(dataitems)

omega(polymatrix$rho)

omega(dataitems)

#hombres

dataitems_hom <- data.frame(data_hom$item1, data_hom$item2, data_hom$item3, data_hom$item4, data_hom$item5, data_hom$item6, data_hom$item7)
describe(dataitems_hom)

polychoric(dataitems_hom)

cor(dataitems_hom, y=NULL, use="complete.obs", method=c("pearson"))

cov(dataitems_hom, y=NULL, use="complete.obs", method=c("pearson"))

skew(dataitems_hom)
kurtosi(dataitems_hom)
scree(dataitems_hom)

polymatrix_hom <- polychoric(dataitems_hom)

alpha(polymatrix_hom$rho)

alpha(data_items_hom)

fa(dataitems_hom, nfactors = 2)

fa(polymatrix_hom$rho)

guttman(polymatrix_hom$rho)

guttman(dataitems_hom)

omega(polymatrix_hom$rho)

omega(dataitems_hom)

#mujeres

dataitems_muj <- data.frame(data_muj$item1, data_muj$item2, data_muj$item3, data_muj$item4, data_muj$item5, data_muj$item6, data_muj$item7)
describe(dataitems_muj)

dataitems_muj

polychoric(dataitems_muj)

cor(dataitems_muj, y=NULL, use="complete.obs", method=c("pearson"))

cov(dataitems_muj, y=NULL, use="complete.obs", method=c("pearson"))

skew(dataitems_muj)
kurtosi(dataitems_muj)
scree(dataitems_muj)

polymatrix_muj <- polychoric(dataitems_muj)

alpha(polymatrix_muj$rho)

alpha(dataitems_muj)

fa(dataitems_muj, nfactors = 2)

fa(polymatrix_muj$rho)

guttman(polymatrix_muj$rho)

guttman(dataitems_muj)

omega(polymatrix_muj$rho)

omega(dataitems_muj)


#6-----------------------------------
dataitemsAVE <- data[, c("item1", "item2", "item3", "item4", "item5", "item6", "item7")]

# Perform factor analysis
factor_analysis <- fa(dataitemsAVE, nfactors = 2, rotate = "varimax") 

# Print factor loadings
print(factor_analysis$loadings)

AVEf = (0.371*0.371+0.514*0.514+0.836*0.836+0.668*0.668)/4
AVEf
AVEc = (0.379*0.379+0.724*0.724+0.775*0.775)/3
AVEc

sqrt(AVEf)
sqrt(AVEc)

correlation_matrix <- cor(cbind(factor1_loadings, factor2_loadings))

#7-------------------

var_personas <- numeric(2)
var_items <- numeric(2)
var_interaccion <- numeric(2)
var_error <- numeric(2)
var_total_error <- numeric(2)

# Calculamos las varianzas
for (i in 1:2) {
  ni <- i + 4
  
  # Varianza de las personas (p)
  var_personas[i] <- var(rowMeans(data[, c("item5", "item6")]))
  
  # Varianza de los ítems (i)
  var_items[i] <- var(colMeans(data[, c("item5", "item6")]))
  
  # Varianza de la interacción persona-ítem (pi,e)
  var_interaccion[i] <- var(unlist(data[, c("item5", "item6")])) - var_personas[i] - var_items[i]
  
}

# Mostramos los resultados
resultados <- data.frame(
  n_items = 1:2,
  var_personas = var_personas,
  var_items = var_items,
  var_interaccion = var_interaccion
)

print(resultados)