
###################################
# Calculamos la varianza real de las mediciones realizadas por los ítems 5 y 6
var_real_items <- apply(data[, c("item5", "item6")], 2, var)

# Inicializamos vectores para almacenar los resultados
n_items <- c(6, 9)
var_dif_total <- numeric(length(n_items))

# Calculamos la varianza de las diferencias entre los dos métodos de medición (σ∆^2) para cada cantidad de ítems
for (i in 1:length(n_items)) {
  ni <- n_items[i]
  var_dif_total[i] <- var_real_items[1] / ni + (var_real_items[1] / ni + var_real_items[2] / ni) / ni
}

# Mostramos los resultados
resultados <- data.frame(Número_de_Ítems = n_items, Varianza_Diferencias = var_dif_total)
print(resultados)


###Varianza

var(data$item5)
var(data$item6)


########################

# Paso 1: Identificar las fuentes de variación
# Ya tienes las personas representadas en las filas y los ítems en las columnas

# Paso 2: Calcular las varianzas del estudio G
# Calcular las medias para cada persona y cada ítem
media_personas <- rowMeans(data[, c("item5", "item6")])
media_items <- colMeans(data[, c("item5", "item6")])

# Calcular las sumas de cuadrados
SS_personas <- sum((data[, c("item5", "item6")] - media_personas)^2)
SS_items <- sum((t(data[, c("item5", "item6")]) - media_items)^2)

# Calcular los grados de libertad
df_personas <- nrow(data[, c("item5", "item6")]) - 1
df_items <- ncol(data[, c("item5", "item6")]) - 1

# Estimar las varianzas
var_personas <- SS_personas / df_personas
var_items <- SS_items / df_items

# Paso 3: Calcular las varianzas del error para decisiones relativas y absolutas
# Calcular la varianza del error para decisiones relativas
var_error_relativas <- (var_personas + var_items) / 2

# Calcular la varianza del error para decisiones absolutas
var_error_absolutas <- (var_personas / ncol(data[, c("item5", "item6")])) + var_error_relativas

# Paso 4: Calcular coeficientes de generalizabilidad y dependibilidad
# Calcular E_rho^2 para decisiones relativas
E_rho_cuadrado <- var_personas / (var_personas + var_error_relativas)

# Calcular phi para decisiones absolutas
phi <- var_personas / (var_personas + var_error_absolutas)

# Paso 5: Interpretar los resultados
# Puedes imprimir los coeficientes para ver los resultados
print(E_rho_cuadrado)
print(phi)






################################

# Inicializamos vectores para almacenar los resultados
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
  
  # Varianza de la interacción persona-ítem (pi)
  var_interaccion[i] <- var(unlist(data[, c("item5", "item6")])) - var_personas[i] - var_items[i]
  
  # Varianza del error (pi,e)
  var_error[i] <- mean(apply(data[, c("item5", "item6")], 1, var))
  
  # Varianza total del error
  var_total_error[i] <- var_error[i] * ni
}

# Mostramos los resultados
resultados <- data.frame(
  n_items = 1:2,
  var_personas = var_personas,
  var_items = var_items,
  var_interaccion = var_interaccion,
  var_error = var_error,
  var_total_error = var_total_error
)

print(resultados)

###############

