library(metafor)
library(readxl)

# Primero, importamos el Excel llamado datos_Hoffman.xlsx. Para ello podemos utilizar el comando "import dataset" de la derecha


#Para realizar el meta-an?lisis de efectos fijos, podemos utilizar la funci?n rma.uni

resultado <- rma.uni(yi = T, vi = Vd, method = "FE", data=Tarea3)
resultado                           

resultado <- rma.uni(yi = T, vi = Vd, method = "DL", data=Tarea3)
resultado   

#FE = fixed-effect = efecto fijo
