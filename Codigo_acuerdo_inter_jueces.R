library(irr)


###################### Variables categ?ricas ############################

#Las siguientes puntuaciones corresponden a las categor?as que han asignado 3 jueces a 25 estudios en cuanto a su diseno
# 0: estudio longitudinal
# 1: quasi-experimental
# 2: experimental

Juez1 <- c(2,0,1,2,0,2,1,0,1,1,1,2,2,1,
              0,0,2,1,1,2,1,0,1,2,1)
Juez2 <- c(2,0,1,1,0,0,1,0,1,0,1,2,0,0,
              0,0,2,1,1,0,0,0,1,2,1)
Juez3 <- c(2,0,1,2,0,2,0,0,0,2,1,2,1,0,
              0,1,0,1,1,0,1,0,1,2,1)

tres_categorico<-cbind(Juez1,Juez2, Juez3)

#Echamos un vistazo a la tabla de contigencia
table(Juez1, Juez2)
table(Juez1, Juez3)
table(Juez2, Juez3)


# Porcentaje de acuerdo
agree(tres_categorico)

#Kappa de Cohen: solo posible para dos jueces
dos_categorico1<-cbind(Juez1,Juez2)
kappa2(dos_categorico1)


dos_categorico2<-cbind(Juez1,Juez3)
kappa2(dos_categorico2)


dos_categorico3<-cbind(Juez2,Juez3)
kappa2(dos_categorico3)


#Kappa de Cohen: Promedio de varios kappas de Cohen entre varios jueces
kappam.light(tres_categorico)


#Kappa de Cohen: posibilita introducir 3 jueces
kappam.fleiss(tres_categorico)


###################### Variables continuas ############################

#Imaginad que los valores representan la media que se ha observado en el pre-test de la variable depresi?n en el grupo que
# va a recibir la intervenci?n en 25 estudios

juez1 <- c(9.8, 10.2, 11.0 ,6.1,  8.2,  8.4,  9.2, 11.2,  9.2,  9.0, 12.5,
           8.1, 9.5, 10.3, 6.3, 10.9, 11.4,  8.7, 12.1,  9.9,  8.9, 7.1, 10.5, 10.7, 12.3)
juez2 <- c(9.2, 10.2, 11.98 , 4.7,  8.2,  8.4,  9.2, 11.2,  8.2,  9.0, 12.5,
           8.1, 9.5, 10.3, 6.3, 10.9, 11.4,  7, 12.1,  9.9,  8.9, 7.1, 10.5, 10.7, 11.3)
juez3 <- c(9.8, 10.2, 13.0, 6.1,  8.2,  8.4,  9.2, 11.2,  9.2,  9.0, 12.5,
            8.1, 8.5, 10.3, 6.3, 10.9, 9.4,  8.7, 12.1,  9.9,  8.9, 7.1, 10.5, 10.7, 11.3)

#Correlaci?n intrajueces
cor(juez1,juez2)
cor(juez2,juez3)
cor(juez1,juez3)

#Correlaci?n intraclase

tres_continuo<-cbind(juez1, juez2, juez3)
icc(tres_continuo)



