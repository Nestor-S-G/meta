library(metafor)
library(boot)


#1
res <-  rma.uni(yi = T, vi = Vd, method = "REML", data=Tarea3)
funnel(res, addtau2 = T)


#2

