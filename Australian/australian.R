rm(list=ls())
# NUESTRO CODIGO

library(mclust)
library(cluster)


datos = read.table("/Users/DanniC/Desktop/Datasets/Australian/australianAll.txt",header = F)
datos2 = datos[,2:15]

#d_euc <- dist(datos2, method="euclidean") # compute the distance
d_euc <- dist(datos2, method="manhattan") # compute the distance

#CODIGO CROSSCLUSTERING MODIFICADO
# #CC_euc <- CrossMod(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=T) # clustering with CC
#
# CC_euc # the resultsli
#
# #Resultados obtenidos
# for(i in 1:nrow(datos)){
#   datos[i,16] <- paste(toString(CC_euc$vectorFinal[i]))
# }
#
# ari <- adjustedRandIndex(unlist(datos[,1]),unlist(datos[,16]))
# ari

#CROSSCLUSTERING ORIGINAL
CC_euc <- cc_crossclustering(d_euc,k_w_min=2,k_w_max=19,k2_max=20,out=T,method='complete') # clustering with CC

CC_euc$vectorFinal
#Resultados obtenidos
for(i in 1:nrow(datos)){
   datos[i,16] <- paste(toString(CC_euc$vectorFinal[i]))
}

ari <- adjustedRandIndex(unlist(datos[,1]),unlist(datos[,16]))
ari

