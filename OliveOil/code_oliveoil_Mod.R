rm(list=ls())
library(pdfCluster)
library(cluster)
library(mclust)


data(oliveoil)
datos = oliveoil[,2:10]
datos2 = datos[,2:9]

d_euc <- dist(datos2, method="euclidean") # compute the distance
CC_euc <- CrossMod(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=F) # clustering with CC
CC_euc
# the results

#Resultados obtenidos
for(i in 1:nrow(datos)){
  datos[i,10] <- paste(toString(CC_euc$vectorFinal[i]))
}

ari <- adjustedRandIndex(unlist(datos[,1]),unlist(datos[,10]))
ari
