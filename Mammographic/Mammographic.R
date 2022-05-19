rm(list=ls())
# NUESTRO CODIGO
library(mclust)
library(cluster)

datos = read.table("/Users/DanniC/Desktop/Datasets/Mammographic/mammographic_masses.txt",header = F)
datos2 = datos[,2:6]

#d_euc <- dist(datos2, method="euclidean") # compute the distance
d_euc <- dist(datos2, method="manhattan") # compute the distance

# CC_euc <- CrossMod(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=T) # clustering with CC
# CC_euc # the resultsli

#CROSSCLUSTERING ORIGINAL
CC_euc <- cc_crossclustering(d_euc,k_w_min=2,k_w_max=19,k2_max=20,out=T,method='complete') # clustering with CC
CC_euc$vectorFinal

#Resultados obtenidos
for(i in 1:nrow(datos)){
  datos[i,7] <- paste(toString(CC_euc$vectorFinal[i]))
}


ari <- adjustedRandIndex(unlist(datos[,1]),unlist(datos[,7]))
ari




