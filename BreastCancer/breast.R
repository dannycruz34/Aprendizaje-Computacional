library(mclust)
library(cluster)

datos = read.table("/Users/DanniC/Desktop/Datasets/BreastCancer/breastF.txt", head=FALSE) # load the dataset
#d_euc <- dist(datos[,1:29], method="euclidean") # compute the distance
d_euc <- dist(datos[,1:29], method="manhattan") # compute the distance

# CC_euc <- CrossMod(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=T) # clustering with CC
# CC_euc
#
# for(c in 1:nrow(datos)){
#   datos[c,32]= paste("Cluster",CC_euc$vectorFinal[c])
# }
#
# print("ARI: ")
# adjustedRandIndex(unname(unlist(datos[,31])), unname(unlist(datos[,32])))


#CROSSCLUSTERING ORIGINAL
CC_euc <- cc_crossclustering(d_euc,k_w_min=2,k_w_max=19,k2_max=20,out=T,method='complete') # clustering with CC

CC_euc$vectorFinal
#Resultados obtenidos
for(c in 1:nrow(datos)){
  datos[c,32]= paste("Cluster",CC_euc$vectorFinal[c])
}

print("ARI: ")
adjustedRandIndex(unname(unlist(datos[,31])), unname(unlist(datos[,32])))
