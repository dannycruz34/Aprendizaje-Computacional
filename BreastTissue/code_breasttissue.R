library(mclust)
library(cluster)

datos = read.table("/Users/DanniC/Desktop/Datasets/BreastTissue/breasttissue.txt", head=TRUE) # load the dataset
#datos = datos[sample(nrow(datos), 5000), ]
#d_euc <- dist(datos[,2:10], method="euclidean") # compute the distance
d_euc <- dist(datos[,2:10], method="manhattan") # compute the distance

# CC_euc <- CrossMod(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=T) # clustering with CC
# CC_euc
#
# for(c in 1:nrow(datos)){
#   datos[c,11]= paste("Cluster",CC_euc$vectorFinal[c])
# }
#
# print("ARI: ")
# adjustedRandIndex(unname(unlist(datos[,1])), unname(unlist(datos[,11])))


#CROSSCLUSTERING ORIGINAL
CC_euc <- cc_crossclustering(d_euc,k_w_min=2,k_w_max=19,k2_max=20,out=T,method='complete') # clustering with CC

CC_euc$vectorFinal
#Resultados obtenidos
for(c in 1:nrow(datos)){
  datos[c,11]= paste("Cluster",CC_euc$vectorFinal[c])
}

print("ARI: ")
adjustedRandIndex(unname(unlist(datos[,1])), unname(unlist(datos[,11])))
