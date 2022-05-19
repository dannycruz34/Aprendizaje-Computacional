library(mclust)
library(cluster)

datos = read.table("/Users/DanniC/Desktop/Datasets/Dermatology/Dermatology.txt", head=TRUE) # load the dataset
#d_euc <- dist(datos[,2-35], method="euclidean") # compute the distance
d_euc <- dist(datos[,2-35], method="manhattan") # compute the distance

# CC_euc <- CrossMod(d_euc,k.w.min=2,k.w.max=19,k.c.max=20,out=T) # clustering with CC
# CC_euc

#CROSSCLUSTERING ORIGINAL
CC_euc <- cc_crossclustering(d_euc,k_w_min=2,k_w_max=19,k2_max=20,out=T,method='complete') # clustering with CC
CC_euc$vectorFinal

for(c in 1:nrow(datos)){
  datos[c,36]= paste("Cluster",CC_euc$vectorFinal[c])
}

print("ARI: ")
adjustedRandIndex(unname(unlist(datos[,1])), unname(unlist(datos[,36])))
