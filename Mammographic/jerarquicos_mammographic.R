rm(list=ls())
# NUESTRO CODIGO
library(mclust)
library(cluster)

datos = read.table("/Users/DanniC/Desktop/Datasets/Mammographic/mammographic_masses.txt",header = F)
datos2 = datos[,2:6]

d_euc <- dist(datos2, method="euclidean") # compute the distance

#K-MEANS
maxARIK=0
maxASWK=0
indKMEANS=0
res <- kmeans(datos2, 2)
maxARIK <- adjustedRandIndex(unlist(datos[,1]),unlist(res$cluster))
indKMEANS=2

for (j in 2:20) {
  res <- kmeans(datos2, j)
  ariK <- adjustedRandIndex(unlist(datos[,1]),unlist(res$cluster))
  aswK = mean(silhouette(as.numeric(res$cluster), dist = d_euc)[,3])

  if(aswK >= maxASWK){
    maxARIK = ariK
    indKMEANS = j
    maxASWK = aswK
  }
}

#DENDROGRAMAS1
clu.ward <- hclust(d_euc, method = "ward.D")
clu.complete <- hclust(d_euc, method = "complete")
clu.single <- hclust(d_euc, method = "single")

maxARIW = 0
maxARIC = 0
maxARIS = 0

maxASWW = 0
maxASWC = 0
maxASWS = 0

indMaxW = 0
indMaxC = 0
indMaxS = 0

#INICIALIZACION MAX ARI
clusterCutW <- cutree(clu.ward , 2)
clusterCutC <- cutree(clu.complete , 2)
clusterCutS <- cutree(clu.single , 2)

maxARIW = adjustedRandIndex(unlist(datos[,1]),unlist(clusterCutW))
maxARIC = adjustedRandIndex(unlist(datos[,1]),unlist(clusterCutC))
maxARIS = adjustedRandIndex(unlist(datos[,1]),unlist(clusterCutS))

maxASWW = mean(silhouette(as.numeric(clusterCutW), dist = d_euc)[,3])
maxASWC = mean(silhouette(as.numeric(clusterCutC), dist = d_euc)[,3])
maxASWS = mean(silhouette(as.numeric(clusterCutS), dist = d_euc)[,3])

print("Ward 2")
print(maxARIW)
print(maxARIC)
print(maxARIS)

#CORTE WARD
for(i in 2:20){
  clusterCutW <- cutree(clu.ward , i)
  clusterCutC <- cutree(clu.complete , i)
  clusterCutS <- cutree(clu.single , i)

  #ARI WARD
  ari1 <- adjustedRandIndex(unlist(datos[,1]),unlist(clusterCutW))
  asw1 <- mean(silhouette(as.numeric(clusterCutW), dist = d_euc)[,3])
  print(ari1)

  if(asw1 >= maxASWW){
    maxARIW = ari1
    maxASWW = asw1
    indMaxW = i
  }

  #ARI COMPLETE LINKAGE
  ari2 <- adjustedRandIndex(unlist(datos[,1]),unlist(clusterCutC))
  asw2 <- mean(silhouette(as.numeric(clusterCutC), dist = d_euc)[,3])
  print(ari2)

  if(asw2 >= maxASWC){
    maxARIC = ari2
    maxASWC = asw2
    indMaxC = i
  }

  #ARI SINGLE LINKAGE
  ari3 <- adjustedRandIndex(unlist(datos[,1]),unlist(clusterCutS))
  asw3 <- mean(silhouette(as.numeric(clusterCutS), dist = d_euc)[,3])
  print(ari3)

  if(asw3 >= maxASWS){
    maxARIS = ari3
    maxASWS = asw3
    indMaxS = i
  }

}


print("RESULTADOS")
print("ARI Ward + Nivel")
print(maxASWW)
print(maxARIW)
print(indMaxW)

print("ARI Complete + Nivel")
print(maxASWC)
print(maxARIC)
print(indMaxC)

print("ARI Single + Nivel")
print(maxASWS)
print(maxARIS)
print(indMaxS)

print("----K-MEANS----")
print(indKMEANS)
print(maxARIK)
