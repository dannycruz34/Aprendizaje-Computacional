CrossMod<-function (d, k.w.min = 2, k.w.max, k.c.max, out = TRUE)
{
    library(cluster)
    n <- (1 + sqrt(1 + 8 * length(d)))/2
    beta.clu.ward <- hclust(d, method = "ward.D")
    beta.clu.complete <- hclust(d, method = "single")
    grid <- as.matrix(expand.grid(k.w.min:k.w.max, k.w.min:k.c.max))
    if (out == T)
        grid <- grid[grid[, 2] > grid[, 1], ]
    else grid <- grid[grid[, 2] >= grid[, 1], ]
    grid <- cbind(grid, 0)
    colnames(grid) <- c("Ward", "Single", "N. classified")
    n.clu <- NULL
    for (i in 1:dim(grid)[1]) {
        n.clu[i] = 0
    }
    t <- proc.time()
    for (i in 1:dim(grid)[1]) {
        n.clu[i] <- proportion.function(grid[i, ], beta.clu.ward = beta.clu.ward,
            beta.clu.complete = beta.clu.complete)
        if (n.clu[i] > 1 * n)
            (break)()
    }
    grid[, 3] <- n.clu
    print(proc.time() - t)
    grid.star <- which(grid == grid[, 3], arr.ind = TRUE)[, 1]
    k.star <- rbind(grid[grid.star, 1:2])
    if (is.null(dim(k.star))) {
        cluster.list <- proportion.function(k.star, beta.clu.ward = beta.clu.ward,
            beta.clu.complete = beta.clu.complete, return.list = T)
        clustz <- sapply(1:n, geneinlista, cluster.list$beta.list)
    }
    else {
        cluster.list <- apply(k.star, 1, proportion.function,
            beta.clu.ward = beta.clu.ward, beta.clu.complete = beta.clu.complete,
            return.list = T)
        clustz <- sapply(cluster.list, function(lasim) sapply(1:n,
            geneinlista, lista = lasim$beta.list))
    }
    clustz[clustz == "integer(0)"] = 0
    if (is.null(dim(clustz))) {
        clustz <- matrix(clustz, ncol = 1)
    }
    cont = 0
    Sil <- list()
    Sil[1] <- mean(silhouette(as.numeric(clustz[, 1]), dist = d)[,
        3])
    anterior = Sil[1]
    for (c in 2:ncol(clustz)) {
        cont = cont + 1
        print(cont)
        Sil[c] <- mean(silhouette(as.numeric(clustz[, c]), dist = d)[,
            3])
        if (unlist(Sil[c]) < anterior) {
            (break)()
        }
        anterior = Sil[c]
    }
    print(Sil)
    if (is.null(dim(k.star))) {
        k.star.star <- k.star[which.max(Sil)]
    }
    else {
        k.star.star <- k.star[which.max(Sil), ]
    }
    print("ES EL DE LA CARPETA")
    vectorFinal = clustz[, which.max(Sil)]
    vectorFinal = unlist(vectorFinal)
    vectorFinal = unname(vectorFinal)
    Cluster.list <- cluster.list[[which.max(Sil)]]$beta.list
    n.clustered <- length(unlist(Cluster.list))
    return(list(Optimal.cluster = length(cluster.list[[which.max(Sil)]]$beta.list),
        Cluster.list = Cluster.list, Silhouette = max(unlist(Sil),
            na.rm = FALSE), n.total = n, n.clustered = n.clustered,
        vectorFinal = vectorFinal))
}
