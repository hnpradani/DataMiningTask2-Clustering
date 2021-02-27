#Outlier Analysis / Anomaly Detection

dataair <- airquality[c(1,3)]
dataair <- na.omit(dataair)
summary(dataair$Ozone)

#Boxplot
boxplot.stats(dataair$Ozone)$out
boxplot(dataair$Ozone)

#find the index of outliers from dataair$ozone
a <- which(dataair$Ozone %in% boxplot.stats(dataair$Ozone)$out)

#find the index of outliers from dataair$Wind
b <- which(dataair$Wind %in% boxplot.stats(dataair$Wind)$out)

#Outliers in either Ozone and Wind
outlier.list <- union(a,b)

#Outliers Plot
plot(dataair)
points(dataair[outlier.list,], col="red", pch='x', cex=2.5)

#Drop Outliers
dataminus <- dataair[-(outlier.list),]

#LOF
library(DMwR)
#Remove Species (Categorical column)
iriscluster <- iris[,1:4]
outlier.scores  <- lofactor(iriscluster, k=3)
plot(density(outlier.scores))

#pick top 5 outliers
outlier<-order(outlier.scores, decreasing = T)[1:5]
print(outlier)
print(iriscluster[outlier,])

n= nrow(iriscluster)
pch<- rep(".", n)
pch[outlier]<-"+"
col <- rep("black", n)
col[outlier] <- "red"
pairs(iriscluster, pch=pch, col=col)

#OutlierDetection NN
library(OutlierDetection)
nn(dataclean, k=4)


#Clustering

irisclusters <- iris
irisclusters$Species <- NULL
irisscale <- scale(irisclusters)

#K-means
kmeans.result<- kmeans(irisscale,center=3, nstart = 25)
kmeans.result

plot(irisclusters[c("Sepal.Length","Sepal.Width")], 
     col = kmeans.result$cluster)

#Plot All
plot(irisclusters, col = kmeans.result$cluster)

library(cluster)

#Aglo Clustering
#method can be 'average', 'single', 'complete', 'ward'
# Agglomerative Nesting (Hierarchical Clustering)
datacluster.agnes <- agnes(x=irisclusters, # data matrix
                    stand = TRUE, # standarize the data
                    metric= "euclidian", # metric for distance matrix
                    method="ward" # Linkage method
                    ) 

library(factoextra)
fviz_dend(iris.agnes, cex=0.6, k=3)

clust<- cutree(iris.agnes, k=3)
fviz_cluster(list(data = iriscluster, cluster=clust))

#Divisive Clustering
iris.diana <- diana(x=irisclusters, # data matrix
                    stand = TRUE, # standardize the data
                    metric = "euclidian" # metric for distance matrix
                    )

#Plot Dendogram
fviz_dend(iris.diana, cex=0.6 , k=3)

clust2<-cutree(iris.diana, k=3)
fviz_cluster(list(data=irisclusters, cluster=clust2))


#Density Based
library(fpc)
iris.ds <- dbscan(irisclusters, eps=0.42, MinPts=5)

#Plot Cluster
plot(iris.ds, irisclusters)
plot(iris.ds, irisclusters[c(1,2)])




#Evaluation

WSS <- sapply(1:10, function(k){
  kmeans(irisscale, centers = k, nstart = 25)$tot.withinss
})
plot(1:10, WSS, type="b", xlab="Number of cluster(k)",
     ylab="Within grops sum of squares")

fviz_nbclust(irisscale, FUN = kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

library(cluster)
km.sil <- silhouette(kmeans.result$cluster, dist(irisscale))
head(km.sil[,1:3],10)

#plot
plot(km.sil, main="silhout plot-kmeans")
#plot using factoextra
fviz_silhouette(km.sil)

iris.dist <- dist(irisclusters)
library(seriation)
pimage(iris.dist)
pimage(iris.dist, order=order(kmeans.result$cluster))
dissplot(iris.dist, labels=kmeans.result$cluster, 
         options = list(main = "k-means with k =3"))

