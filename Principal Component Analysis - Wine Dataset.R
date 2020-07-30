#install.packages("gdata")
#install.packages("xlsx")
library(gdata)

wine <- read.csv('D:\\Data Science\\Excelr\\Assignments\\Assignment\\PCA\\wine.csv')

pca <- princomp(wine[,2:14], cor = TRUE, covmat = NULL)
summary(pca)
pca$scores

#pca$loadings

plot(pca$scores[,1:2], col="BLUE", pch=18,cex=0.5, lwd=3)
text(pca$scores[,1:2], labels = c(1:25), cex = 1)



pc <- pca$scores[,1:3]

######### Hierarchial Clustering

pcs <- scale(pc)
d <- dist(pcs, method = "euclidean") # Computing the distance matrix
fit <- hclust(d, method = "average") #Computing the algorithm # try with centroid
plot(fit) #display dendrogram
groups <- cutree(fit, k=3) #cut tree into 3 clusters
#draw dendrogram with red borders around the 5 clusters
rect.hclust(fit, k=3, border = "red")
#attach the cluster number to universities
clusters=data.frame('Type'=wine[,1],'Cluster'=groups)
write.csv(clusters,file = "clusters-hc-type.csv")

fit1 <- hclust(d, method = "centroid") #Computing the algorithm # trying with centroid
plot(fit1) #display dendrogram
groups <- cutree(fit1, k=3) #cut tree into 3 clusters
#draw dendrogram with red borders around the 3 clusters
rect.hclust(fit1, k=3, border = "red")
#attach the cluster number to universities
clusters1=data.frame('Type'=wine[,1],'Cluster'=groups)
write.csv(clusters,file = "clusters1-hc-type.csv")


########### K-means Clustering

#Elbow Chart
wss <- c()
for (i in 2:15) wss[i] <- sum(kmeans(pc, centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of Clusters", ylab = "Avg Distance")
### Cluster algorithm building
km <- kmeans(pc, 3)
km$centers
km$cluster
##Animation
install.packages("animation")
library(animation)
windows()
km <- kmeans.ani(pc, 3)
