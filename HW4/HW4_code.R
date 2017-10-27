library(stats)
library(ggplot2)
library(dbscan)
library(factoextra)
library(NbClust)
library(cluster)
library(mclust)
library(e1071)
library(fpc)

#Read dataset
cluster_data3 <- read.csv("Clustering3.csv")
cluster_data3 <- cluster_data3[,2:3]
colnames(cluster_data3)<- c("dim1","dim2") 

df <- scale(cluster_data3)

#Visualization before clustering
ggplot(as.data.frame(df), aes(dim1, dim2)) + geom_point()


# Elbow method for optimal no of clusters
kmeans_elbow <- fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

kmeans_elbow

# Silhouette method
kmeans_silhouette <- fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

kmeans_silhouette

#---------------------------------------------------------------------------------------------------------------#
#Kmeans clustering 
set.seed(20)
cluster2 <- kmeans(df, 4, nstart = 20)

#Visualization after clustering

kmeans_cluster<- fviz_cluster(cluster2, data = df, geom = "point",
             stand = FALSE, frame.type = "norm")
kmeans_cluster
# cluster2$cluster <- as.factor(cluster2$cluster)
# ggplot(cluster_data3, aes(dim1, dim2, color = cluster2$cluster)) + geom_point()
#----------------------------------------------------------------------------------------------------------------#
#Hierarchical clustering
hcut_elbow <- fviz_nbclust(df, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

hcut_elbow

# Silhouette method
hcut_silhouette <- fviz_nbclust(df, hcut, method = "silhouette")+
  labs(subtitle = "Silhouette method")

hcut_silhouette

# Compute pairewise distance matrices
dist.res <- dist(df, method = "euclidean")

# Hierarchical clustering results
hc <- hclust(dist.res, method = "complete")

# Visualization of hclust
plot(hc, labels = FALSE, hang = -1)
# Add rectangle around 3 groups
rect.hclust(hc, k = 4, border = 2:4) 

#-------------------------------------------------------------------------------------------------------------------#
#Density based clustering 
db_optimal <- dbscan::kNNdistplot(df)  
abline(h = 0.4, lty = 2) 
  
set.seed(123)

db <- dbscan(df, 0.4, 4)
db
hullplot(df, db$cluster) 
  
  

