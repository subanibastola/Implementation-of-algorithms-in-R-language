library(class)
library(data.table)
A = data.frame(iris)
library(mltools)
B = one_hot(as.data.table(A))
#---------KMEANS-----------
model_kmeans = kmeans(B,4)
model_kmeans$cluster
C = data.frame(B,CV_K= model_kmeans$cluster)
#---------HIERARCHICAL-----------
model_hc = hclust(dist(B),method = "complete")
CV_hc = cutree(model_hc,k=4)
C = data.frame(C,CV_hc)
plot(model_hc)
