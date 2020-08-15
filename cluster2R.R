library(MASS)
library(car)

data(iris)
attach(iris) 

k <- 3
kmeansobj<-kmeans(iris[1:4],k)
kmeansobj
pairs(iris[,1:4],col = c("red", "green3", "blue")[kmeansobj$cluster] )
pairs(iris[,1:4],col = c("red", "green3", "blue")[unclass(iris$Species)] )


d = dist(iris[1:4])
tree.avg = hclust(d, method="average")
plot(tree.avg)
membership <- cutree(tree.avg, k = 3)
pairs(iris[,1:4],col = c("red", "green3", "blue")[membership] )


library(cluster)
gap <- clusGap(iris[1:4], FUN = kmeans, K.max = 8)
plot(gap)

## EM algorithm for mixture of normal
#install.packages('mclust')

library(mclust)
mixclust = Mclust(iris[,1:4])
plot(mixclust)