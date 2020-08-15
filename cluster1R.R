
########################################################
######################                                 #           
# R example code for cluster analysis:                 #
######################                                 #
########################################################


#################################################################
#################################################################
###########
###########           Hierarchical Clustering        
###########
#################################################################
#################################################################


# This is a new dataset "foodstuffs" data set 

###########################################
###  Foodstuffs example 
###########################################

food <- read.table("foodstuffs.txt", header=T)

attach(food)

# The hclust function requires that a distance object be input:

# Let's first scale the data by dividing each variable by its standard deviation:

std <- apply(food[,-1], 2, sd) # finding standard deviations of variables
food.std <- sweep(food[,-1],2,std,FUN="/") 

# Calculating pairwise Euclidean distances between the (standardized) objects:

dist.food <- dist(food.std)

# Single linkage:

food.single.link <- hclust(dist.food, method='single')

# Plotting the single linkage dendrogram:

plclust(food.single.link, labels=Food, ylab="Distance")

windows() # opening new window while keeping previous one open

# complete linkage:

food.complete.link <- hclust(dist.food, method='complete')

# Plotting the complete linkage dendrogram:

plclust(food.complete.link, labels=Food, ylab="Distance")

windows() # opening new window while keeping previous one open

# Average linkage:

food.avg.link <- hclust(dist.food, method='average')

# Plotting the average linkage dendrogram:

plclust(food.avg.link, labels=Food, ylab="Distance")

# Note the complete linkage algorithm is slightly less prone to forming 
# "outlier-only" clusters here.

# Cutting the complete-linkage dendrogram to form k=2 clusters here:

cut.2 <- cutree(food.complete.link, k=2)
cut.2     # printing the "clustering vector"

food.2.clust <- lapply(1:2, function(nc) Food[cut.2==nc])  
food.2.clust   # printing the clusters in terms of the Food labels

# Suppose we preferred a 5-cluster solution:

cut.5 <- cutree(food.complete.link, k=5)

# Equivalently, in this case:
cut.5 <- cutree(food.complete.link, h=3.5)  
# h specifies the height at which the dendrogram should be cut

cut.5   # printing the "clustering vector"

food.5.clust <- lapply(1:5, function(nc) Food[cut.5==nc])  
food.5.clust   # printing the clusters in terms of the Food labels


############# Visualization of Clusters:

### Via the scatterplot matrix:

pairs(food[,-1], panel=function(x,y) text(x,y,cut.5))

# Cluster 1 seems to be the high-fat, high-energy foods (beef, ham, pork)
# Cluster 2 foods seem to have low iron (more white meats than red meats)
# Cluster 4 foods have low protein (the clams)
# Cluster 5 is a high-calcium outlier (canned sardines)

### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

food.pc <- princomp(food[,-1],cor=T)

# Setting up the colors for the 5 clusters on the plot:
my.color.vector <- rep("green", times=nrow(food))
my.color.vector[cut.5==2] <- "blue"
my.color.vector[cut.5==3] <- "red"
my.color.vector[cut.5==4] <- "orange"
my.color.vector[cut.5==5] <- "brown"

# Plotting the PC scores:

par(pty="s")
plot(food.pc$scores[,1], food.pc$scores[,2], ylim=range(food.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(food.pc$scores[,1], food.pc$scores[,2], labels=Food, cex=0.7, lwd=2,
     col=my.color.vector)

# What would this plot look like for the 2-cluster solution?
# For the 3-cluster solution?


#################################################################
#################################################################
###########
###########           Partitioning Clustering        
###########
#################################################################
#################################################################



##########################
##
##  K-means clustering
##
##########################

###########################################
###  Foodstuffs example 
###########################################

# Consider the food.std data frame given above.

# A K-means clustering with k = 5:

# Note that the stability of the result can be improved by increasing the maximum number 
# of iterations and using multiple random starts:

food.k5 <- kmeans(food.std, centers=5, iter.max=100, nstart=25)
food.k5

# Let's try k=4:

food.k4 <- kmeans(food.std, centers=4, iter.max=100, nstart=25)
food.k4

# Printing the clustering vector for the 4-cluster solution:

food.k4$cluster

food.k4.clust <- lapply(1:4, function(nc) Food[food.k4$cluster==nc])  
food.k4.clust   # printing the clusters in terms of the Food labels


############# Visualization of Clusters:

### Via the scatterplot matrix:

pairs(food[,-1], panel=function(x,y) text(x,y,food.k4$cluster))

# Cluster 1 foods tend to be high in calcium. (this comment does not reflect all runs of the algorithm)
# Cluster 4 foods tend to be high in fat. (this comment does not reflect all runs of the algorithm)


### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

food.pc <- princomp(food[,-1],cor=T)

# Setting up the colors for the 5 clusters on the plot:
my.color.vector <- rep("green", times=nrow(food))
my.color.vector[food.k4$cluster==2] <- "blue"
my.color.vector[food.k4$cluster==3] <- "red"
my.color.vector[food.k4$cluster==4] <- "orange"

# Plotting the PC scores:

par(pty="s")
plot(food.pc$scores[,1], food.pc$scores[,2], ylim=range(food.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(food.pc$scores[,1], food.pc$scores[,2], labels=Food, cex=0.7, lwd=2,
     col=my.color.vector)

# Cluster 1 is the "canned seafood" cluster.  (this comment does not reflect all runs of the algorithm)
# Cluster 2 is the clams cluster.  (this comment does not reflect all runs of the algorithm)

## NOTE:  The default for the kmeans function in R is the Hartigan-Wong (1979) algorithm.
## The MacQueen algorithm (1967) can be used by altering the code to, say:
##                kmeans(food.std, centers=4,algorithm="MacQueen")
## You can try it in this case -- I don't think the MacQueen algorithm produces as good of a result.




#################################################################
#################################################################
###########
###########           Model-based Clustering        
###########
#################################################################
#################################################################

# Consider the built-in USArrests data set in r:

help(USArrests)

# We will perform a model-based clustering of the 50 states based on these 4 variables:

# Loading the mclust package:
# May need to install the mclust package first?
# If so, type at the command line:  install.packages("mclust", dependencies=T)
# while plugged in to the internet.

library(mclust)

# The R function Mclust performs model-based clustering for a range of models
# and a variety of values of k:

arrest.clus <- Mclust(USArrests)

# By default, the models considered are:
# "EII": spherical, equal volume 
# "VII": spherical, unequal volume 
# "EEI": diagonal, equal volume and shape
# "VEI": diagonal, varying volume, equal shape
# "EVI": diagonal, equal volume, varying shape 
# "VVI": diagonal, varying volume and shape 
# "EEE": ellipsoidal, equal volume, shape, and orientation 
# "EEV": ellipsoidal, equal volume and equal shape
# "VEV": ellipsoidal, equal shape 
# "VVV": ellipsoidal, varying volume, shape, and orientation  

# Plotting the BIC values:

plot(arrest.clus, data=USArrests, what="BIC")

# Hit ENTER to see the BIC plot.

# The best solution is VEI with 3 clusters.

# The clustering vector:

clus.vec.3 <- arrest.clus$classification
clus.vec.3

arrest.3.clust <- lapply(1:3, function(nc) row.names(USArrests)[clus.vec.3==nc])  
arrest.3.clust   # printing the clusters in terms of the state names

# This gives the probabilities of belonging to each cluster for every object:

round(arrest.clus$z,2)


# Visualizing the clusters:

## Via a scatterplot matrix:

plot(arrest.clus, data=USArrests, what="classification")

# Hit ENTER to see a scatterplot matrix with the points separated by cluster.
# Hit ENTER again to see a scatterplot of the first two variables with objects separated by cluster.

### Via a plot of the scores on the first 2 principal components, 
### with the clusters separated by color:

arrests.pc <- princomp(USArrests,cor=T)

# Setting up the colors for the 5 clusters on the plot:
my.color.vector <- rep("blue", times=nrow(USArrests))
my.color.vector[arrest.clus$classification==2] <- "red"
my.color.vector[arrest.clus$classification==3] <- "green"

# Plotting the PC scores:

par(pty="s")
plot(arrests.pc$scores[,1], arrests.pc$scores[,2], ylim=range(arrests.pc$scores[,1]), 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
text(arrests.pc$scores[,1], arrests.pc$scores[,2], labels=row.names(USArrests), 
     cex=0.7, lwd=2, col=my.color.vector)

# Reviewing the PCA:

summary(arrests.pc,loadings=T)

# Note PC1 is an overall "lack-of-crime" index and PC2 is a "rural" index.

## Note: We could also specifically request the best, say, 2-cluster solution (according to BIC)
## if we wanted to, for example:

# arrest.clus.2 <- Mclust(USArrests, G=2)


