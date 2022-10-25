# Course: COMP3340
# Student Number: C3328365
# Name: Eden Fagerstrom
# Assignment: Assignment 1 - Part 3


main <- function(){
  
  library("readxl")
  library(igraph)
  library('rgexf')
  library('BNSL')
  library('igraph')
  library('cccd')
  library('class')
  #concrete <- read_excel("Concrete_Data.xls")
  # Feature labels renamed for simplicity
  #names(concrete) <- c("Cement","Blast Furnace Slag","Fly Ash", "Water", 
  #                     "Superplasticizer", "Coarse Aggregate", "Fine Aggregate", 
  #                     "Age", "Concrete Compressive Strength")
  
  #concreteFeatSampDistMats(concrete)
  #RNGConcfeatSamps(concrete)
  #borutaFeatSel()
  #rowShuffler()
  #irisRowMatrix()
  commonEdgesMstKnn()
  
}

main()



# Exercise 1









# returns 2 matrices created using euclidean distance measure between rows and 
# columns of Concrete regression dataset
concreteFeatSampDistMats <- function(dataFile){
  
  colMat <- matrix(nrow = ncol(dataFile), ncol = ncol(dataFile))
  rowMat <- matrix(nrow = nrow(dataFile), ncol = nrow(dataFile))
  colnames(colMat) <- colnames(dataFile)
  rownames(colMat) <- colnames(dataFile)
  colnames(rowMat) <- rownames(dataFile)
  rownames(rowMat) <- rownames(dataFile)
  concFeatsMat <- colMat
  concSampsMat <- rowMat
  
  x <- 1
  y <- 1
  
  # Assigns Euclidean distance values to empty matrix 
  while(x < nrow(colMat)+1){
    while(y < ncol(colMat)+1){
      
      concFeatsMat[x,y] <-  euclideanDist(dataFile[,x],dataFile[,y])
      
      y = y + 1
    }
    x = x + 1
    y = 1
  }
  
  
  
  i <- 1
  j <- 1
  
  # Assigns Euclidean distance values to empty matrix 
  while(i < nrow(rowMat)+1){
    while(j < ncol(rowMat)+1){
      
      concSampsMat[i,j] <-  euclideanDist(dataFile[i,],dataFile[j,])
      
      j = j + 1
    }
    i = i + 1
    j = 1
  }
  
  # writes distances matrices to csv file
  write.csv(concFeatsMat, "ConcreteFeaturesEuclideanMatrix.csv")
  write.csv(concSampsMat, "ConcreteSamplesEuclideanMatrix.csv")
  
  concreteLis <- list(concFeatsMat, concSampsMat)
  
  
}



# returns euclidean distance between 2 vectors
euclideanDist <- function(x, y){
  
  sqrt(sum((x-y)^2))
  
}

# Returns the RNG calculated from taking the euclidean distance between samples of the concrete dataset and the RNG
# calculated by taking the euclidean distance between features of the concrete dataset
RNGConcfeatSamps <- function(dataFile)
{
  library(cccd)
  featSampMat <- concreteFeatSampDistMats(dataFile)

  featMat <- featSampMat[[1]]
  sampMat <- featSampMat[[2]]
  
  rngFeats <- rng(dx=featMat, open = FALSE, r = 1, algorithm = 'cover_tree')
  rngSamps <- rng(dx=sampMat, open = FALSE, r = 1, algorithm = 'cover_tree')
  
  
  # sets graphs to undirected
  rngFeats <- as.undirected(rngFeats)
  rngSamps <- as.undirected(rngSamps)
  
  
  #Sets the labels which will be displayed in yEd 
  E(rngFeats)$weight <- apply(get.edges(rngFeats,1:ecount(rngFeats)),1,function(x)featMat[x[1],x[2]])
  E(rngFeats)$label <- round(E(rngFeats)$weight, 3)
  V(rngFeats)$label <- colnames(featMat)
  E(rngSamps)$weight <- apply(get.edges(rngSamps,1:ecount(rngSamps)),1,function(x)sampMat[x[1],x[2]])
  E(rngSamps)$label <- round(E(rngSamps)$weight, 3)
  V(rngSamps)$label <- colnames(sampMat)
  
  # writes graph to gml file to be displayed in yEd graph editor
  write_graph(rngSamps, 'ConcreteSamplesRNG.gml', format = "gml")
  write_graph(rngFeats, 'ConcreteFeaturesRNG.gml', format = "gml")
  plot(rngFeats)
  plot(rngSamps)
  
  
  
}



# Exercise 2





borutaFeatSel <- function(){
  library('cccd')
  library('igraph')
  library('Boruta')
  library('caret')
  
  pres <- read.csv("USPresidency.csv", header = TRUE, sep = ",")
  pres$Year <- NULL
  set.seed(111)
  bor <- Boruta(pres, pres$Target, doTrace = 2)
  bor
  bor <- TentativeRoughFix(bor, averageOver = Inf)
  x <- getSelectedAttributes(bor, withTentative = FALSE)
  x <- as.data.frame(x)
  View(x)
}


# Creating new dataframe with subset of features selected from Boruta Algorithm
presSubset <- read.csv("USPresidency.csv", header = TRUE, sep = ",")
presSubset <- presSubset[, c("Q4", "Q5", "Q10", "Q12", "Target")]

# Writing new dataframe to csv file 
write.csv(presSubset, file = "USPresidencyFeatureSubset.csv")

# Feature Subset Dataset row order has been randomized and split into 2 
# datasets of similar sample size (Training, Test)
rowShuffler <- function(){
  
  set.seed(42)
  presSubset <- presSubset[sample(1:nrow(presSubset)),]
  write.csv(presSubset[1:15,],"USTrainingSet.csv")
  write.csv(presSubset[16:31,],"USTestingset.csv")
  
}






# Exercise 5



irisRowMatrix <- function()
{
  
  library(igraph)
  
  
  # reads in Iris dataset
  iris <- read.csv("Iris.csv", header = TRUE)
  rowMat <- matrix(nrow = nrow(iris), ncol = nrow(iris))
  # Row and Column names set to row Ids for uniqueness
  colnames(rowMat) <- iris$Id
  rownames(rowMat) <- iris$Id
  iris$Species <- NULL
  iris$Id <- NULL
  
  
  x <- 1
  y <- 1
  
  # Assigns Euclidean distance values to empty matrix 
  while(x < nrow(rowMat)+1){
    while(y < ncol(rowMat)+1){
      
      rowMat[x,y] <-  euclideanDist(iris[x,],iris[y,])
      
      y = y + 1
    }
    x = x + 1
    y = 1
  }
  
  irisRowsMatrix <- rowMat
  
  
  
  
}


commonEdgesMstKnn <- function()
{
  library(cccd)
  
  
  irisSamps <- irisRowMatrix()
  #mst code
  irisMST <- graph_from_adjacency_matrix(irisSamps,mode = 'undirected', weighted = TRUE)
  V(irisMST)$name <- colnames(irisSamps)
  V(irisMST)$label <- colnames(irisSamps)
  E(irisMST)$label <- round(E(irisMST)$weight, 3)
  E(irisMST)$name <- round(E(irisMST)$weight, 3)
  MSTGraphIris <- mst(irisMST)
  plot(MSTGraphIris)
  write_graph(MSTGraphIris, 'IrisSamplesMST.gml', format = "gml")
  
  
  #knn code
  knnGraphIris <- nng(dx = irisSamps, k = 4, algorithm = 'cover_tree')
  knnGraphIris <- as.undirected(knnGraphIris)
  V(knnGraphIris)$name <- colnames(irisSamps)
  V(knnGraphIris)$label <- colnames(irisSamps)
  E(knnGraphIris)$weight <- apply(get.edges(knnGraphIris,1:ecount(knnGraphIris)),1,function(x)irisSamps[x[1],x[2]])
  E(knnGraphIris)$label <- round(E(knnGraphIris)$weight, 3)
  
  write_graph(knnGraphIris, 'IrisSamplesKnn.gml', format = "gml")
  
  
  # stores common edges between graphs in new variable
  commonEdges <- intersection(knnGraphIris, MSTGraphIris, byname = "auto" )
  # labels for common edges graph
  E(commonEdges)$weight <- apply(get.edges(commonEdges,1:ecount(commonEdges)),1,function(x)irisSamps[x[1],x[2]])
  E(commonEdges)$label <- round(E(commonEdges)$weight, 3)
  V(commonEdges)$name <- colnames(irisSamps)
  V(commonEdges)$label <- colnames(irisSamps)
  
  write_graph(commonEdges, 'IrisCommonEdgeMstKnn.gml', format = "gml")
  plot(commonEdges)
}
library(igraph)
install.packages("mstknnclust")
library("mstknnclust")
irisSamps <- irisRowMatrix()
results <- mst.knn(irisSamps)
results
cg.network=igraph::graph.adjacency(irisSamps, mode="undirected", weighted=TRUE)
plot(results$network, edge.label=round(E(cg.network)$weight, 2), vertex.size=8, 
     vertex.color=igraph::clusters(results$network)$membership, 
     layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
     main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))









# Exercise 6

# WSS Plot helps to identify the ideal number of clusters to set for the k-means algorithm
wssPlot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}



install.packages("NbClust")
install.packages("cluster")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(NbClust)
library(cluster)




kmeansIris <- function(){
  
  irisSamps <- irisRowMatrix()
  wssPlot(irisSamps)
  irisKM <- kmeans(irisSamps, centers = 3, nstart = 25)
  irisG <- graph_from_adjacency_matrix(irisSamps)
  plot(irisG)
  summary(irisKM)
  table(irisKM$cluster, iris$Species)
  clusplot(pam(irisSamps, 3))
  
}
kmeansIris()
























