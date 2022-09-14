
# Reading in Students Academic Performance Dataset
student <- read.csv('xAPI-Edu-Data.csv')
student

# Exercise 1

# a)

# Summary of Students Academic Performance Dataset
summary(student)

# pearsons correlation between numeric variables
pearsonCorr <- function(){
  a <- cor.test(student$raisedhand, student$VisITedResources, method = "pearson")
  b <- cor.test(student$raisedhand, student$AnnouncementsView, method = "pearson")
  c <- cor.test(student$raisedhand, student$Discussion, method = "pearson")
  d <- cor.test(student$VisITedResources, student$AnnouncementsView, method = "pearson")
  e <- cor.test(student$VisITedResources, student$Discussion, method = "pearson")
  f <- cor.test(student$AnnouncementsView, student$Discussion, method = "pearson")
  lis <- list(a,b,c,d,e,f)
  lis
}
pearsonCorr()



# scatter plot between numeric variables
numPlotter <- function(){
  par(mfrow = c(2, 3))
  plot(student$raisedhand, student$VisITedResources)
  plot(student$raisedhand, student$AnnouncementsView)
  plot(student$raisedhand, student$Discussion)
  plot(student$VisITedResources, student$AnnouncementsView)
  plot(student$VisITedResources, student$Discussion)
  plot(student$AnnouncementsView, student$Discussion)
}

  numPlotter()

# b)



  
  
  
  

chisq.test(student$gender, student$NationalITy)
chisq.test(student$gender, student$PlaceofBirth)
chisq.test(student$gender, student$StageID)
chisq.test(student$gender, student$GradeID)
chisq.test(student$gender, student$SectionID)
chisq.test(student$gender, student$Topic)
chisq.test(student$gender, student$Semester)
chisq.test(student$gender, student$Relation)
chisq.test(student$gender, student$raisedhands)
chisq.test(student$gender, student$Topic)
chisq.test(student$gender, student$Topic)











# Exercise 2
install.packages("readxl")
library(readxl)
library(igraph)

# reads in training set from Alzheimers Disease dataset
alz <- read_excel("AlzheimersDisease.xls", sheet = "Training Set")
head(alz)


# a)

euclideanDist <- function(x, y){
  
  sqrt(sum((x-y)^2))
  
}


alzSamplesMatrix <- function()
{

  #install.packages("readxl")
  library(readxl)
  library(igraph)
  
  
  # reads in training set from Alzheimers Disease dataset
  alz <- read_excel("AlzheimersDisease.xls", sheet = "Training Set")
  alz$CLASS <- NULL
  colMat <- matrix(nrow = ncol(alz), ncol = ncol(alz))
  
  colnames(colMat) <- colnames(alz)
  rownames(colMat) <- colnames(alz)
  alzSampMat <- colMat
  
  x <- 1
  y <- 1
  
  # Assigns Euclidean distance values to empty matrix 
  while(x < nrow(colMat)+1){
    while(y < ncol(colMat)+1){
      
      alzSampMat[x,y] <-  euclideanDist(alz[,x],alz[,y])

      y = y + 1
    }
    x = x + 1
    y = 1
  }
  
  write.csv(alzSampMat, "AlzheimerSamplesMatrix.csv")
  
  alzAdjMat <- alzSampMat
  
}

alzSamplesMatrix()


# Returns The MST Graph for Samples (Columns) of Alzheimers dataset
mstSamplesGraph <- function ()
{
  
  alzMat = alzSamplesMatrix()
  
  alzSampGraph <- graph_from_adjacency_matrix(alzMat,mode = 'undirected', weighted = TRUE)
  
  #Sets the labels for vertices and edges
  V(alzSampGraph)$name <- colnames(alzMat)
  V(alzSampGraph)$label <- colnames(alzMat)
  E(alzSampGraph)$label <- round(E(alzSampGraph)$weight, digits = 3)
  E(alzSampGraph)$name <- round(E(alzSampGraph)$weight, digits = 3)
  

  # writes graph to gml file to be displayed in yEd graph editor
  write_graph((mst(alzSampGraph)),'alzheimerSamplesMST.gml',format = "gml")
  
  plot(mst(alzSampGraph))

  
}
mstSamplesGraph()

# b)

alzProteinsMatrix <- function()
{
  
  #install.packages("readxl")
  library(readxl)
  library(igraph)
  
  
  # reads in training set from Alzheimers Disease dataset
  alz <- read_excel("AlzheimersDisease.xls", sheet = "Training Set")
  protMat <- matrix(nrow = nrow(alz), ncol = nrow(alz))
  colnames(protMat) <- alz$CLASS
  rownames(protMat) <- alz$CLASS
  alz$CLASS <- NULL
  x <- 1
  y <- 1
  
  # Assigns Euclidean distance values to empty matrix 
  while(x < nrow(protMat)+1){
    while(y < ncol(protMat)+1){
      
      protMat[x,y] <-  euclideanDist(alz[x,],alz[y,])
      
      y = y + 1
    }
    x = x + 1
    y = 1
  }
  
  write.csv(protMat, "AlzheimerProteinsMatrix.csv")
  
  alzDistProtMat <- protMat
  #alzDistProtMat
  
}

alzProteinsMatrix()





mstProteinsGraph <- function ()
{
  
  alzMat = alzProteinsMatrix()
  
  alzProtGraph <- graph_from_adjacency_matrix(alzMat,mode = 'undirected', weighted = TRUE)
  
  #Sets the labels for vertices and edges
  V(alzProtGraph)$name <- colnames(alzMat)
  V(alzProtGraph)$label <- colnames(alzMat)
  E(alzProtGraph)$label <- round(E(alzProtGraph)$weight, digits = 3)
  E(alzProtGraph)$name <- round(E(alzProtGraph)$weight, digits = 3)
  
  
  # writes graph to gml file to be displayed in yEd graph editor
  write_graph((mst(alzProtGraph)),'alzheimerProteinsMST.gml',format = "gml")
  
  plot(mst(alzProtGraph))
  
  
}
mstProteinsGraph()







# c)





RNGSamples <- function()
{
  library(cccd)
  alzSampMat = alzSamplesMatrix()
  rngSampCols <- rng(dx=alzSampMat, open = FALSE, r = 1, algorithm = 'cover_tree')
  
  # sets graphs to undirected
  rngSampCols <- as.undirected(rngSampCols)
  
  #Sets the labels which will be displayed in yEd 
  E(rngSampCols)$weight <- apply(get.edges(rngSampCols,1:ecount(rngSampCols)),1,function(x)alzSampMat[x[1],x[2]])
  E(rngSampCols)$label <- round(E(rngSampCols)$weight, 3)
  V(rngSampCols)$label <- colnames(alzSampMat)
  
  # writes graph to gml file to be displayed in yEd graph editor
  write_graph(rngSampCols, 'AlzheimerSamplesRNG.gml', format = "gml")
  plot(rngSampCols)
}
RNGSamples()





# d)


RNGProteins <- function()
{
  library(cccd)
  alzProtMat = alzProteinsMatrix()
  rngProtRows <- rng(dx=alzProtMat, open = FALSE, r = 1, algorithm = 'cover_tree')
  
  # sets graphs to undirected
  rngProtRows <- as.undirected(rngProtRows)
  
  #Sets the labels which will be displayed in yEd 
  E(rngProtRows)$weight <- apply(get.edges(rngProtRows,1:ecount(rngProtRows)),1,function(x)alzProtMat[x[1],x[2]])
  E(rngProtRows)$label <- round(E(rngProtRows)$weight, 3)
  V(rngProtRows)$label <- colnames(alzProtMat)
  
  # writes graph to gml file to be displayed in yEd graph editor
  write_graph(rngProtRows, 'AlzheimerProteinsRNG.gml', format = "gml")
  plot(rngProtRows)
}

RNGProteins()




install.packages('Boruta')
library(Boruta)

?Boruta

alz <- read_excel("AlzheimersDisease.xls", sheet = "Training Set")
alz

bor <- Boruta(alz$CLASS ~ ., data = alz, doTrace = 2)


alz$CLASS








