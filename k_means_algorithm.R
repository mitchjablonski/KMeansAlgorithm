library("lsa")
library("gdata")

kmeansAlg <- function(numPoints, myData, userList){
  myData = determineNewPoints(userList, numPoints, myData)
  names <- c(colnames(myData))
  userList = c(names[3:length(names)])
  matrix = as.matrix.data.frame((myData[userList]))
  simMat=cosine(matrix)
  clusters = determineCurrentClusters(simMat, numPoints, userList, myData)
  
  return(clusters)
}

determineNewPoints <- function( userList, numPoints, myData)
{
  selNumbers = 1:length(userList)
  for (i in 1:numPoints)
  {
    currValue = sample(1:(length(selNumbers)), 1)
    newPoint = userList[selNumbers[currValue]]
    var = paste("c",i ,sep="")
    userList[[length(userList)+1]] <-var
    
    myData[var] <- myData[newPoint]
    
    myMask = selNumbers[1:length(selNumbers)] != selNumbers[currValue]
    selNumbers = selNumbers[myMask]
  }
  return(myData)
}

determineCurrentClusters <- function(simMat, numPoints, userList, myData)
{
  previousClust = ''
  clusterList = ''
  clusterList = '1'
  while ((previousClust) != (clusterList))
  {
    previousClust = clusterList
    clusterList = ''
    for (i in 1:numPoints)
    {  var = paste("c",i ,sep="")
       myData[var] = 0
    }
    for (x in 1:(length(userList)-numPoints))
    {
      maxValue = 0;
      maxVar = paste("c",0 ,sep="")
      for (y in (length(userList)-numPoints+1): length(userList))
        if (simMat[x,y] > maxValue)
        {
          maxValue = max(maxValue, simMat[x,y])
          maxVar = userList[y]
        }
      }
      clusterList = paste(maxVar,clusterList ,sep="")
      myData[maxVar] = myData[userList[x]] + myData[maxVar]

    } 
    matrix = as.matrix.data.frame((myData[userList]))
    simMat=cosine(matrix)
  }
  return(clusterList)
}

{
  }
}
##-1 will be an empty string, drop it

