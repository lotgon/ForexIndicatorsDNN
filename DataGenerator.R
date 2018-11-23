source("parameters.R")
require(data.table)
require(caret)

CreateXYGenerator <- function(ticks){
#return generator function
#input 
#output list [1] = dim(batch_size, Tx, Nx) 
  
  ticks[,duration:=as.numeric(createTime - shift(createTime))]
  ticks <- ticks[complete.cases(ticks)]
  ticks <- ticks[duration >= minTickDuration]
  ticks[duration > maxTickDuration, duration:=maxTickDuration]
  ticks[, duration:=signif(duration, 1)]
  ticks[, duration:= sdForexRates*duration/sd(duration)]
  ticks[,duration:=0]
  meanRates <- mean(ticks$bid)
  sdRates <- sd(ticks$bid)
  ticks[,bidN:=(bid-meanRates)/sdForexRates]
  ticks[,askN:=(ask-meanRates)/sdForexRates]
  
  ticksNumber <- ticks[,.N]
  previousIndex <- 1
  function(batchSize=batch_size){
    xBatch <- array(0, dim=c(batchSize, Tx, Nx))
    yBatch <- array(0, dim=c(batchSize, Ty))
    #xBatch <- array(0, dim=c(ticksNumber, Tx, Nx))
    #yBatch <- array(0, dim=c(ticksNumber, Ty))
    
    startIndices <- (round(previousIndex + (1:batchSize) * Tx + runif(batchSize, 0, Tx/2))) %% (ticksNumber - Tx - 1 )  + Tx
    #startIndices <- Tx:ticksNumber
    previousIndex <- tail(startIndices, 1)
    
    for(i in seq_along(startIndices)){
      inputData <- PrepareInput(ticks, startIndices[[i]])
      outputData <- PrepareOutput(ticks, startIndices[[i]])
      xBatch[i,,] <- as.matrix( inputData )
      yBatch[i,] <- outputData
    }
    list(xBatch, yBatch)
  }
  
}
PrepareOutput <- function(ticks, startIndex){
  ticks[(startIndex - Tx +1 ):startIndex, (ask+bid)/2]
}
PrepareInput <- function(ticks, startIndex){
  x <- ticks[(startIndex - Tx +1 ):startIndex, .(ask, bid, duration) ]
  x  
}

  