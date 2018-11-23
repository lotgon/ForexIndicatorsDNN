source("parameters.R")
source("DataGenerator.R")
source("Model.R")

ticks <- rbind(readRDS("C:/Quotes/EURCHF 20180101 20180201.rds"))

trainGenerator <- CreateXYGenerator(ticks)
testGenerator <- CreateXYGenerator(readRDS("C:/Quotes/EURCHF 20180201 20180301.rds"))

model<-CreateModelFunctional()
model %>% fit_generator(trainGenerator, round(nrow(ticks)/batch_size), 100, validation_data = testGenerator, validation_steps=1)


testData<- testGenerator(1)
predictedData <- model %>% predict(testData[[1]])
plot(predictedData[1,] - testData[[2]][1,])




w <- model$get_weights()
str(w)
w[[1]] [1, 1:3, 1] <- c(0.5, 0.5, 0)
model$set_weights(w)
