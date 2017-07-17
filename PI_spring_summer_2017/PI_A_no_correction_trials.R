#set the working directory
setwd("./data")

#list rat number here
ratList <- list(101,102,103,104,105,106,107,108,109,110,112)

#make a data frame for the AB acquisition days
sessList <- list(124:129)#list session nubmers here
x <- length(sessList)
y <- length(ratList)
rowsNeeded <- (x*y)

ABsummary <- matrix(data = NA, nrow = rowsNeeded, ncol=3)
colnames(ABsummary)<- c('ratID', 'session', 'accuracy')

i = 1
for (sessNum in sessList){
  for (rat in ratList) {
    filename <- paste("U3_",rat,"_",sessNum,".csv", sep="")
    if(file.exists(filename)){
      print(filename)
      currentData <- read.csv(filename, header=TRUE, sep="\t")
      currentData$resp2 <- ifelse(currentData$Response == "correct", 1,0)
      currentAB <- subset(currentData, trialType == "AB")
      ABacc <- mean(currentAB$resp2)
      ABsummary[i,0:3] <- c(rat, sessNum, ABacc)
      i=i+1
    }
  }
}


ABsummary <- data.frame(ABsummary)
ABsummary$ratID <- as.factor(ABsummary$ratID)
ABsummary$session <- as.factor(ABsummary$session)
#ABsummary$condition <- ifelse(is.element(ABsummary$ratID, sapGroup) ,'sap','control')

ABsummaryTable <- aggregate(accuracy~session, data=ABsummary, FUN=(mean))
tempTable <- aggregate(accuracy~session, data=ABsummary, FUN=(sd))
ABsummaryTable<- as.data.frame(ABsummaryTable)
ABsummaryTable$sd <- tempTable$accuracy*100
tempTable$accuracy <- tempTable$accuracy/sqrt(y)
ABsummaryTable$se <- tempTable$accuracy*100
ABsummaryTable$accuracy <- ABsummaryTable$accuracy * 100

ratTable <- aggregate(accuracy~ratID, data=ABsummary, FUN=(median))
