#list rat number here
ratList <- 101:112

#make a data frame for the AB acquisition days
sessList <-list(45,47,48)#list session nubmers here
x <- length(sessList)
y <- length(ratList)
rowsNeeded <- (x*y)

#making an empty matrix
data1 <- matrix(data = NA, nrow = rowsNeeded, ncol=5)
colnames(data1)<- c('ratID', 'session', 'firstAttempt','meanAttempts','medianAttempts')

i = 1
for (sessNum in sessList){
  for (rat in ratList) {
    filename <- paste("U2_unlimi_",rat,"_",sessNum,".csv", sep="")
    print(filename)
    currentData <- read.csv(filename, header=TRUE, sep="\t")
    currentData$resp2 <- ifelse(currentData$Attempts == "1", 1,0)
    firstAttempts <- mean(currentData$resp2)
    totalAttmpts <- mean(currentData$Attempts)
    MedianAttmpts <- median(currentData$Attempts)
    data1[i,0:5] <- c(rat, sessNum, firstAttempts,totalAttmpts,MedianAttmpts)
    i=i+1
  }
}
#making the matrix into a data frame and adding lesion condition
data1 <- data.frame(data1) #making it into a data frame
data1$ratID <- factor(data1$ratID) #making rat number a factor

aggregate(data1$firstAttempt, by=list(data1$session), FUN='mean')
aggregate(data1$firstAttempt, by=list(data1$session), FUN='sd')

aggregate(data1$meanAttempts, by=list(data1$session), FUN='mean')
aggregate(data1$meanAttempts, by=list(data1$session), FUN='sd')

aggregate(data1$medianAttempts, by=list(data1$session), FUN='mean')
aggregate(data1$medianAttempts, by=list(data1$session), FUN='sd')

write.csv(data1, file="Group_Attempt_Data.csv")
