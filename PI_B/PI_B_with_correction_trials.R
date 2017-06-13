#list rat number here
ratList <- 101:112

#make a data frame for the AB acquisition days
sessList <- 70:73list session nubmers here
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
    if(file.exists(filename)){
      print(filename)
      currentData <- read.csv(filename, header=TRUE, sep="\t")
      currentData$resp2 <- ifelse(currentData$Attempts == "1", 1,0)
      firstAttempts <- mean(currentData$resp2)
      totalAttmpts <- mean(currentData$Attempts)
      MedianAttmpts <- median(currentData$Attempts)
      data1[i,0:5] <- c(rat, sessNum, firstAttempts,totalAttmpts,MedianAttmpts)}
    i=i+1
  }
}
#making the matrix into a data frame and adding lesion condition
data1 <- data.frame(data1) #making it into a data frame
data1$ratID <- factor(data1$ratID) #making rat number a factor

firstAttempts <- aggregate(data1$firstAttempt, by=list(data1$session), FUN='mean')
SDs<-aggregate(data1$firstAttempt, by=list(data1$session), FUN='sd')
totalAttempts<- aggregate(data1$meanAttempts, by=list(data1$session), FUN='mean')
total_SDs <- aggregate(data1$meanAttempts, by=list(data1$session), FUN='sd')

data2<-as.data.frame(firstAttempts)
colnames(data2)[1] <- "session"
colnames(data2)[2] <- "firstAttempts"
data2$firstAttempts_SD <- SDs$x
data2$totalAttempts <- totalAttempts$x
data2$totalAttempts_sD <- total_SDs$x


write.csv(data1, file="Group_Attempt_Data.csv")
write.csv(data2, file="Mean_first_Attempts.csv")