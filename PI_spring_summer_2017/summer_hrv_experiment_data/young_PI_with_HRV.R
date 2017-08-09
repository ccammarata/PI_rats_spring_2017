options(contrasts=c("contr.sum", "contr.poly"))
library(foreign)
library(psych)
library(reshape2)
library(ggplot2)
library(car)
library(ez)
library(Rmisc)
library(gmodels)
library(package)
library(afex)
library(wesanderson)
# for AB acquisition
sessList <- list(136,137,138,139)#list session nubmers here
ratList <- list(101,102,103,104,105,106,107,108,109,110,112)#list rat number here
x <- length(sessList)
y <- length(ratList)
rowsNeeded <- (x*y)


ABsummary <- matrix(data = NA, nrow = rowsNeeded, ncol=3)
colnames(ABsummary)<- c('ratID', 'session', 'accuracy')

i = 1
for (sessNum in sessList){
  for (rat in ratList) {
filename <- paste("U3_",rat,"_",sessNum,".csv", sep="")
    print(filename)
    currentData <- read.csv(filename, header=TRUE, sep="\t")
    currentData$resp2 <- ifelse(currentData$Response == "correct", 1,0)
    currentAB <- subset(currentData, trialType == "AB")
    ABacc <- mean(currentAB$resp2)
    ABsummary[i,0:3] <- c(rat, sessNum, ABacc)
    i=i+1
    
  }
}
ABsummary <- data.frame(ABsummary)
ABsummary$ratID <- as.factor(ABsummary$ratID)
ABsummary$session <- as.factor(ABsummary$session)

ABsummaryTable <- aggregate(accuracy~session, data=ABsummary, FUN=(mean))
tempTable <- aggregate(accuracy~session, data=ABsummary, FUN=(sd))
ABsummaryTable<- as.data.frame(ABsummaryTable)
ABsummaryTable$sd <- tempTable$accuracy*100
tempTable$accuracy <- tempTable$accuracy/sqrt(y)
ABsummaryTable$se <- tempTable$accuracy*100
ABsummaryTable$accuracy <- ABsummaryTable$accuracy * 100


#for full task
sessList <- list(140,141,142,143,144)#list session nubmers here
ratList <- list(101,102,103,104,105,106,107,108,109,110,112)#list rat number here
x <- length(sessList)
y <- length(ratList)
rowsNeeded <- (x*y)

FULLsummary <- matrix(data = NA, nrow = rowsNeeded, ncol=5)
colnames(FULLsummary)<- c('ratID', 'session', 'AB_acc', 'AC_acc', 'DE_acc')

i = 1
for (sessNum in sessList){
  for (rat in ratList) {
    filename <- paste("U3_",rat,"_",sessNum,".csv", sep="")
    print(filename)
    currentData <- read.csv(filename, header=TRUE, sep="\t")
    currentData$resp2 <- ifelse(currentData$Response == "correct", 1,0)
    currentAB <- subset(currentData, trialType == "AB")
    ABacc <- mean(currentAB$resp2)
    currentDE <- subset(currentData, trialType == "DE")
    DEacc <- mean(currentDE$resp2)
    currentAC <- subset(currentData, trialType == "CA")
    ACacc <- mean(currentAC$resp2)
    FULLsummary[i,0:5] <- c(rat, sessNum, ABacc, ACacc, DEacc)
    i=i+1
    
  }
}

#prepare wide and long form data frames for full task
FULLfinalSummaryW <- data.frame(FULLsummary)
#FULLfinalSummaryW$PI_effect <- FULLfinalSummaryW$DE_acc - FULLfinalSummaryW$AC_acc
z = rowsNeeded*3
FULLfinalSummaryL <- reshape(FULLfinalSummaryW, varying = c('AB_acc', 'AC_acc', 'DE_acc'), v.names="accuracy",
                         timevar='trialType',times=c('AB', 'CA', 'DE'), new.row.names=1:z, direction = 'long')
FULLfinalSummaryL$ratID <-as.factor(FULLfinalSummaryL$ratID)
FULLfinalSummaryL$session <-as.factor(FULLfinalSummaryL$session)
FULLfinalSummaryL$trialType <- as.factor(FULLfinalSummaryL$trialType)
FULLfinalSummaryW$ratID <-as.factor(FULLfinalSummaryW$ratID)
FULLfinalSummaryW$session <-as.factor(FULLfinalSummaryW$session)


#making summary table for full task
FULLsummaryTable <- aggregate(accuracy~trialType, data=FULLfinalSummaryL, FUN=(mean))
tempTable <- aggregate(accuracy~trialType, data=FULLfinalSummaryL, FUN=(sd))
FULLsummaryTable<- as.data.frame(FULLsummaryTable)
FULLsummaryTable$sd <- tempTable$accuracy
tempTable$accuracy <- tempTable$accuracy/sqrt(y)
FULLsummaryTable$se <- tempTable$accuracy
FULLsummaryTable$accuracy <- FULLsummaryTable$accuracy


#part 1 - AB acquisition
#check distribution
pander(describe(ABsummary))
hist(ABsummary$accuracy,main = ('Distribution of Accuracy during AB acquisiton, all sessions'),xlab = ('AB accuracy'))
#distribution is actually a bit strange, though all above chance - also pretty small number of rats
plot(accuracy~session, data=ABsummary, main=('AB accuracy by session, AB acquisition'))
# no interesting results, though by the end of the first session the group average was almost 80% accuracy

#part 2 - full task
pander(describe(FULLfinalSummaryW))
hist(FULLfinalSummaryL$accuracy, main = ('Distribution of Overall Accuracy, Full Task Days'),xlab=('Overall Accuracy'))
#accuracy for the full task is normally distributed 
plot(accuracy~session, data=FULLfinalSummaryL,main=('Overall accuracy by session, full task days'))
#looks like session didn't ahve much effect
p1= ggplot(data=FULLfinalSummaryL, aes(trialType,accuracy))
p1+geom_boxplot(aes(fill=trialType)) +
  coord_cartesian(ylim=c(0,1))+
  ggtitle("Accuracy by Trial type, averages across full task days") +
  labs(x="Trial Type",y="Accuracy")+
  scale_fill_manual(values=wes_palette("FantasticFox"))+
  guides(fill=guide_legend(title="trial type"))

ggplot(FULLsummaryTable, aes(x=trialType, y=accuracy, fill=trialType)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  coord_cartesian(ylim=c(0,1))+
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se), width=.2,
                position=position_dodge(.9)) +
  scale_fill_manual(values=wes_palette("FantasticFox"))+
  ggtitle("Accuracy by trial trype, averaged across days")+
  guides(fill=guide_legend(title="trial type")) +
  labs(x="Trial Type",y="Accuracy")

FULLsummaryTable <- aggregate(accuracy~trialType+session, data=FULLfinalSummaryL, FUN=(mean))
tempTable <- aggregate(accuracy~trialType+session, data=FULLfinalSummaryL, FUN=(sd))
FULLsummaryTable<- as.data.frame(FULLsummaryTable)
FULLsummaryTable$sd <- tempTable$accuracy
tempTable$accuracy <- tempTable$accuracy/sqrt(y)
FULLsummaryTable$se <- tempTable$accuracy
FULLsummaryTable$accuracy <- FULLsummaryTable$accuracy

subSummaryTable <-subset(FULLsummaryTable, trialType != 'AB')

ggplot(subSummaryTable, aes(x=session,y=accuracy,colour = trialType))+
  geom_line(aes(group=trialType))+geom_point()+
  geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se),width=.05)+
  ggtitle('Accuracy by Trial Type and Session, Full Task Days')

ggplot(FULLsummaryTable, aes(x=session,y=accuracy,colour = trialType))+
  geom_line(aes(group=trialType))+
  geom_point()+geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se),width=.1,position=position_dodge(.05))+
  coord_cartesian(ylim=c(.4,1))+
  guides(colour=guide_legend(title="trial type"))+
  scale_color_manual(values=wes_palette("FantasticFox"))+
  ggtitle('Accuracy by Trial Type and Session, Full Task Days')

plot(PI_effect~session,data=FULLfinalSummaryW, main=('PI effect (DE - CA) by session'),ylab = ('PI Effect'))


#inferential tests of full task
qqnorm(FULLfinalSummaryL$accuracy)
qqline(FULLfinalSummaryL$accuracy) #to check for normality of residuals
aov1 <- ezANOVA(data=FULLfinalSummaryL, dv=accuracy, wid = ratID,within=c(trialType,session),detailed=TRUE,return_aov = TRUE)
aov1$`Mauchly's Test for Sphericity` #to quckly check sphericity, which is fine in this case
#I used ezANOVA to get the sphericity test but I can't figure out how to do post hoc analyses with that output
aov2 <- aov_ez("ratID","accuracy",data=FULLfinalSummaryL,within = c("trialType","session"))
summary(aov2)
lsmeans(aov2, "trialType", contr = "pairwise", adjust = "holm")

AB_wide <- dcast(ABsummary, ratID~session, value.var = "accuracy")
AB_wide$last_two_days <- (AB_wide$`138`+ AB_wide$`139`)/2
keepRats <- subset(AB_wide, last_two_days >=.70)
keepRats <- array(keepRats$ratID)

data2 <- subset(FULLfinalSummaryW, ratID %in% keepRats)
data3 <- subset(FULLfinalSummaryL, ratID %in% keepRats)

data3Table <- aggregate(accuracy~trialType+session, data=data3, FUN=(mean))
tempTable <- aggregate(accuracy~trialType+session, data=data3, FUN=(sd))
data3Table<- as.data.frame(data3Table)
data3Table$sd <- tempTable$accuracy
tempTable$accuracy <- tempTable$accuracy/sqrt(y)
data3Table$se <- tempTable$accuracy
data3Table$accuracy <- data3Table$accuracy

ggplot(data3Table, aes(x=session,y=accuracy,colour = trialType))+
  geom_line(aes(group=trialType))+
  geom_point()+geom_errorbar(aes(ymin=accuracy-se, ymax=accuracy+se),width=.1,position=position_dodge(.05))+
  coord_cartesian(ylim=c(.4,1))+
  guides(colour=guide_legend(title="trial type"))+
  scale_color_manual(values=wes_palette("FantasticFox"))+
  ggtitle("High performers' Accuracy by Trial Type and Session, Full Task Days")

# PI_effect_only <- subset(FULLfinalSummaryL, trialType=="PI")
# aov3 <- aov_ez("ratID","accuracy", data=PI_effect_only,within=c("session"))
# summary(aov3)
# lsmeans(aov3, "session", contr = "pairwise", adjust = "holm")

#simple main effects for trial type within each full task session
sess140 <- subset(FULLfinalSummaryL, session == "140")
sess141 <- subset(FULLfinalSummaryL, session == "141")
sess142 <- subset(FULLfinalSummaryL, session == "142")
sess143 <- subset(FULLfinalSummaryL, session == "143")
sess144 <- subset(FULLfinalSummaryL, session == "144")

aov4 <- aov_ez("ratID","accuracy",data=sess140,within=c("trialType"))
aov5 <- aov_ez("ratID","accuracy",data=sess141,within=c("trialType"))
aov6 <- aov_ez("ratID","accuracy",data=sess142,within=c("trialType"))
aov7 <- aov_ez("ratID","accuracy",data=sess143,within=c("trialType"))
aov8 <- aov_ez("ratID","accuracy",data=sess144,within=c("trialType"))

lsmeans(aov4, "trialType", contr = "pairwise", adjust = "holm")
lsmeans(aov5, "trialType", contr = "pairwise", adjust = "holm")
lsmeans(aov6, "trialType", contr = "pairwise", adjust = "holm")
lsmeans(aov7, "trialType", contr = "pairwise", adjust = "holm")
lsmeans(aov8, "trialType", contr = "pairwise", adjust = "holm")

slopeComparison <- matrix(data=NA,nrow=length(keepRats),ncol = 4)
colnames(slopeComparison)<-c("ratID","DE_slope","AC_slope","slope_diff")
sessions <- array(140:142)
i=1
for(rat in keepRats){
subset1 <- subset(FULLfinalSummaryW, session %in% sessions)
subset2 <- subset(subset1, ratID == rat)
subset2$session <- as.character(subset2$session)
subset2$session <- as.numeric(subset2$session)
lmAC <- lm(subset2$AC_acc~subset2$session)
slopeAC<-lmAC$coefficients[[2]]
lmDE<- lm(subset2$DE_acc~subset2$session)
slopeDE<-lmDE$coefficients[[2]]
slopeDiff <- slopeDE-slopeAC
slopeComparison[i,]<-c(rat,slopeDE,slopeAC,slopeDiff)
#plot(subset2$session,subset2$DE_acc, col="red", type="l")
#plot(subset2$session,subset2$AC_acc,col="blue", type="l")
i=i+1
}
slopeComparison<-as.data.frame(slopeComparison)
describe(slopeComparison)

hist(slopeComparison$DE_slope)
hist(slopeComparison$AC_slope)
hist(slopeComparison$slope_diff)
t.test(slopeComparison$DE_slope,slopeComparison$AC_slope, paired = TRUE)
