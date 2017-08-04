library(reshape2)
library(psych)

hrv_data <- read.csv("HRV_PI.csv")

hr_by_rat <- dcast(hrv_data, rat~session,value.var="avg_HR")
hr_by_rat <- data.frame(hr_by_rat)

rmssd_by_rat <- dcast(hrv_data, rat~session,value.var="rmssd")
rmssd_by_rat <- data.frame(rmssd_by_rat)

describe(hr_by_rat)
describe(rmssd_by_rat)

plot(X1~X2,data=rmssd_by_rat)
plot(X1~X2,data=hr_by_rat)

lm1 <- lm(X1~X2,data=rmssd_by_rat)
lm1
summary(lm1)

lm2 <- lm(X1~X2,data=hr_by_rat)
summary(lm2)
