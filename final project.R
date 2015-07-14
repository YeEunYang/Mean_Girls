issdata=read.csv("C:\\Users\\yeeun\\Desktop\\issdata(1).csv",header=T)
summary(issdata)

library(ggplot2) 

explain=issdata[,c(4:103)]


cor(explain)

qplot(explain$NumUnderPov, explain$NumInShelters, 
xlab="NumUnderPov", ylab="NumInShelters",title="Correlation between ¡°MalePctNevMar¡± and ¡°PctWorkMom¡±")

#### 1,2,3 don't need. ViolentCrimesPerPop is response variale

cor(issdata[,c(4:104)])

qplot(explain$FemalePctDiv,violent, colour = "red", 
      size=I(3),
      xlab="FemalePctDiv", ylab="ViolentCrimesPerPop") 


qplot(explain$FemalePctDiv, logit, 
      size=I(3),
      xlab="FemalePctDiv", ylab="logit")



names(issdata)
violent=issdata[,104]
explain=issdata[,c(4:103)]
summary(explain)
logit=issdata[,105]
datalo=as.data.frame(cbind(logit,explain))
length(logit)*0.7

sam=sample(1:1994,1396)

lotrain=datalo[sam,] # training data
lotest=datalo[-sam,] # sample data

fit=glm(logit~.,family=binomial,data=lotrain)
fit1=step(fit)
summary(fit)

library("logistf")
lr2 = logistf(logit ~ pctWFarmSelf + pctWInvInc + NumUnderPov + 
                PctPopUnderPov + PctKids2Par + PctWorkMom + PctIlleg + PctNotSpeakEnglWell + 
                PctHousOccup + MedOwnCostPctIncNoMtg + PctForeignBorn, family = binomial, 
              data = lotrain)
summary(lr2)



pred=predict(fit1,newdata=lotest, type="response")

library("SDMTools")


confusion.matrix(lotest$logit,pred,threshold=0.5) 
accuracy(lotest$logit,pred,threshold=0.5)





