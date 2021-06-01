library(dplyr)

setwd("/Users/kseniapanidi/Desktop/MDM/")
data <- read.csv("baseline_KG.csv")
data <- read.csv("S2_test_baseline2.csv")
data <- read.csv("MNbaseline1_hard_TOUCHSCR80ms_260321.csv")
data <- read.csv("test_baseline.csv", sep=';')
data <- read.csv("baseline2.csv", dec=',')
data1 <- read.csv("377.csv", dec=',')
data <- read.csv("for_trial_duration.csv", dec=',')


data <- data.frame(data)
attach(data)

int <- seq(39, 99, 5)
baseline <- c()

for (i in 1:(length(int)-1)){

  block <- filter(data, data[,6]<int[i+1] & data[,6]>=int[i])
  success = count(block, block[,8]!=0)
  baseline = rbind(baseline, success[2,"n"]/nrow(block))
}
baseline


p = c(0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8)
prob = c()
upper = 99
bounds= c()
for (j in 1: length(p)){
  
  for (k in 2:(99-39)){
    
    block <- filter(data, data[,2]<(upper) & data[,2]>=(upper-k))
    success = count(block, block[,4]!=0)
    prob=rbind(prob, cbind(k,success[2,"n"]/nrow(block)))
  }
  #which(prob[which(is.na(prob[,2])==F),2]<=p[j])
  index = which(prob[which(is.na(prob[,2])==F),2]<p[j])
  bounds = cbind(bounds, upper-prob[length(index),'k'])
  upper = upper-prob[length(index),'k']
}



data1 <- read.csv("test_200_50.csv", dec=',')
data1 <- mutate(data1, sure = 50)
data2 <- read.csv("test_600_250.csv", dec=',')
data2 <- mutate(data2, sure = 250)
data3 <- read.csv("test_700_150.csv", dec=',')
data3 <- mutate(data3, sure = 150)
data <- rbind(data1, data2, data3)
data <- mutate(data, risky = ifelse(Item2==Item4, 1, ifelse(Item4==0, NA, 0)))
names(data) = c("trial", "rr", "time", "win" , "total", "dist", "sure","risky")

library(lme4)
library(lmerTest)

data <- mutate(data, trial_all = seq(1, 176,1))
model <- glm( risky ~ I(rr/sure)*dist+trial, data = data, family ='binomial' )
summary(model)

model <- lm( time ~ I(rr/sure)*dist+trial+risky, data = data)
summary(model)


### logit regression for baseline
data1 <- mutate(data1, hit = ifelse(Item4>0, 1, 0))
model <- glm(hit ~ Item1 + Item2, family='binomial', data=data1)
summ(model, digits=5)

model$coefficients
X1_val <- mean(data1$Item1)
X2_range <- seq(from=min(data1$Item2), to=max(data1$Item2), by=.01)
b0=model$coefficients[1]
b1=model$coefficients[2]
b2=model$coefficients[3]
a_logits = b0+X1_val*b1+X2_range*b2
a_probs <- exp(a_logits)/(1 + exp(a_logits))
plot(X2_range, a_probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="distance", ylab="P(hit)", main="Probability of hit")

View(cbind(X2_range, a_probs))



library(ggiraph)
library(ggiraphExtra)
library(ggplot2)
ggPredict(model,se=TRUE,interactive=FALSE)

library(modEvA)
plotGLM(model)


#################################
setwd("/Users/kseniapanidi/Desktop/MDM/12_05_2021")
setwd("/Users/kseniapanidi/Desktop/MDM/29_04_2021")
data1 <- read.csv("200_50.csv", dec=',')
data1 <- mutate(data1, sure = 50)
data2 <- read.csv("600_100.csv", dec=',')
data2 <- mutate(data2, sure = 100)
data3 <- read.csv("700_150.csv", dec=',')
data3 <- mutate(data3, sure = 150)
data4 <- read.csv("900_140.csv", dec=',')
data4 <- mutate(data3, sure = 140)

data=c()
data <- rbind(data1, data2, data3, data4)
#data <- mutate(data, risky = ifelse(Item2==Item4, 1, ifelse(Item4==0, NA, 0)))
data <- mutate(data, risky = ifelse(Item2==Item4, 1, ifelse(Item4==0, 1, 0)))
names(data) = c("trial", "rr", "time", "win" , "total", "dist", "sure","risky")


library(lme4)
library(lmerTest)

model <- glm( risky ~ I(rr/sure)*dist+trial, data = data, family ='binomial' )
summary(model)
summ(model, digits=5)

model <- glm( risky ~ rr+sure*dist+trial, data = data, family ='binomial' )



###############################

library(dplyr)

setwd("/Users/kseniapanidi/Desktop/MDM/")
data1 <- read.csv("test_ksenia_base_149msec.csv", dec=',')
data1 <- read.csv("dima_baseline_309msec.csv", dec='.')

time_lim = 0.149

data1 <- mutate(data1, trial_dur = ifelse(Trial.Duration>0.5, Trial.Duration-0.5, Trial.Duration))
data1 <- mutate(data1, hit = ifelse(trial_dur<time_lim & trial_dur>0, 1, 0))


#data1 <- mutate(data1, hit = ifelse(Trial.Score>0, 1, 0))
#data1 <- mutate(data1, time = ifelse( hit==1, Trial.Duration-0.5, Trial.Duration))
#model <- glm(hit ~ log(ID)+log2(I(2*Risky.Difficulty/4)), family='binomial', data=data1)
model <- glm(hit ~ log(ID)+log(Risky.Difficulty), family='binomial', data=data1)
#model <- lm(time ~ log(ID)+log(Risky.Difficulty), data=data1)
#model <- lm(Trial.Duration ~ log(ID)+log2(I(2*Risky.Difficulty/4)), family='binomial', data=data1)
summ(model, digits=5)

X1_val <- mean(data1$ID)
X2_range <- seq(from=min(data1$Risky.Difficulty), to=max(data1$Risky.Difficulty), by=.01)
#X2_range <- seq(from=min(data1$Trial.Duration), to=max(data1$Trial.Duration), by=.0001)

newdata=data.frame(ID=mean(data1$ID), Risky.Difficulty=X2_range)
#newdata=data.frame(ID=mean(data1$ID), Trial.Duration=X2_range)
a_probs=predict.glm(model, newdata=newdata, type="response")

plot(X2_range, a_probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="distance", ylab="P(hit)", main="Probability of hit")
points(cbind(int[-1], baseline))


probs = cbind(X2_range, a_probs)
probs[which(X2_range==44),]
probs[which(X2_range==50),]
probs[which(X2_range==59),]
probs[which(X2_range==65),]
probs[which(X2_range==69),]
probs[which(X2_range==74),]
probs[which(X2_range==80),]



# model$coefficients
# X1_val <- mean(data1$ID)
# X2_range <- seq(from=min(data1$Risky.Difficulty), to=max(data1$Risky.Difficulty), by=.01)
# b0=model$coefficients[1]
# b1=model$coefficients[2]
# b2=model$coefficients[3]
# b3=model$coefficients[4]
# a_logits = b0+log(X1_val)*b1+ log(X2_range)*b2 + b3*log(X1_val)*log(X2_range)
# a_probs <- exp(a_logits)/(1 + exp(a_logits))
# plot(X2_range, a_probs, 
#      ylim=c(0,1),
#      type="l", 
#      lwd=3, 
#      lty=2, 
#      col="gold", 
#      xlab="distance", ylab="P(hit)", main="Probability of hit")
# 
# View(cbind(X2_range, a_probs))


int <- seq(39, 99, 5)
data1$interval = NA

for (i in 1:(length(int)-1)){
  data1[which(data1[,6]<int[i+1] & data1[,6]>=int[i]),]$interval = (int[i+1]+int[i])/2
}
View(data1)

model <- glm(hit ~ log(ID)+log(interval), family='binomial', data=data1)


X1_val <- mean(data1$ID)
X2_range <- seq(from=min(data1[which(is.na(data1$interval)==F),]$interval), to=max(data1[which(is.na(data1$interval)==F),]$interval), by=.01)


newdata=data.frame(ID=mean(data1$ID), interval=X2_range)
#newdata=data.frame(ID=mean(data1$ID), Trial.Duration=X2_range)
a_probs=predict.glm(model, newdata=newdata, type="response")

plot(X2_range, a_probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="distance", ylab="P(hit)", main="Probability of hit")
points(cbind(int[-1], baseline))


probs = cbind(X2_range, a_probs)
probs[which(X2_range==50),]