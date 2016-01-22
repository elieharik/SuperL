library(boot)
car <- read.table("Car.csv", sep = ",", header = TRUE)
n <- nrow(car)
car.fit <- glm(y ~ income + car_age, family = binomial(link = "logit"), data = car)
summary(car.fit)

#AIC formula
AIC<- -2*as.numeric(logLik(car.fit))/n+2*3/n

#Cross val for above model
out<-cv.glm(CAR, car.fit, function(y,phat) -mean(log(phat)*y+log(1-phat)*(1-y)), K=11)

AIC
car.fit$aic/n
#Same!
out$delta


#Add  interaction term income:car_age
car.fit<-glm(y~income+car_age+income:car_age,family=binomial(link = "logit"),data=CAR) 
summary(car.fit)
AIC<- -2*as.numeric(logLik(car.fit))/n+2*4/n
out<-cv.glm(CAR, car.fit, function(y,phat) -mean(log(phat)*y+log(1-phat)*(1-y)), K=11) 
AIC
car.fit$aic/n 
out$delta
