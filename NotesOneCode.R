#Model Fitting and Illustration of the Likelihood Function

car <- read.table("Car.csv", sep = ",", header = TRUE)
car

#Model fit - proba that household will buy car
glm1 <- glm(y~., family = binomial(link = "logit"), data = car)
summary(glm1)

#For predict default type will return log odds (p on a logit scale)
#While type = "response" will return the predicted probabilities. 
p_hat <- predict(glm1, type = "response")

df <- data.frame(car, p_hat =round(p_hat, 3))

y <- car$y

#phat.proba <- ifelse(p_hat >= 0.5, 1, 0)
#table(phat.proba, y) #Check classification accuracy.

#Plot observation where car was bought
plot(car$car_age[y==1], car$income[y==1], col = "red",
     pch = 15, xlab = "car_age", ylab = "income", 
     xlim = c(1,6), ylim = c(10,100))







#Plot other observations
points(car$car_age[y==0], car$income[y==0], col = "black", pch = 19)
