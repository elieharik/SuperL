library(boot)
MLC <- read.table("MLC.csv", sep = ",", header = TRUE)

#Bootsrappiong parameters SE/CIs 

#MLCfit will return the parameter estimates 
#THIS is what we want to get an estimate for by bootsrappin'
MLCfit <- function(Z, i, theta0) {
  Zboot <- Z[i,]
  x1 <- Zboot[[1]] #double brackets will return you the object itself
  x2 <- Zboot[[2]]
  y <- Zboot[[3]]
  fn <- function(p) {
    yhat <- p[1] + p[2] * x1 + p[4] * exp(p[3] * x2)
    sum ((y-yhat)^2)
  }
  out <- nlm(fn, p = theta0) #Reminder: p is the starting parameter vals for the minimization
  theta <- out$estimate
}
#MLC: our data
#MLCfit: function that returns the statistic of interest. 
#R is number of bootstrap replicates
MLCboot <- boot(MLC, MLCfit, R = 5000, theta0 = c(1,-0.05, -0.14, -0.55))
CovTheta <- cov(MLCboot$t) #to get the standard error of the parameter estimates
SE <- sqrt(diag(CovTheta)) #Now that's how you get only diag values
MLCboot
CovTheta
SE #Only look at diagonal vals. 

 #Let's plot this 
plot(MLCboot, index = 1) #index = i calculates results for ith paramter
boot.ci(MLCboot, conf = c(.9,.95,.99), index = 1, type = c("norm", "basic")) #Get the bootsrap CIs
#norm gives our crude CI, basic gives the better CI reflected by percentiles. 




#How about using Fisher Info??

x1<-MLC$Location;x2<-MLC$Week;y<-MLC$Efficiency

fn <- function(p) {
  yhat <- p[1] + p[2] * x1 + p[4] * exp(p[3] * x2)
  sum ((y-yhat)^2)
}

out <- nlm(fn, p =c(1,0,-.5,-.1), hessian = TRUE) 
theta <- out$estimate
theta

#Calculate SE:
MSE <- out$minimum/(length(y)-length(theta)) #Estimate error variance
InfoMat <- out$hessian/2/MSE #Observed info matrix
CovTheta <- solve(InfoMat)
SE <- sqrt(diag(InfoMat))
MSE
CovTheta
SE

#Use Fisher info matrix when n is large enough and want expression for SE.
#if n is not large enough, use bootstrappong. 
#If there is no underlying probabilistic model for the data -> no likelihood or MSE -> bootstrapping. 
#Model nonparametric -> bootstrappin
#For SEs for the predicted response CIs or PIs with nonlinear models -> bootstrapping. 




#What if want to boostrap response for CIs?? 
MLCfit2 <- function(Z, i, theta0, x_pred) {
  Zboot <- Z[i,]
  x1 <- Zboot[[1]] #double brackets will return you the object itself
  x2 <- Zboot[[2]]
  y <- Zboot[[3]]
  fn <- function(p) {
    yhat <- p[1] + p[2] * x1 + p[4] * exp(p[3] * x2)
    sum ((y-yhat)^2)
  }
  out <- nlm(fn, p = theta0) #Reminder: p is the starting parameter vals for the minimization
  theta <- out$estimate
  
  #NEW LINE: 
  #Predicted response. 
  y_pred <- theta[1] + theta[2] * x_pred[1] + theta[4] * exp(theta[3] * x_pred[2])
}

MLCboot2<-boot(MLC, MLCfit2, R=5000, theta0=c(1,-.05,-.14,-.55), x_pred=c(1,15)) 
VarYhat<-var(MLCboo2t$t)
SEYhat<-sqrt(VarYhat)
MLCboot2
VarYhat
SEYhat
plot(MLCboot2) 
boot.ci(MLCboot2,conf=c(.9,.95,.99),type=c("norm","basic"))

#Difference here is that we are asking CI on the predicted response for a specified
#set of values of predictors (here 1 and 15)


#Bootsrapping PI's instead of CI
Yhat0 <- MLCboot$t0 #returns the observed value of statistic applied to data. 
Yhatboot <- MLCboot2$t
e <- rnorm(nrow(Yhatboot), mean = 0, sd = sqrt(MSE)) #MSE from before #rnorm gives density function 4 specified means and sds. 
Yboot <- Yhatboot-e
SEY <- sqrt(var(Yboot)) #Get SD (SE)
Yquant <- quantile(Yboot, prob=c(0.25, .975)) #Get desired quantiles
L <- 2* Yhat0 - Yquant[2] #Get lower bound
U <- 2* Yhat0 - Yquant[1] #Get upper bound
hist(Yboot,10)
c(L,U)
SEY
