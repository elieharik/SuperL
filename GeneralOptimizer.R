#Commands for fitting learning curve using 
#General optimizer nlm()

MLC <- read.table("MLC.csv", sep = ",", header = TRUE)

x1 <- MLC$Location
x2 <- MLC$Week
y <- MLC$Efficiency

fn <- function(p){
  yhat <- p[1]+p[2]*x1+p[4]*exp(p[3]*x2)
  sum((y - yhat)^2)
}


#nlm's first argument is the function that you need to minimize.
#Should be a function with arguments:
#1) vector of the length of p + any other arguments
#Next argument for nlm() is p: the starting param vals 
#to be used in the minimization.
out <- nlm(fn, p= c(1,0,-0.5,-0.1), hessian = TRUE)
theta <- out$estimate #parameter estimates TB used later

#Use this later to find SEs and CIs

#Min gives values of the estimated min of f.
MSE <- out$minimum/(length(y)-length(theta)) #error variance estimation
InfoMat <- out$hessian/2/MSE #observed info matrix
CovTheta <- solve(InfoMat) #Solves system of equations. 
SE <- sqrt(diag(CovTheta)) #SEs of params estimates
MSE
CovTheta
SE



#Commands while using nls() 
#Nonelinear Least Squares
fn2 <- function(x1,x2,p) {
  p[1] + p[2] * x1 + p[4] * exp(p[3] * x2)
}
out2 <- nls(y~fn2(x1,x2,p),
            start = list(p=c(1,0,-0.5,-0.3)),
            trace = TRUE)
summary(out2)
