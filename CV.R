#Creating indices of partition for K-fold CV

##CVInd(n,K) function: 
#n is sample size
#K is number of parts
#Function returns K-length list of indices
##
CVInd <- function(n,K) { 
  m <- floor(n/K) #Approximate size of each part - take floor of sample size / desired parts
  r <- n-m*K
  I <- sample(n,n) #random reordering of the indices - no replacement - size n 
  Ind <- list() #Init list of indices for all K parts
  length(Ind) <- K #e.g. in LOOCV, K = n ;
  for (k in 1:K) { #for all K folds
    if (k <= r) {
      kpart <- ((m+1)*(k-1)+1):((m+1)*k)
    } else {
      kpart <- ((m+1)*r+m*(k-r-1)+1):((m+1)*r+m*(k-r))
    }
    Ind[[k]] <- I[kpart] #gives indices for kth part of the data #LIST #returns vectors of obj
  }
  Ind #return val 
  }
  


#Command for running multiple random replicates of CV
MLC <- read.table("MLC.csv", sep = ",", header = TRUE)


Nrep <- 20 #Number of replicates of CV 
K <- 10 #K-fold CV on each rep
n <- nrow (MLC)
y <- MLC$Efficiency
SSE <- matrix(0, Nrep, 2) #Nrep is rownumber here
for (j in 1:Nrep) {
  Ind <- CVInd(n,K) #previous command that returns K-length list of indices
  yhat1 <- y
  yhat2 <- y
  for (k in 1:K) {
    out <- lm(Efficiency~., MLC[-Ind[[k]],]) #Leave out K part 
    yhat1[Ind[[k]]] <- as.numeric(predict(out, MLC[Ind[[k]],])) #Test out on data in left out K part
    out<-lm(Efficiency ~ . - Location,MLC[-Ind[[k]],]) #some second model to compare 
    yhat2[Ind[[k]]]<-as.numeric(predict(out,MLC[Ind[[k]],])) #same
  } #k loop end
  SSE[j,] = c(sum((y - yhat1)^2), sum((y-yhat2)^2))
}#j loop end
SSE
apply(SSE,2,mean)



#Let's use the above code for K-fold CV for Manual Learning Curve Data
FitFun1 <- function(x1,x2,p) {
  p[1]+p[2]*x1+p[4]*exp(p[3]*x2)
}
FitFun2 <- function(x1,x2,p) {
  p[1]+p[3]*exp(p[2]*x2) #no x1 here. 
}

for (j in 1:Nrep) { 
  Ind<-CVInd(n,K)
  yhat1<-y; yhat2<-y;
  for (k in 1:K) {
    out<-nls(Efficiency~FitFun1(Location,Week,p),
             data=MLC[-Ind[[k]],],start=list(p=c(1,-.05,-.15,-.55)))
    yhat1[Ind[[k]]]<-as.numeric(predict(out,MLC[Ind[[k]],]))
    out<-nls(Efficiency~FitFun2(Location,Week,p),
             data=MLC[- Ind[[k]],],start=list(p=c(1,-.15,-.55)))
    yhat2[Ind[[k]]]<-as.numeric(predict(out,MLC[Ind[[k]],]))
    } #end of k loop
  
  
  SSE[j,]=c(sum((y-yhat1)^2),sum((y-yhat2)^2))
} #end of j loop 
SSE
apply(SSE,2,mean)