library(doParallel)
library(foreach)

# Calculate the number of cores
getDoParWorkers()                                          
detectCores()                                                      
cl=makeCluster(30)                                              
registerDoParallel(cl)                                                                
getDoParWorkers()   

#for(m in 1:sim)
myfunc = function(m)
{
  options(warn=-1)
  library(geepack);library(MASS);library(ResourceSelection);library(ltmle); library(SuperLearner)
  library(dplyr); library(lme4); library(glm2)
  library(data.table)
  setDTthreads(1)
  library(nleqslv)
  library(mgcv)
  
  #library(reshape2)  #do not use for data frame only
  
  logit <- function(term) {
    return( ifelse(!is.na(term),log(term/(1-term)),NA) )
  }
  
  EXPIT <- function(term) {
    return( ifelse(!is.na(term),exp(term)/(1+exp(term)),NA) )
  }
  
  source("datagen.R")
  set.seed(112789)
  seeds = floor(runif(1000)*10^8);
  set.seed(seeds[m])
  
  nobs <- 2500; intcpt <- -5.85
  df <- datagen(nobs, intcpt)
  dffull = df[df$dat==0,]
  dtrial = df[df$dat==1,]
  nrct = nrow(dtrial)

  ### estimate Ya1
  tmpry = df[df$dat==0,]
  fitY = gam(Y ~ M + s(L1) + s(L2) + s(L3) + s(L1, by=M) + s(L2, by=M) + s(L3, by=M) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), family = binomial(), data=tmpry)
  df$Ya1 = predict(fitY, newdata=df, type="response")
  
  tmpry = df[df$dat==1 & df$A==1,]
  fitY2 = gam(Ya1 ~ s(L1) + s(L2) + s(L3) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), family = binomial(), data=tmpry)
  df$Ya2 = predict(fitY2, newdata=df, type="response")
  
  tmpry = df[df$dat==0,]
  meanres = mean(tmpry$Ya2) 
  meanres
  
  return(meanres)
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"gcomp_gam.csv")

stopCluster(cl)