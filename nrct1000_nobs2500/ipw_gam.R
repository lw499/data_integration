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
  library(mgcv)
  library(nleqslv)
  
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
  df <- datagen(nobs,intcpt)
  dffull = df[df$dat==0,]
  dtrial = df[df$dat==1,]
  nrct = nrow(dtrial)
  
  ### estimate propensities
  afitsml = gam(A ~ M + s(L1) + s(L2) + s(L3) + s(L1, by=M) + s(L2, by=M) + s(L3, by=M) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), family=binomial, data=dtrial)
  afitsl = glm2(A~1, family = binomial(), data=dtrial)
  df$apredsml = predict(afitsml, newdata=df, type="response")
  df$apredsl = predict(afitsl, newdata=df, type="response")
  
  ## estimate source allocation
  sfitml =  gam(dat ~ M + s(L1) + s(L2) + s(L3) + s(L1, by=M) + s(L2, by=M) + s(L3, by=M) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), family=binomial, data=df)
  sfitl = gam(dat~ s(L1) + s(L2) + s(L3) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), family=binomial, data=df)
  df$s1predml = predict(sfitml, newdata=df, type="response")
  df$s0predml = 1-df$s1predml
  df$s1predl = predict(sfitl, newdata=df, type="response")
  df$s0predl = 1-df$s1predl
  
  dffull = df[df$dat==0,]
  dffull$wt = (dffull$apredsml*dffull$s1predml*dffull$s0predl)/(dffull$apredsl*dffull$s1predl*dffull$s0predml)
  fit = glm2(Y~1,weight=wt, family = binomial(), data=dffull)
  meanres = plogis(coefficients(fit)[1])
  meanres
  
  return(meanres)
}
test = foreach(m=1:1000) %dopar% myfunc(m)
test2 = do.call("rbind", test)

write.csv(test2,"ipw_gam.csv")

stopCluster(cl)