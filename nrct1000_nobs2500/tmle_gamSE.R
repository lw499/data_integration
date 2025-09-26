library(doParallel)
library(foreach)

# Calculate the number of cores
getDoParWorkers()                                          
detectCores()                                                      
cl=makeCluster(50)                                              
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
  library(hal9001);
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
  df <- datagen(nobs,intcpt)
  dffull = df[df$dat==0,]
  dtrial = df[df$dat==1,]
  nrct = nrow(dtrial)
  df$wt3 = (nrow(dffull)/nrow(df))^(-1)
  
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
  df$wt1 = (df$apredsml*df$s1predml*df$s0predl)/(df$apredsl*df$s1predl*df$s0predml)
  df$wt2 = (df$s0predl)/(df$apredsl*df$s1predl)
  
  ### estimate Ya1
  tmpry = df[df$dat==0,]
  fitY = gam(Y ~ M + s(L1) + s(L2) + s(L3) + s(L1, by=M) + s(L2, by=M) + s(L3, by=M) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), family=binomial, data=tmpry)
  tmpry$Ya1 = predict(fitY, new_data=tmpry)
  
  fitYup = glm(Y~1, weight=wt1, offset = Ya1, data=tmpry, family=binomial)
  df$Ya1 = predict(fitY, newdata = df)
  df$Ya1up = predict(fitYup, newdata = df, type="response")
  
  tmpry = df[df$dat==1 & df$A==1,]
  fitY2 = gam(Ya1up ~ s(L1) + s(L2) + s(L3) + ti(L1, L2) + ti(L1, L3) + ti(L2, L3), data=tmpry, family=binomial)
  tmpry$Ya2 = predict(fitY2, newdata=tmpry)
  
  fitY2up = glm(Ya1up~1, weight=wt2, offset = Ya2, data=tmpry, family=binomial)
  df$Ya2 = predict(fitY2, newdata = df)
  df$Ya2up = predict(fitY2up, newdata = df, type="response")
  
  tmpry = df[df$dat==0,]
  df$Ya3 = mean(tmpry$Ya2up)
  
  ### SE
  part1 = (I(df$dat==0)*df$wt1*df$wt3)*(df$Y-df$Ya1up); 
  part1 = ifelse(df$dat==0, part1, 0); sum(part1)
  part2 = (I(df$dat==1 & df$A==1)*df$wt2*df$wt3)*(df$Ya1up - df$Ya2up); 
  part2 = ifelse(df$dat==1 & df$A==1, part2, 0); sum(part2);
  part3 = (I(df$dat==0)*df$wt3)*(df$Ya2up-df$Ya3); 
  part3 = ifelse(df$dat==0, part3, 0); sum(part3)
  
  expected1 = 0;
  for(i in 1:nrow(df))
  {
    expected1 = expected1 + ((part1[i]+part2[i]+part3[i])^2)
  }
  expected1_f = expected1/nrow(df)
  myparam = sqrt(expected1_f/nrow(df))
  myparam
  
  return(c(mean(tmpry$Ya2up),myparam))
}
test = foreach(m=1:1000, .errorhandling = "pass") %dopar% myfunc(m)
blah = lapply(test, function(x) if(is.numeric(x)){x} else{c(NA,NA)}) 
test2 = do.call("rbind", blah)

write.csv(test2,"tmle_gam_SE.csv")

stopCluster(cl)
