#time varying case:
#L1: private insurance
#L2: screening of rectal STI (yes/no)
#L3: log transformed CD4 (maximal value of 10)

datagen <- function(nobs, intcpt)
{
  library(data.table); library(dplyr)
  Nsup = 100000
  ids <- seq(1,Nsup)
  L1 = rnorm(Nsup, 0, 1)
  L2 = rnorm(Nsup, 0, 1)
  L3 = runif(Nsup, 0, 2)
  
  mydatatmp = as.data.frame(cbind(ids, L1, L2, L3))
  
  ## sample into randomized trial
  samp_prob = rbinom(nrow(mydatatmp), 1, plogis(intcpt+mydatatmp$L1+mydatatmp$L2+mydatatmp$L3-0.5*mydatatmp$L3^2))
  ids_ = which(samp_prob==1) ## generating "S" who to select into trial
  nrct = sum(samp_prob)
  rctdata = as.data.frame(mydatatmp[ids_,])
  rctdata$A = rbinom(nrct, 1, 0.5)  
  rctdata$M = rbinom(nrct, 1, plogis(-3+2*rctdata$A+2*rctdata$L1+rctdata$L2+rctdata$L3))
  rctdata$Y = NA
  rctdata$dat = 1
  
  ##obs
  obsdata = subset(mydatatmp, !(ids %in% ids_))
  obsdata$A = 0 ## V=0, no need to generate; P(A=1|S=0)=P(A=1|V=0)=0
  obsdata$M = rbinom(nrow(obsdata), 1, plogis(-3+2*obsdata$A+2*obsdata$L1+obsdata$L2+obsdata$L3))
  obsdata$Y = rbinom(nrow(obsdata),1, plogis(1+2*obsdata$M+obsdata$L1+obsdata$L2-obsdata$L3-obsdata$L2*obsdata$L3-obsdata$L2^2))
  obsdata$dat = 0

  ids_obs = sample(seq(1,nrow(obsdata)), size=nobs, replace = FALSE, prob = NULL) 
  collected = as.data.frame(obsdata[ids_obs,])
  
  #samp_prob = rbinom(N, 1, plogis(intcpt+L1+L2+L3-0.5*L3^2))
  #ids_ = which(!samp_prob==1) ## generating "S" not in trial
  
  #A = rep(1,N)
  #M = rbinom(N, 1, plogis(-2+2*A+L1+L2+L3))
  #Y = rbinom(N,1,plogis(1+2*M+L1+L2-L3-L2*L3-0.5*L3^2))
  
  
  temp_data = data.table(rbind(collected, rctdata))
  return(temp_data)
}


