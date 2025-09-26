#a=1
set.seed(100000)
rexpit <- function(x) rbinom(n=length(x), size=1, prob=plogis(x))
N <- 100000000
intcpt <- -5.85

#Intervention A=1
L1 = rnorm(N, 0, 1)
L2 = rnorm(N, 0, 1)
L3 = runif(N, 0, 2)

samp_prob = rbinom(N, 1, plogis(intcpt+L1+L2+L3-0.5*L3^2))
ids_ = which(!samp_prob==1) ## generating "S" not in trial

A = rep(1,N)
M = rbinom(N, 1, plogis(-3+2*A+2*L1+L2+L3))
Y = rbinom(N,1,plogis(1+2*M+L1+L2-L3-L2*L3-L2^2))

mean(Y[ids_])
# 0.511519


