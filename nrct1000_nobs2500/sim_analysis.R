truer1 = 0.5111739

tmpdat = read.csv("tmle_gam_SE.csv")
tmpdat$X=NULL; tmpdat$V2=NULL
mean = colMeans(tmpdat)
gformseq = mean
gformseq2 = tmpdat;
gformseqSE = apply(gformseq2, 2, var)^0.5
gformseqVAR = gformseqSE^2

bpgformseq = (gformseq-truer1)/gformseqSE
bgformseq = (gformseq-truer1)
MSE = bgformseq^2 + gformseqVAR
cbind(bgformseq*100, gformseqSE*100, bpgformseq*100, sqrt(MSE))

#####
sedat = read.csv("tmle_gam_SE.csv")
mdf = sedat[sedat$V2<100,]
low = mdf$V1 - qnorm(0.975)*mdf$V2
hi = mdf$V1 + qnorm(0.975)*mdf$V2

counts=0
counts = ifelse(low<=truer1 & hi>=truer1,1,0)
sum(counts)/nrow(mdf)
nrow(mdf)

###
tmpdat = read.csv("gcomp_gam.csv")
tmpdat$X=NULL;
mean = colMeans(tmpdat)
gformseq = mean
gformseq2 = tmpdat;
gformseqSE = apply(gformseq2, 2, var)^0.5
gformseqVAR = gformseqSE^2

bpgformseq = (gformseq-truer1)/gformseqSE
bgformseq = (gformseq-truer1)
MSE = bgformseq^2 + gformseqVAR
cbind(bgformseq*100, gformseqSE*100, bpgformseq*100, sqrt(MSE))


tmpdat = read.csv("ipw_gam.csv")
tmpdat$X=NULL;
mean = colMeans(tmpdat)
gformseq = mean
gformseq2 = tmpdat;
gformseqSE = apply(gformseq2, 2, var)^0.5
gformseqVAR = gformseqSE^2

bpgformseq = (gformseq-truer1)/gformseqSE
bgformseq = (gformseq-truer1)
MSE = bgformseq^2 + gformseqVAR
cbind(bgformseq*100, gformseqSE*100, bpgformseq*100, sqrt(MSE))

