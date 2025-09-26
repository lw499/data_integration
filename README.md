This repository contains code from the paper "Estimating average treatment effects when treatment data are absent in a target study", co-authored by Wen and Sarvet. 

The folder contain codes to generate data for data generating mechanism described in the main manuscript. The files to reproduce the results found in the main paper (and in the supplementary materials) include:

datagen.R: contains the function to generate data sets 
deterministic_datagen_wide_true.R: code to produce the true parameter estimates 
tmle_.R: codes to produce the parameter estimates and standard errors from the TMLE estimators described in the main manuscript 
ICE_.R: codes to produce the parameter estimates and standard errors from an ICE estimator 
IPW_.R: codes to produce the parameter estimates and standard errors from an IPW estimator

