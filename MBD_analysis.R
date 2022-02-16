setwd("~/Desktop/Research/PTrauma")
source("fit_denom_MBD.R")
source("plot_SSD_mbd.R")

library(runjags)
library(rjags)
library(ggplot2)
library(SingleCaseES)
fulldata <- read.csv("trauma_mbd.csv")

results <- as.data.frame(summary(fit.denom.MBD(fulldata)))


for (i in 1:3){
  print(NAP(
    fulldata[(fulldata$sub==i)&(fulldata$x==1), "y"],
    fulldata[(fulldata$sub==i)&(fulldata$x==2), "y"],
    improvement = "decrease",
    SE = "unbiased",
    confidence = 0.95
  ))
}


write.csv(results, "Trauma_MBD_results.csv")

#converting data to required format
colnames(fulldata) <- c("time", "treatment", "outcome", "case")
fulldata <- data.frame(fulldata)
fulldata$treatment <- fulldata$treatment - 1
plot_SSD_mbd(fulldata)
