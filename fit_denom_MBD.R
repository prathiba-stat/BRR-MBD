fit.denom.MBD <- function(data){
  library(runjags) #load runjags
  y <- data$y #extract the outcome variable y
  nsub <- max(data$sub) #extract the number of participants
  markers <- matrix(NA, nsub, 4) #create a dummy matrix so we know phase lengths for 
                     #each participant
  colnames(markers) <- c("base", "treat", "start", "end")
         #baseline phase length, treatment phase length, 
         #corresponding numeric positions on the y vector for the start and end of
         #the observations for each participant
  for (i in 1:nsub){
    markers[i,1:2] <- c(sum(data[data$sub==i,]$x==1), 
                        sum(data[data$sub==i,]$x==2))

  }
  sums <- rowSums(markers, na.rm = T) #total number of observations for each participant
  t <- c(1, cumsum(sums) + 1) #starting point of observation for each participant
  markers[,3] <- t[1:nsub]
  markers[,4] <- cumsum(sums)
  pois.kunz.MBD <- "model{ #define the MBD model
  ## Likelihood
   for (i in 1:nsub){ #repeat this process for each participant
    Rhat[i] ~ dnorm(beta[i], prec) #Rhat is the log of the rate ratio
    R[i] <- exp(Rhat[i]) #R is the rate ratio
    beta[i] ~ dnorm(gamma00, prec2) #random effect of RR for each participant
   } #end of first loop 
  gamma00 ~ dnorm(0, 1) #fixed effect of RR
  prec <- 1/(sigma^2) #precision for level 1 model
  prec2 <- 1/(tau^2) #precision for level 2 model
  sigma ~ dgamma(1, 1) 
  tau ~ dgamma(1, 1)
  tau2 <- tau^2 #variation between participants
  sigma2 <- sigma^2 #variation within participants
  ICC <- sigma2/(tau2+sigma2) #intraclass correlation
  for (i in 1:nsub){ #repeat this process for each participant
    y[markers[i,3]] ~ dpois(thetahat[markers[i,3]]) 
    #y for the first observation in baseline for each participant is 
    #Poisson distributed with thetahat parameter
    y[markers[i,3] + markers[i, 1]] ~ dpois(thetahat[markers[i,3]] + markers[i, 1])
    #y for the first observation in treatment for each participant is 
    #Poisson distributed with thetahat parameter
    thetahat[markers[i,3]] <- theta[i,1]
    thetahat[markers[i,3] + markers[i,1]] <- theta[i,2]
    #thetahat equals theta for first observation in baseline and treatment phases
    #for each participant
    theta[i,1] <- lambda[i,1] * (markers[i,3] + markers[i,1] - 1)
    #for baseline phase theta is a function of lambda[,1] and baseline phase length
    theta[i,2] <- lambda[i,2] * (markers[i,3] + markers[i,1])
    #for treatment phase theta is a function of lambda[,2] and treatment phase length

    for (j in (markers[i,3] + 1): (markers[i,3] + markers[i,1] - 1)){
    #begin a new nested loop across all time-points except the first
    #time-point of the baseline phase
      y[j] ~ dpois(thetahat[j]) #y is poisson distributed 
      thetahat[j] <- (theta[i, 1]*(1-rho)) + (rho*(y[j - 1]))
      #thetahat is a function of theta of baseline and autocorrelation rho
    } #end nested loop
    for (j in (markers[i,3] + markers[i,1] + 1): markers[i, 4]){
    #begin a new nested loop across all time-points except the first
    #time-point of the treatment phase
      y[j] ~ dpois(thetahat[j])
      thetahat[j] <- (theta[i, 2]*(1-rho)) + (rho*(y[j - 1]))
      #thetahat is a function of theta of treatment and autocorrelation rho
    } #end nested loop
  } #end the outer loop
  
  #defining lambdas (event rates)
   for (i in 1:nsub){
     lambda[i,1] ~ dunif(0, 100) # event rate for baseline
     lambda[i,2] <- lambda[i,1] * R[i] #event rate for treatment as a function
     #of event rate for baseline and rate ratio
   }
  rho ~ dunif(-1, 1) #prior for autocorrelation
}"
  #now we begin fitting the model in runjags
  results <- autorun.jags(
    model = pois.kunz.MBD, #specify the name of the model
    data = list(y = y, nsub = nsub, markers = markers), #specify the data
    monitor = c("rho",  "theta", "lambda", "R", "beta", "tau2", "sigma2",
                "gamma00", "ICC"), #specify the parameters of interest to monitor
    n.chains = 4, #specify the number of chains
    startsample = 30000, #specify burn-in
    raftery.options = FALSE,
    method = "rjparallel"
  )
  results$draws <- combine.mcmc(results$mcmc) #combine the results across the chains
  results #output the results
}