# plot a single-subject MBD 

plot_SSD_mbd <- function(y) {
  require(ggplot2)
  require(gridExtra)
  library(Matrix)
  n.cases <- max(y$case)
  maxi <- max(y$time) + 1
  mini <- min(y$time)
  plt <- list()
  time <- as.matrix(aggregate(cbind(count = time) ~ case, 
                              data = y, 
                              FUN = function(x){NROW(x)}))
  treatment <- aggregate(cbind(count = treatment) ~ case, 
                         data = y, 
                         FUN = function(x){nnzero(x)})
  
  baseline <- cbind(time[ ,1], time[ ,2] - treatment[ ,2])
  firsttime <- c(1, (cumsum(time[,2])+1))
  firsttime <- firsttime[-(n.cases+1)]
  for (casenum in 1:n.cases){
  ifelse(treatment[casenum, 2] >= baseline[casenum, 2],
         yre <- rbind(c(y[(firsttime[casenum]: (firsttime[casenum] + baseline[casenum, 2] - 1)), "outcome"], 
                        rep(NA, treatment[casenum, 2] - baseline[casenum, 2])),
                      y[(firsttime[casenum] + baseline[casenum, 2]):(firsttime[casenum] + time[casenum, 2] - 1), "outcome"]), 
         yre <- rbind(y[(firsttime[casenum]: (firsttime[casenum] + baseline[casenum, 2] - 1)), "outcome"],
                      c(y[((firsttime[casenum] + baseline[casenum, 2]):(firsttime[casenum] + time[casenum, 2] - 1)), "outcome"], 
                        rep(NA, baseline[casenum, 2] - treatment[casenum, 2])))
  )
  P <- nrow(yre)  
  T <- apply(yre, 1, function(x) max(which(!is.na(x))))
  
  observations <- rep(NA, sum(T))
  k = 1
  for (i in 1:P) {
    for (j in 1:T[i]) {
      observations[k] = yre[i, j]
      k = k + 1
    }
  }
  changepoints <- cumsum(T)
  
  DF <- data.frame(
    phase = rep(seq(P), times = T),
    time = seq(sum(T)),
    shame = observations
  )


  
  plt[[casenum]] <- ggplot(DF, aes(x = time, y = shame)) +
    geom_point() +
    geom_line(aes(group = phase)) +
    geom_vline(xintercept = head(changepoints, -1) + 0.5, linetype = "dotted") +
    theme_bw() + ylim(0, max(y$outcome)) + 
    labs(title = paste0("Subject ", casenum)) + xlim(mini, maxi)
  
}
quartz()
grid.arrange(plt[[1]], plt[[2]], plt[[3]], 
             nrow = 3)
}
