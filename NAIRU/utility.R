#############################
## Additional Utility macros 
#############################
# summarise_model, a function to extract parameter estimates and plot the nairu
# estimates produced by the model

summarise_model <- function(model,
                            au.unemployment,
                            plot = TRUE) {
  para_summ  <- summary(model, pars = c("deltap","beta1","beta2","beta3","phi",
                                         "gammap","lambdap","alpha1","psip",
                                         "deltaulc","omega1", "omega2", "gammaulc",
                                         "lambdaulc","psiulc", "ep_p","ep_ulc",
                                         "ep_nairu"))$summary
  nairu_summ <- summary(model, pars = c("nairu"))$summary
  N <- length(au.unemployment)
  if(plot){
    plot(1:N, nairu_summ[,1], type='l', ylim=c(3,12), ylab="", xlab="")
    lines(1:N, au.unemployment, col="blue")
    legend("topright",legend = c("NAIRU", "Unemployment"),col=c("black", "blue"),lty=1:1)
  }
  
  return(rbind(para_summ, nairu_summ))
}

