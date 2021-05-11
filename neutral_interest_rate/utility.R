#############################
## Additional Utility macros 
#############################

# summarise_results, a function to extract parameter estimates and plot the 
# estimates produced by the model

summarise_results <- function(model, plot = TRUE){
  N <- model@par_dims$z
  para_summ  <- data.frame(summary(model, pars = c("a1","a2","ar","beta1","beta2"), probs = c(0.1, 0.5, 0.9,0.99))$summary)
  sigma_summ <- data.frame(summary(model, pars = c("ep_IS","ep_ph","ep_ystar","ep_g","ep_z"), probs = c(0.1, 0.5, 0.9,0.99))$summary)
  
  ystar_summ  <- data.frame(summary(model, pars = c("ystar"), probs = c(0.1, 0.5, 0.9,0.99))$summary,  var = "ystar")
  # for potential output we need to correct the mean adjustment
  ystar_summ[,1] <- ystar_summ[,1]+12.5
  ystar_summ[,4] <- ystar_summ[,4]+12.5
  ystar_summ[,5] <- ystar_summ[,5]+12.5
  ystar_summ[,6] <- ystar_summ[,6]+12.5
  ystar_summ[,7] <- ystar_summ[,7]+12.5
  
  rstar_summ <- data.frame(summary(model, pars = c("rstar"), probs = c(0.1, 0.5, 0.9,0.99))$summary,  var="rstar")

  g_summ <- data.frame(summary(model, pars = c("g"), probs = c(0.1, 0.5, 0.9,0.99))$summary,  var = "g")
  
  z_summ <- data.frame(summary(model, pars = c("z"), probs = c(0.1, 0.5, 0.9,0.99))$summary,  var = "z")
  
  yt_summ  <- data.frame(summary(model, pars = c("yt"), probs = c(0.1, 0.5, 0.9,0.99))$summary,  var = "output gap")
  
  all_results <-rbind.data.frame(cbind(para_summ, var = NA),
                                 cbind(sigma_summ,  var = NA),
                                 ystar_summ,
                                 rstar_summ,
                                 g_summ,
                                 z_summ,
                                 yt_summ)
  if(plot){
    par(mfrow=c(3,2))
    plot(1:N, ystar_summ[,1], type='l', main = "potential output - y star", ylab="")
    plot(1:N, rstar_summ[,1], type='l', main = "rstar", ylab="")
    plot(1:N, g_summ[,1], type='l', main = "trend growth", ylab="")
    plot(1:N, z_summ[,1], type='l', main = "z", ylab="")
    plot(1:N, yt_summ[,1], type='l', main = "output gap", ylab="")
    par(mfrow=c(1,1))
  }
  
  return(all_results)
}
