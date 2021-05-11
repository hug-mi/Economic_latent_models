# run_neutral_rate_model --------------------------------------------------

# v1.0 May 2021
# Authors: Hugh Miller, Isabella Lyons
# Code used to run the Neutral interest rate model
# To accompany paper "Grasping at the invisible – Bayesian models for estimating latent economic variables"

# This requires:
# - neutral_rate_data.csv, containing the data used to fit the model
# - neutral_rate_model.stan, containing stan code specifying the model
# - utility, containing summary functions for assessing the model fit

# Note that this model will not work well when including quarters beyond March
# 2020 due to the effects of COVID-19 on the economy

# Load packages and data --------------------------------------------------

library("rstan") 
source("utility.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Read in data
au.data <- read.table("neutral_rate_data.csv",
                      sep = ',', na.strings = ".", header=TRUE, stringsAsFactors=FALSE)

# Fit the model -----------------------------------------------------------

# Extract data for the time period the model is to be fit over - the default is from March 65
# We also exclude pandemic affected quarters
use_data <- au.data[au.data$date >="1965-03-31",]
use_data <- use_data[use_data$date < "2020-06-30",] 

au.log.output             <- use_data$gdp.log - 12.5  # Rough mean adjustment helps a fair amount with initialization
au.inflation              <- use_data$inflation
au.inflation.expectations <- use_data$inflation.expectations 
au.nominal.interest.rate  <- use_data$interest
au.real.interest.rate     <- au.nominal.interest.rate - au.inflation.expectations
N <- length(au.real.interest.rate)


# Load the stan code and run the model
stancode <- stan_model("neutral_rate_model.stan", verbose = FALSE)

myiter <-3000 # Total number of iterations including warm-up

neutral_fit <- sampling(stancode,
                        iter = myiter,
                        warmup = 2000,
                        seed = 1546,
                        data = list(N = N,
                                   y  = au.log.output, 
                                   pi = au.inflation, 
                                   r  = au.real.interest.rate,
                                   ie = au.inflation.expectations),
                        control = list(max_treedepth = 14, 
                                       adapt_delta = 0.99)
                        ) 

# Save the model and view outputs -----------------------------------------

save(neutral_fit, file ="neutral_fit.rdata")

# Plot the neutral rate estimates and get parameter estimates
neutral_sum <- summarise_results(neutral_fit, plot = TRUE)
write.csv(neutral_sum, "neutral_rate_model.csv")

# See https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html for
# info on rstan and diagnostics