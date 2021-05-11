
# run_nairu_model ---------------------------------------------------------

# Code used to run the NAIRU model

# v1.0 May 2021
# Authors: Hugh Miller, Isabella Lyons
# Code used to run the Neutral interest rate model
# To accompany paper "Grasping at the invisible – Bayesian models for estimating latent economic variables"


# This requires:
# - nairu_data.csv, containing the data used to fit the model
# - nairu_model.stan, containing stan code specifying the model
# - utility, containing summary functions for assessing the model fit

# Load packages and data --------------------------------------------------

library("rstan") 
source("utility.R")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# read in the data
au.data <- read.table("nairu_data.csv",
                      sep = ',', na.strings = ".", header=TRUE, stringsAsFactors=FALSE)


# Fit the model -----------------------------------------------------------

# Extract data for the time period the model is to be fit over - the default is from March 83
use_data <- au.data[au.data$date >="1983-03-31",]

au.inflation              <- use_data$inflation
au.inflation.expectations <- use_data$inflation.expectations
au.labour.costs.growth    <- use_data$labour.costs.growth
au.unemployment           <- use_data$unemployment
au.imports.price.growth   <- use_data$imports.price.growth
au.oil.price.change       <- use_data$oil.price.change
au.dummy                  <- use_data$dummy
N <- length(au.unemployment)

# Load the stan code and run the model
stancode <- rstan::stan_model("nairu_model.stan", verbose = FALSE)

myiter <- 2000 # Total number of iterations including warm-up

nairu_fit <- sampling(stancode,
                      iter = myiter,
                      warmup = myiter-1000,
                      seed = 1547, 
                      data = list(N  = N,
                                  pi  = au.inflation,
                                  pe = au.inflation.expectations,
                                  ulc= au.labour.costs.growth,
                                  u  = au.unemployment,
                                  pm = au.imports.price.growth,
                                  oil= au.oil.price.change,
                                  d  = au.dummy),
                      control = list(max_treedepth = 10,
                                     adapt_delta=0.97)
                      )

# Save the model and view outputs -----------------------------------------

# Save the model
save(nairu_fit, file = "nairu_fit.rdata")

# Plot the NAIRU estimates and get parameter estimates
nairu_sum <- summarise_model(nairu_fit, au.unemployment, plot = TRUE)
write.csv(nairu_sum, "nairu_model.csv")


# See https://cran.r-project.org/web/packages/rstan/vignettes/rstan.html for
# info on rstan and diagnostics

