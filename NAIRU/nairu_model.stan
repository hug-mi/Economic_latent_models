//
// Stan code for the NAIRU model, to be used in run_nairu_model.R
//

// nairu_model.stan --------------------------------------------------
// v1.0 May 2021
// Authors: Hugh Miller, Isabella Lyons
// Code used to run the MAIRU  model
// To accompany paper "Grasping at the invisible – Bayesian models for estimating latent economic variables"

// Input data
data {
  int<lower=0> N;
  vector[N] pi; //Inflation
  vector[N] pe; // Inflation expectations
  vector[N] ulc; // Unit labour costs
  vector[N] u; // Unemployment
  vector[N] pm; // Import prices
  vector[N] oil; // Oil prices
  vector[N] d; // Dummy indicator for oil parameter
}

// The parameters accepted by the model
parameters {
  
  // error terms
  vector[N] nairuer;
  real<lower=0> ep_p;
  real<lower=0> ep_ulc;
  real<lower=0, upper=0.5> ep_nairu;
  
  // inflation equation parameters
  real deltap;
  real beta1;
  real beta2;
  real beta3;
  real<lower=0> phi;
  real gammap;
  real lambdap;
  real alpha1;
  real psip;
  
  // labour cost growth parameters 
  real deltaulc;
  real omega1;
  real omega2;
  real gammaulc;
  real lambdaulc;
  real psiulc; 
}

// The model to be estimated
transformed parameters {
  vector[N] nairu;
  
  // nairuer~normal(0,1), so this is equivalent to nairu[t]~normal(nairu[t-1], ep_nairu)
  nairu[1]=nairuer[1];
  for(t in 2:N)
   nairu[t]=nairu[t-1]+nairuer[t]*ep_nairu;
  
}

model {
  // error terms
  ep_p ~inv_gamma(2.180,0.354);  
  ep_ulc ~inv_gamma(4,3); 
  ep_nairu ~inv_gamma(3,1); 

  // inflation equation  parameters
  deltap ~ normal(0.4,1);
  beta1 ~ normal(0.3,1);
  beta2 ~ normal(0.2,1);
  beta3 ~ normal(0.05,1); 
  phi ~ normal(0.05,1);
  gammap ~ normal(-0.2,0.5);
  lambdap ~ normal(-0.4,1);
  alpha1 ~ normal(0.01,1);
  psip ~ normal(0.1,1);
  
  // labour cost growth parameters 
  deltaulc ~ normal(0.7,1);
  omega1 ~ normal(0.5,1);
  omega2 ~ normal(0.1,1);
  gammaulc ~ normal(-5,1);
  lambdaulc ~ normal(-2,1);
  psiulc ~ normal(0.05,1);
  
  
  // Error terms for NAIRU
  nairuer[1]~normal(3,2); 
  for(t in 2:N)
  nairuer[t]~std_normal();

  // inflation
  for (t in 4:N)
  pi[t] ~ normal(deltap*pe[t]+beta1*pi[t-1]+beta2*pi[t-2]+beta3*pi[t-3]+phi*ulc[t-1]+gammap*(u[t]-nairu[t])/u[t]+lambdap*(u[t-1]-u[t-2])/u[t]+alpha1*(pm[t-1]-pm[t-2])+d[t]*psip*oil[t-2],ep_p);
  
  // labour cost growth
  for (t in 3:N)
  ulc[t] ~ normal(deltaulc*pe[t]+omega1*pi[t-1]+omega2*pi[t-2]+gammaulc*(u[t]-nairu[t])/u[t]+lambdaulc*(u[t-1]-u[t-2])/u[t]+d[t]*psiulc*oil[t-2],ep_ulc);
}



