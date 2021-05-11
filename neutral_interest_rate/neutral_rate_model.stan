//
// Stan code for the neutral interest rate model, to be used in run_neutral_rate_model.R
//

// neutral_rate_model.stan --------------------------------------------------
// v1.0 May 2021
// Authors: Hugh Miller, Isabella Lyons
// Code used to run the Neutral interest rate model
// To accompany paper "Grasping at the invisible – Bayesian models for estimating latent economic variables"

// Input data
data {
  int<lower=0> N;
  vector[N] y; // output
  vector[N] pi; // Inflation
  vector[N] r; // Real interest rate
  vector[N] ie; // Inflation expectations
}

// The parameters accepted by the model
parameters {
  
  // error terms
  vector[N] yster;
  vector[N] ger;
  vector[N] zer;
  real<lower=0> ep_IS;
  real<lower=0> ep_ph;
  real<lower=0> ep_ystar;  
  
  // parameters
  real a1;
  real a2;
  real<lower=0.025, upper=1> ar;  
  real<lower=0, upper=1> beta1; 
  real<lower=0.05> beta2;
  real<lower=0> ep_g;
  real<lower=0> ep_z;  
}

// The model to be estimated
transformed parameters {
  vector[N] rstar;
  vector[N] yt;  
  vector[N] z;
  vector[N] g;
  vector[N] ystar;

  // error terms are modelled separately as stan samples better from standard normal distributions,
  // then transformed here 

  z[1]=zer[1];
  g[1]=1+ger[1];
  ystar[1]=y[1]+yster[1];
  
  for(t in 2:N){
     z[t]=z[t-1]+zer[t]*ep_z; // z[t]~normal(z[t-1], ep_z)
     g[t]=g[t-1]+ger[t]*ep_g; // g[t]~normal(g[t-1], ep_g)
     ystar[t]=ystar[t-1]+g[t]/100+yster[t]*ep_ystar/100; // ystar[t]~normal(ystar[t-1]+g[t]/100, ep_ystar)
  }
   
  // rstar and y-tilde
  for(t in 1:N)
    rstar[t]=4*g[t]+z[t];
  for (t in 1:N)
    yt[t]=100*(y[t]-ystar[t]); 
}

model {
  // error terms
  ep_IS ~inv_gamma(2.25,0.625); 
  ep_ph ~inv_gamma(3,2); 
  ep_ystar ~inv_gamma(3,2);
  ep_g  ~inv_gamma(2.062500,0.265625); 
  ep_z  ~inv_gamma(3,2);

  // parameters
  a1 ~ normal(1.1,0.5);
  a2 ~ normal(-0.2,0.5);
  ar ~ inv_gamma(2.0225, 0.153375); 
  beta1 ~ beta(0.5,0.3); 
  beta2 ~ normal(0.5,0.3);
  
  // modelling equations - initial time points
  yster[1]~normal(0,0.5);
  zer[1] ~ normal(0,1);
  ger[1] ~ normal(0,1);
  
  
  // modelling equations
  for (t in 2:N)
   yster[t]~std_normal();
  for (t in 2:N){
    zer[t]~std_normal();
    ger[t]~std_normal();
  }
  
  // IS curve
  for (t in 3:N)
    y[t] ~ normal(ystar[t] + (a1*yt[t-1]+a2*yt[t-2]-ar/2*((r[t-1]-rstar[t-1]) + (r[t-2]-rstar[t-2])))/100, ep_IS); 
  
  // Phillips curve
  pi[1] ~ normal(8,2);
  pi[2] ~ normal(pi[1]+beta2*(yt[1]), ep_ph);  
  pi[3] ~ normal((1-beta1)*ie[2]+beta1*(pi[1])/1+beta2*(yt[2]), ep_ph);  
  pi[4] ~ normal((1-beta1)*ie[3]+beta1*(pi[2]+pi[1])/2+beta2*(yt[3]), ep_ph); 
  for (t in 5:N)
    pi[t] ~ normal((1-beta1)*ie[t]+beta1*(pi[t-1]+pi[t-2]+pi[t-3])/3+beta2*(yt[t-1]), ep_ph); 
} 