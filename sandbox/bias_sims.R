library(metafor)
library(tidyverse)
source("cca_bias_sim_helperfns.R")

bd <- metafor::dat.bangertdrowns2004

bd %>% summarize_all(.funs = function(x) sum(is.na(x)))

dat <- bd %>% select(yi, vi, x = feedback) %>% na.omit() %>% as_tibble()
df = tibble(x = rbinom(nrow(dat), 1, .65), 
            v = dat$vi)

fm = rma(yi, vi, mods = ~x, data = dat)
beta0 = fm$beta[1]
beta1 = fm$beta[2]
tau2 = fm$tau2

se_beta0 = fm$se[1]
se_beta1 = fm$se[2]
se_tau2 = fm$se.tau2
# gamma0 = -1.65; gamma1 = 0.15; gamma2 = 1.5; gamma3 = 0.2
gamma = c(-1.65, 0, 1.5, 0)



simmed_mods <- list()
nsims <-  1000
for(i in 1:nsims){
  df_sim = df %>%
    mutate(y = rnorm(nrow(df), beta0 + beta1 * x, sqrt(tau2 + v)))

  ad <- ampute_linear(df_sim, gamma)
  
  if(length(unique(ad$x)) == 3 & nrow(ad) > 4){
    
      mod <- rma(y, v, mods = ~ x, data = ad, method = "PM")
    
      simmed_mods[[i]] <- tibble(sim = i, 
                                 pct_miss = mean(is.na(ad$x)),
                                 beta0 = mod$beta[1,1], 
                                 beta1 = mod$beta[2,1],
                                 tau2 = mod$tau2, 
                                 se_beta0 = mod$se[1], 
                                 se_beta1 = mod$se[2], 
                                 se_tau2 = mod$se.tau2)
    }
}

sim = bind_rows(simmed_mods)
mean(sim$beta0) - beta0
mean(sim$beta1) - beta1

bias_fun(df, 0, c(beta0, beta1), tau2, gamma)


## Beta 0
d0 = df %>% filter(x == 0)
vv = mean(tau2 + dat$vi)
vv = nrow(d0) / sum(1/(tau2 + d0$vi))
H = exp(gamma0 + gamma2 * (beta0))/(1 + exp(gamma0 + gamma2 * (beta0 + beta1)))
M = H/(1 + exp(gamma0 + gamma2 * (beta0))) * (gamma2)^2
vv * (1 - H) * gamma2



ymn = list()
nr = 100
mu = -.1; sigma = .2
gamma0 = -1.5; gamma1 = 2.5
for(i in 1:nsims){
  y = rnorm(nr, mu, sigma)
  
  num = exp(gamma0 + gamma1 * y)
  denom = 1 + num
  
  probs = num/denom
  
  nas = rbinom(nr, 1, probs)
  
  y_obs = y[nas == 1]
  if(length(y_obs) > 0){
    ymn[[i]] = mean(y_obs)
  }
  
}
 
H = exp(gamma0 + gamma1 * mu)/(1 + exp(gamma0 + gamma1 * mu))
M = H/(1 + exp(gamma0 + gamma1 * mu)) * gamma1^2
sigma^2 * (1 - H) * gamma1
mean(unlist(ymn)) - mu

