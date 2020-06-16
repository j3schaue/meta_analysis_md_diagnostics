setwd("sandbox/cca_aca/")
library(metafor)
library(tidyverse)
source("cca_bias_sims_helperfns.R")

bd <- metafor::dat.bangertdrowns2004

bd %>% summarize_all(.funs = function(x) sum(is.na(x)))

dat <- bd %>% select(yi, vi, x = feedback) %>% na.omit() %>% as_tibble()
df = tibble(x = runif(nrow(dat), -1, 1), #rbinom(nrow(dat), 1, .65), 
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


beta0 = 0.1; beta1 = 0.8
simmed_mods <- list()
nsims <-  5000
for(i in 1:nsims){
  df_sim = df %>%
    mutate(y = rnorm(nrow(df), beta0 + beta1 * x, sqrt(tau2 + v)))

  ad <- ampute_linear(df_sim, gamma)
  
  if(length(unique(ad$x)) > 3 & nrow(ad) > 4){
    
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




X = matrix(c(rep(1, nrow(df)), df$x), ncol = 2)
Sig = diag(1/(df$v + tau2))
VV = df$v + tau2
Xb = X %*% c(beta0, beta1)
Y = rnorm(nrow(X), Xb[,1], sqrt(df$v + tau2))
solve(t(X) %*% Sig %*% X) %*% t(X) %*% Y

Sx2 = sum((X[,2] - weighted.mean(X[,2], 1/VV))^2/(VV))
Sy2 = sum((Y - weighted.mean(Y, 1/VV))^2/(VV))
Sxy = sum((Y - weighted.mean(Y, 1/VV)) * (X[,2] - weighted.mean(X[,2], 1/VV))/VV)
Sxy
Sxy/sqrt(Sx2 * Sy2) * sqrt(sum(1/VV))/sum(1/VV)
