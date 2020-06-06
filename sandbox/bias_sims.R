library(metafor)
library(tidyverse)

bd <- metafor::dat.bangertdrowns2004

bd %>% summarize_all(.funs = function(x) sum(is.na(x)))

dat <- bd %>% select(yi, vi, x = feedback) %>% na.omit() %>% as_tibble()

ampute_dat = function(data, gamma0=-2, gamma1=1){
  out <- data %>%
    mutate(
      prob = exp(gamma0 + gamma1 * yi)/(1 + exp(gamma0 + gamma1 * yi)), 
      na = map_dbl(prob, rbinom, n = 1, size = 1),
      x = ifelse(na == 1, NA, x)
    )
  
  return(out)
}

fm = rma(yi, vi, mods = ~x, data = dat)
beta0 = fm$beta[1]
beta1 = fm$beta[2]
tau2 = fm$tau2

se_beta0 = fm$se[1]
se_beta1 = fm$se[2]
se_tau2 = fm$se.tau2
gamma0 = -2.2; gamma1 = 2.5

simmed_mods <- list()
nsims <-  1000
for(i in 1:nsims){
  ad <- ampute_dat(dat, gamma0, gamma1)
  mod <- rma(yi, vi, mods = ~ x, data = ad, method = "REML")
  simmed_mods[[i]] <- tibble(sim = i, 
                             beta0 = mod$beta[1,1], 
                             beta1 = mod$beta[2,1],
                             tau2 = mod$tau2, 
                             se_beta0 = mod$se[1], 
                             se_beta1 = mod$se[2], 
                             se_tau2 = mod$se.tau2)
}

sim = bind_rows(simmed_mods)
head(sim)

ggplot(sim) +
  geom_histogram(aes(beta0)) +
  geom_vline(xintercept = beta0)

(mean(sim$beta0) - beta0)/se_beta0


ggplot(sim) +
  geom_histogram(aes(beta1)) +
  geom_vline(xintercept = beta1)

(mean(sim$beta1) - beta1)/se_beta1


vv = mean(tau2 + dat$vi)
os = -2 * vv * gamma1 + 2 * vv * exp(gamma0)/(1 + exp(gamma0)) * gamma1
mean(sim$beta0) - beta0
os * vv
