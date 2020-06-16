###-----------------------------------------------------###
###-----------------------------------------------------###
### Script to for helper functions when checking some
###   approximations for complete- and available-case 
###   estimators in meta-analysis
###-----------------------------------------------------###
###-----------------------------------------------------###

ampute_linear <- function(data,
                         gamma = c(-2, 0.5, 1.5, 1), # vector
                         verbose = FALSE){
  out <- data %>% 
    mutate(
      #
      # logit probability of observation
      gamma_xy = map2_dbl(x, y, .f = function(x, y){ 
        prd <- gamma %*% c(1, x, y, x * y)
        return(prd[1,1])
      }),
      #
      # probability of observation
      prob = exp(gamma_xy)/(1 + exp(gamma_xy)),
      #
      # randomly ampute according to probabilities
      na = map_dbl(prob, rbinom, n = 1, size = 1),
      x = ifelse(na == 0, NA, x)
    )
  
  if(verbose){
    print(
      paste0(
        round(mean(is.na(out$x) * 100)),
        "% of effects missing covariates"
      )
    )
  }
  
  return(out)
}


h_fun <- function(x, beta, gamma){
  xb <- c(1, x) %*% beta
  xb <- xb[1,1]
  xg <- gamma %*% c(1, x, xb, xb * x) 
  ee <- exp(xg[1,1])
  
  H <- ee/(1 + ee)
  return(H)
}

bias_fun <- function(data,
                     x,
                     beta,
                     tau2,
                     gamma){
  ## Beta 0
  d0 = data %>% filter(x == 0)
  v0 = nrow(d0) / sum(1/(tau2 + d0$v))
  H0 = h_fun(0, beta, gamma)
  bias0 = v0 * (1 - H0) * gamma[3]
  
  ## Beta 1 
  d1 = data %>% filter(x == 1)
  v1 = nrow(d1) / sum(1/(tau2 + d1$v))
  H1 = h_fun(1, beta, gamma)
  bias1 = v1 * (1 - H1) * (gamma[3] + gamma[4]) - bias0
  
  return(list(beta0 = bias0, beta1 = bias1))
}
