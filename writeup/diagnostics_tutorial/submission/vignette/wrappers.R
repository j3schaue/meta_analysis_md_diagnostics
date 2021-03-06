###-------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------###
### Wrapper functions for visual EDA of missingness in a meta-analysis.
###-------------------------------------------------------------------------------------###
###-------------------------------------------------------------------------------------###

###---Dependencies
library(tidyverse)
library(tidyselect)
library(cowplot)
library(gridExtra)
library(cowplot)

###---Functions

###-----------------------------------------------------------------------------------###
#' # Function to plot distribution of ES or SE colored by whether a covariate is missing
#' @name gg_es_covariate_miss
#' @param shadow: shadow object of a tibble generated by nanair
#' @param es_col: string indicating the effect size column name
#' @param covariate: string indicating the covariate column name
#' @param adjust: numeric indicating the kernel bandwith adjustment for
#'                density plots.
#' @return plot of the density of the effect sizes colored according to if
#'         the covariate is missing or not
#' @note The function will work if you specify any es_col that corresponds to 
#'       a continuous variable in the tibble.
#' ###--------------------------------------------------------------------------------###
gg_es_covariate_miss <- function(shadow, es_col, covariate, adjust = 1){
  
  # Make sure we get the missingness indicator
  cov_na <- paste0(covariate, "_NA")
  shadow[[cov_na]] = sapply(shadow[[cov_na]], 
                            FUN = function(x) ifelse(x == "!NA", "Not Missing", "Missing"))
  
  # Construct plot
  p_es <- ggplot(shadow) +
    aes_string(x = es_col,
               colour = cov_na) + 
    stat_density(geom = "line", # Use stat_density to avoid x-axis lines
                 position = "identity", 
                 adjust = adjust, # adjust smoothing bandwidth
                 size = 1)
  
  # Return plot
  return(p_es)
}


###------------------------------------------------------------------------------------------###   
# Function to plot ES and SE plots in a grid
#' @name gg_esse_covariate_miss
#' @param shadow: shadow object of a tibble generated by nanair
#' @param es_col: string indicating the effect size column name
#' @param se_col: string indicating the standard error of the effect size column name
#' @param covariate: string indicating the covariate column name
#' @param adjust: list of numerics indicating the kernel bandwith adjustment for
#'                density plots. Order of list gives smoothing for plots on:
#'                c(effect size, standard error)
#' @param colors: list of colors (length 2)
#' @param ymax: numeric max density to display in plots
#' @param label: label for the color legend. Defaults to covariate column name.
#' @return side-by-side plots of the density of the effect sizes and their standard errors 
#'         colored according to if the covariate is missing or not
#' ###---------------------------------------------------------------------------------------###   
gg_esse_covariate_miss <- function(shadow, es_col, se_col, covariate, adjust = c(1, 1), 
                                   colors = c("#56B4E9", "#E69F00"), 
                                   ymax = NULL, label = NULL,
                                   legend_pos = "bottom"){
  
  # Sterilize input
  if(length(adjust) == 1){ adjust <- rep(adjust, 2) }
  if(is.null(label)){ label <- covariate }
  if(!is.null(ymax)){ ymax <- ylim(0, ymax) }
  
  
  ## ES plot
  p_es <- gg_es_covariate_miss(shadow, es_col, covariate, adjust[1]) + 
    scale_color_manual(label, values = colors) +
    ymax + 
    labs(x = "Effect Size", y = "Density") +
    theme_bw() + 
    guides(color = guide_legend(override.aes = list(size = 0.8)))
  
  ## SE plot
  p_se <- gg_es_covariate_miss(shadow, se_col, covariate, adjust[2]) + 
    scale_color_manual(label, values = colors) +
    ymax + 
    labs(x = "Standard Error of Effect Size") +
    theme_bw()

  # Extract legend
  legend <- get_legend(p_es + theme(legend.position = "bottom",
                                   legend.title = element_text(size = 9)))
  
  # Arrange plots in a grid
  prow <- plot_grid(p_es + 
                      theme(legend.position = "none",
                            axis.title.x = element_text(size=7)),
                    #
                    p_se + 
                      theme(legend.position = "none",
                            axis.title.x = element_text(size=7)) + 
                      labs(y = ""), # second y-label is superfluous
                    nrow=1)
  
  # Create grid plot
  if(legend_pos == "bottom"){
    out <- plot_grid(prow, legend, ncol=1, rel_heights=c(1, .1))
  } else {
    out <- plot_grid(legend, prow, ncol=1, rel_heights=c(.1, 1))
  }
  
  # Return grid plot
  return(out)
}

# # Example
# gg_esse_covariate_miss(adt_shadow,
#                        es_col = "es_g", 
#                        se_col = "se_g", 
#                        covariate = "g1hrsperweek", 
#                        adjust = c(1.3, 1.2), # Adjust smoothing for ES and SE densitites
#                        label = "Group 1 Hrs Per Week")



###-------------------------------------------------------------------------------###  
# Function to make forest plot 
#' @name gg_forest_covariate_miss
#' @param shadow: shadow object of a tibble generated by nanair
#' @param es_col: string indicating the effect size column name
#' @param se_col: string indicating the standard error of the effect size column name
#' @param covariate: string indicating the covariate column name
#' @param colors: list of colors (length 2)
#' @param label: label for the color legend. Defaults to covariate column name.
#' @param arrange_by: string, either "ES" or "SE", arranges forest plot by SE or ES
#' @return forest plot of effect sizes colored by whether the covariate is missing
###----------------------------------------------------------------------------###  
gg_forest_covariate_miss <- function(shadow, es_col, se_col, covariate, 
                                     colors = c("#E69F00", "#56B4E9"), 
                                     label = NULL, 
                                     arrange_by = "ES"){
  
  # Arrange data for plot
  cov_na <- paste0(covariate, "_NA")
  cov_sym = ensym(covariate) 
  es_sym = ensym(es_col)
  se_sym = ensym(se_col)
  
  tmp = shadow %>% 
    mutate(lb = !!es_sym - 1.96*!!se_sym, # get error bar bounds
           ub = !!es_sym + 1.96*!!se_sym,
           mm = is.na(!!cov_sym)) 
  
  if(arrange_by == "SE"){
    tmp <- tmp %>% arrange(desc(mm), # arrange by missingness and 
                    desc(!!se_sym),  # effect size
                    !!es_sym)  # SE of effect size
  } else {
    tmp <- tmp %>% arrange(desc(mm), # arrange by missingness and 
                    !!es_sym,  # effect size
                    desc(!!se_sym))  # SE of effect size
  }
    
  
  
  plot <- ggplot(tmp) + 
    geom_point(aes_string(x = 1:nrow(tmp), y = es_col, color = cov_na)) + 
    geom_segment(aes_string(x = 1:nrow(tmp), xend = 1:nrow(tmp), 
                            y = "lb", yend = "ub", color = cov_na)) + 
    coord_flip() + 
    scale_color_manual(label, 
                       values = colors,
                       labels = c("Observed", "Missing")) + 
  labs(x = "", y = "Effect Size") + 
  theme_bw() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())
  
  return(plot)
    
}

# gg_forest_covariate_miss(adt_shadow %>% sample_frac(.5),
#                           es_col = "es_g",
#                           se_col = "se_g",
#                           covariate = "g1hrsperweek",
#                           label = "Group 1 Hrs \nPer Week")


###-------------------------------------------------------------------------------###  
# Function to compute missing data summaries by variable 
#' @name mis_ma_var_summary
#' @param data: data set (tibble)
#' @param se_col: string indicating the standard error of the effect size column name
#' @param truncate: boolean; If TRUE, omits any variables with no missingness
#' @return forest plot of effect sizes colored by whether the covariate is missing
###----------------------------------------------------------------------------###  
mis_ma_var_summary <- function(data, se_col, truncate = TRUE){
  
  # Quick compute functions:
    # no. of cases
  n_miss_fun <- function(x){ 
      return(sum(is.na(x))) 
  } 
  
  # pct of cases
  pct_miss_fun <- function(x){ 
      return(mean(is.na(x)) * 100) 
  }  
  
  # weighted pct of cases
  wt_miss_fun <- function(x, wt){ 
    return(sum(as.integer(is.na(x)) * wt)/sum(wt) * 100) 
  }
  
  # pull standard erros
  se_vals <- data %>% 
    select(all_of(se_col)) %>% 
    as_vector()
  
  # build summar table
  ntab <- data %>% 
    summarize_all(n_miss_fun)
  pctab <- data %>% 
    summarize_all(pct_miss_fun)
  
  sums <- bind_rows(ntab, pctab)
  
  if(!is.null(se_col)){
    
    wpctab <- data %>% 
      summarize_all(.funs = function(x) wt_miss_fun(x, (1/se_vals)^2))
    
    sums <- bind_rows(sums, wpctab)
    row.names(sums) <- c("n_miss", "pct_miss", "wtpct_miss")
    
  } else {
    
    row.names(sums) <- c("n_miss", "pct_miss")
    
  }
  
  sums_tab <- as_tibble(cbind(Variable = names(sums), t(sums))) %>%
    mutate_at(grep("miss", names(.), value = T), as.numeric) %>%
    arrange(desc(wtpct_miss)) 
  
  if(truncate){ sums_tab <- filter(sums_tab, n_miss > 0) }
  
  return(sums_tab)
}

# mis_ma_var_summary(adt_data, "se_g", truncate = FALSE)


###-----------------------------------------------------------------------------------###
#' # Function to plot whole-data summaries in one grid
#' @name gg_summary_covariate_miss
#' @param data: original dataset
#' @return plot two different whole-data summaries side by side
###--------------------------------------------------------------------------------###
gg_summary_covariate_miss <- function(data){

  #Visualize whole dataframe at once
  visdat<- vis_dat(data) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      legend.key.size = unit(.4, "cm"),
      axis.text.x = element_text(size = 7, 
                                 angle = 77.5)
    )
  #summary of whether the data is missing or not
  vismiss<-vis_miss(data) +
    theme(
      legend.text = element_text(size = 7),
      legend.key.size = unit(.5, "cm"),
      axis.text.x = element_text(size = 7, 
                                 angle = 77.5)
    )
  
  # Arrange plots in a grid
  prow <- plot_grid(
    visdat,
    vismiss + labs(y = ""),
    labels=c('A','B'),
    align="h"
  
  )
 
  return(prow)

}


