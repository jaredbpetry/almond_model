#'
#' @param  
#' @param  
#' @param  
#' @param 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 

library(here)
library(tidyverse)

calc_almond_yield_anomaly <- function(clim_data = "clim.txt", a = -0.015, b = -0.0046, c = -0.07, d = 0.0043, e = 0.28) {
  clim_df <- read.table(here(paste0("data/", "clim.txt")), header = TRUE) # clim_data
  
  clim_df_prepped <- clim_df %>%
    group_by(month, year) %>%
    summarize(mean_tmax_c = mean(tmax_c),
              mean_tmin_c = mean(tmin_c),
              sum_precip = sum(precip))
  
  years <- sort(unlist(unique(clim_df_prepped$year)))
  years <- years[2:23] # we don't want 1988
  
  yield_list <- c()
  
  for (i in seq_along(years)) {
    clim_df_prepped_filtered <- clim_df_prepped %>%
      filter(year == years[[i]]) # filter df to year at hand
    message(paste("on year", years[[i]]))
    
    min_T_2 <- clim_df_prepped_filtered$mean_tmin_c[[2]] # get min_T_2 (min temp from month 2)

    precip_1 <- clim_df_prepped_filtered$sum_precip[[1]] # get precip_1 (precip from month 1)
    
    Y <- (a * min_T_2) + (b * (min_T_2^2)) + (c * precip_1) + (d * (precip_1^2)) + e # calculate yield anomaly with formula
    
    yield_list <- append(yield_list, Y) # add value to list
  }
  
  return(c(min_yield_anomaly = min(yield_list),
           max_yield_anomaly = max(yield_list),
           mean_yield_anomaly = mean(yield_list))) # output the yield anomaly min, max, mean
}
