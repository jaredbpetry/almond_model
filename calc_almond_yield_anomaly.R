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
  clim_df <- read.table(here(paste0("data/", clim_data)), header = TRUE) # read in the climate data
  
  clim_df_prepped <- clim_df %>%
    group_by(month, wy) %>% # group by month and water year
    summarize(max_tmax_c = max(tmax_c), # get max of max temps for the month of year (not used for almonds)
              min_tmin_c = min(tmin_c), # get min of min temps for the month of year
              sum_precip = sum(precip)) # get sum of precip for the month of year
  
  years <- sort(unlist(unique(clim_df_prepped$wy))) # get an ordered list of years for the loop
  
  yield_list <- c() # initialize list to hold yield anomalies by year
  
  for (i in seq_along(years)) {
    clim_df_prepped_filtered <- clim_df_prepped %>%
      filter(wy == years[[i]]) # filter df to year at hand
    
    min_T_2 <- clim_df_prepped_filtered$min_tmin_c[[2]] # get min_T_2 (min temp from month 2)

    precip_1 <- clim_df_prepped_filtered$sum_precip[[1]] # get precip_1 (precip from month 1)
    
    Y <- (a * min_T_2) + (b * (min_T_2^2)) + (c * precip_1) + (d * (precip_1^2)) + e # calculate yield anomaly with formula
    
    yield_list <- append(yield_list, Y) # add value to list
  }
  
  return(c(min_yield_anomaly = min(yield_list),
           max_yield_anomaly = max(yield_list),
           mean_yield_anomaly = mean(yield_list))) # output the yield anomaly min, max, mean across all years
}
