#' Almond yield anomalies
#'
#' A function that computes almond yield anomalies (ton/acre) for years in a dataframe, converting daily information on minimum temperatures and precipitation to monthly information and using these in an equation with set numerical parameters by default.
#'
#' @param clim_data A character string ending in .txt referring to climate data with variables for daily minimum temperature in degrees Celsius (named tmin_c) and daily precipitation in mm (named precip), with corresponding month and year columns (named month and wy). This should be stored in the working directory in a folder called data. By default, this will search for "clim.txt."
#' @param temp_param A number corresponding to the almond yield anomaly parameter that is multiplied by the minimum minimum temperature of the second month of the year. Default = -0.015
#' @param sq_temp_param A number corresponding to the almond yield anomaly parameter that is multiplied by the minimum minimum temperature of the second month of the year, squared. Default = -0.0046
#' @param precip_param A number corresponding to the almond yield anomaly parameter that is multiplied by the sum of the precipitation of the first month of the year. Default = -0.07
#' @param sq_precip_param A number corresponding to the almond yield anomaly parameter that is multiplied by the sum of the precipitation of the first month of the year, squared. Default = 0.0043
#' @param constant A number that is added to all of the other parts of the almond yield anomaly equation. Default = 0.28
#'
#' @return A dataframe of the almond yield anomalies (ton/acre) for all years of the original dataframe
#' @export
#'
#' @examples
#' calc_almond_yield_anomalies_all_years() # this will run it with all of the defaults
#' calc_almond_yield_anomalies_all_years(clim_data = "climate.txt") # for example, running the function if your file is named differently

# read in necessary libraries
library(here)
library(tidyverse)

calc_almond_yield_anomalies_all_years <- function(clim_data = "clim.txt", temp_param = -0.015, sq_temp_param = -0.0046, precip_param = -0.07, sq_precip_param = 0.0043, constant = 0.28) {
  clim_df <- read.table(here(paste0("data/", clim_data)), header = TRUE) # read in the climate data
  
  clim_df_prepped <- clim_df %>%
    group_by(month, wy) %>% # group by month and water year; use water year because 1988 only has months 10,11,12 and need to exclude 1988 from years anyway
    summarize(min_tmin_c = min(tmin_c), # get min of min temps for the month of year
              sum_precip = sum(precip)) # get sum of precip for the month of year
  
  years <- sort(unlist(unique(clim_df_prepped$wy))) # get an ordered list of years for the loop
  
  yield_list <- c() # initialize list to hold yield anomalies by year
  
  for (i in seq_along(years)) { # for each year in the df
    clim_df_prepped_filtered <- clim_df_prepped %>%
      filter(wy == years[[i]]) # filter df to year at hand
    
    min_T_2 <- clim_df_prepped_filtered$min_tmin_c[[2]] # get min_T_2 (min temp from month 2)
    
    precip_1 <- clim_df_prepped_filtered$sum_precip[[1]] # get precip_1 (precip from month 1)
    
    Y <- (temp_param * min_T_2) + (sq_temp_param * (min_T_2^2)) + (precip_param * precip_1) + (sq_precip_param * (precip_1^2)) + constant # calculate yield anomaly with formula
    
    yield_list <- append(yield_list, Y) # add resulting value to list, and continue to next year if one exists
  }
  
  return(tibble(year = years,
                yield_anomaly = yield_list)) # create a df with years and corresponding anomalies
}