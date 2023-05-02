#' **Almond profit model**
#'
#' The general idea of the model is as follows: 
#' The 'yield' function is nested within the 'profit' function.
#' 
#' >>> profit = selling_price x yield - production_cost x yield
#' >>> yield = (expected_yield x acres) + (yield_anomaly x acres)
#'
#' @description A function that computes the profit from almonds in a certain year. A main component of the function is the actual *yield* which we are assuming is the usual *estimated yield*(1 ton per acre) plus the *yield anomaly* (from yield anomaly function - unit: tons per acre).  Both elements are measured in tons per acre, so they must be multiplied by how many acres is in the almond orchard. This *yield* equations is then nested within the *profit* function, which has the variables *selling_price* and *production_cost*.  These two variables both are related to the tons of almonds harvested, so the units are dollars per ton, and are multiplied by the tons of almonds that was harvested that year.
#' 
#' 
#' @assumptions
#' We will assume that a given company sells an average of 100 tons (200,000 lbs) of almonds every year
#' We will assume that almonds cost $5,000 dollars per ton 
#' This means that the average profit of almonds is 
#' thinking something like this: 
#' profit = selling_price (yield) - cost_price (yield) 
#' the formula for yield is 
#' yield = average yield + ()
#' 
#' # PARAMETERS
#' 
#' @param expected_yield The expected yield per acre of the almond orchard. We will use 1 ton per acre. 
#' 
#' @param acres The number of acres someone has for their almond tree orchard.
#' 
#' @param selling_price The price at which the almonds are selling for.  $5,000 per ton. We will run the sensitivity analysis on this parameter.
#' 
#' @param production_cost How much the almonds cost to produce.  $4,000 per ton. We will run the sensitivity analysis on this parameter.
#' 
#' # VARIABLES - vary per year
#' 
#' @param yield_anomaly_df$yield_anomaly Calculated from the almond yield anomaly function. The units are tons per acre.
#' 
#' @param yield This is calculated based off the almond yield anomaly (tons per acre), the expected yield (tons per acre), and the amount of acres on the almond farm.  The average yield plus or minus the positive or negative yield anomaly will give us the yield for a given (year?).
#' 
#' 
#'
#' # OUTCOME 
#' 
#' @return The profit that is expected from the almond orchard (list of annual and mean).


# IMPORT LIBRARIES
library(tidyverse) 
library(here)

# FUNCTIONS

# ---- Call the almond yield function 

source("calc_almond_yield_anomaly_for_profit_func.R")


# ---- Create the profit function described above

calc_almond_profit <- function(selling_price = 5000, production_cost = 4000,  # price and production cost in dollars per ton
                         acres = 200, expected_yield  = 1) {  # expected yield is assumed 1 ton per acre (found USDA survey online)
  
  # assign the yield anomalies by years dataframe (from sourced yield anomaly function) to a variable to use in this function
  yield_anomaly_df <- calc_almond_yield_anomaly_for_profit_func()
  
  # profit_list_no_anom <- c()
  profit_list_w_anom <- c()
  
  for (i in seq_along(yield_anomaly_df$year)) {
  # calculate yield with ONLY expected yield and  WITHOUT yield_anomaly based on number of acres present (200 assumed)
  yield = expected_yield * acres  # units of yield comes out to tons of almonds produced
  profit_no_anom = selling_price * yield - production_cost * yield
  
  # calculate yield with expected yield and anomaly based on number of acres present (200 assumed)
  yield = expected_yield * acres + yield_anomaly_df$yield_anomaly[[i]] * acres # units of yield comes out to tons of almonds produced
  profit_w_anom = selling_price * yield - production_cost * yield
  
  # profit_list_no_anom <- append(profit_list_no_anom, profit_no_anom)
  profit_list_w_anom <- append(profit_list_w_anom, profit_w_anom)
  }
  
  profit_df <- tibble(year = yield_anomaly_df$year,
                      profit_w_anom = profit_list_w_anom) # create df with profits
  
  return(list(annual = profit_df, mean = mean(profit_df$profit_w_anom))) # return list of annual profits and mean profit (relevant for sensitivity analysis)
  
}



