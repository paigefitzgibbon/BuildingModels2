
#' @param feb_temp Average minimum temperature in February (Celcius)
#' @param jan_precip Average precipitation in January (mm)
#' @param user_input User input data which determines if the function will output average yield anomoly or yield anomoly by year
#' @param dtemp Change in temperature due to climate change
#' @param dprecip Change in precipitation due to climate change
#' @author Paige FitzGibbon


almond_yield <- function(feb_temp, jan_precip, user_input = "year", dtemp = 0, dprecip = 0) {   
  feb_temp = feb_temp + dtemp
  jan_precip = jan_precip * (1 + dprecip)
  output = -0.015 * feb_temp - 0.0046 * feb_temp^2 - 
    0.07 * jan_precip + 0.0043 * jan_precip^2 + 0.28
 data = c(min(output),max(output))
  names(data) = c("Minimum Almond Yield Anomaly", "Maximum Almond Yield Anomaly")
  avg = mean(output)
  if(user_input == "year") {return(list(output,data))}
  if(user_input == "average") {return(list(output, data))}
}

