#' @param value Value of almond harvest, can be altered
#' @param time Year that is being evaluated for the NPV, can be altered
#' @param discount Discount rate set at 10%, can be altered
#' @author Paige FitzGibbon

compute_NPV <- function(value, time, discount=0.10)
{
  result = value / (1 + discount)**time
  return(result)
}
