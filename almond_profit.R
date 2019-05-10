#' Compute profit from a given almond yield anomaly
#' @param  price Price per ton almonds
#' @param  yield tons of almonds per year
#' @param discount Discount rate set to 10%
#' @author Paige FitzGibbon

almond_profit <-  function(yield, price, discount=0.10) {
  years = seq(from=1, to=length(yield))
  year_profit = data.frame(year=years, yield=yield)
  year_profit$total =  yearprofit$yield*price
  year_profit$npv = mapply(compute_NPV, value=year_profit$total, time=year_profit$year, discount=discount)
  return(list(year_profit = year_profit, total = sum(year_profit$npv)))
}
