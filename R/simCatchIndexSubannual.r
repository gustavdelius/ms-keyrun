#' The Atlantis-simulated fishery catch indices for the mskeyrun project
#'
#' atlantosom output is accessed and fishery data pulled over time
#' simulated fishery catches are cumulative, not snapshots
#' fishery total catch for the year is the sum of the months
#' fishery catch for the month is all catch reported since the last reporting month
#'
#' @format A data frame
#'
#' \describe{
#'\item{ModSim}{Atlantis model name and simulation id}
#'\item{year}{year simulated fishery conducted}
#'\item{fishMonth}{month simulated fishery conducted}
#'\item{Code}{Atlantis model three letter code for functional group}
#'\item{Name}{Atlantis model common name for functional group}
#'\item{fishery}{simulated fishery name}
#'\item{variable}{catch or coefficient of variation (cv) of biomass}
#'\item{value}{value of the variable}
#'\item{units}{units of the variable}
#'
#' }
#'
#'
"simCatchIndexSubannual"
