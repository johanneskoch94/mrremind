#' Floor space in buildings
#'
#' Residential, commercial and total floor space from EDGE-B. Set
#'
#' @author Antoine Levesque, Robin Hasse
#'
#' @param onlyTotal boolean, only give total instead of sub-sectoral floor space
#' @return MAgPIE object with buildings floor space
#'
#' @export
#'
calcFloorspace <- function(scenario, onlyTotal = FALSE) {

  # Replace calls to SSPs and SSP2IndiaDEAs to individual scenarios, if present
  if ("SSPs" %in% scenario) {
    scenario <- c(scenario[!grepl("SSPs", scenario)], c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))
  }
  if ("SSP2IndiaDEAs" %in% scenario) {
    scenario <- c(scenario[!grepl("SSP2IndiaDEAs", scenario)], c("SSP2IndiaMedium", "SSP2IndiaHigh"))
  }

  data <- readSource("EdgeBuildings", subtype = "Floorspace", subset = scenario)

  if (onlyTotal) {
    data <- collapseNames(data[, , "buildings"])
  }

  list(x = data,
       weight = NULL,
       unit = "million m2",
       description = "Buildings floor space")
}
