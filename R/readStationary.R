#' Load Stationary File as magclass object
#'
#' @return magclass object
#' @order 1
readStationary <- function() {
  read.magpie("EDGE_TradMod.cs4r")
}

#' @rdname readStationary
#' @order 2
#' @param x MAgPIE object returned from readStationary
convertStationary <- function(x) {

  #---- Functions -------------
  noYearDim <- function(x) setYears(x, NULL)

  addSSPnames <- function(x) {
    do.call("mbind", lapply(c(paste0("SSP", c(1:5, "2_lowEn", "2_highDemDEU")),
                              paste0("SDP", c("", "_EI", "_RC", "_MC"))),
                            function(s) setNames(x, paste(s, getNames(x), sep = "."))))
  }

  renameExtraWeights <- function(magObj, magWeight, mapping) {
    do.call("mbind", lapply(mapping[["EDGEitems"]], function(itemIN) {
      if (itemIN %in% getNames(magObj, dim = "item")) {
        itemWeight <- mapping[mapping$EDGEitems == itemIN, "weight_convertEDGE"]
        subMagpie <- magWeight[, , itemWeight]
        res <- setNames(subMagpie, gsub(itemWeight, itemIN, getNames(subMagpie)))
      } else {
        res <- NULL
      }
      return(res)
    }))
  }

  calcLambda <- function(exceedingYearsVec, threshold, previousYears = NULL) {
    exceedingYearsBefore <- exceedingYearsVec[exceedingYearsVec <= threshold]
    exceedingYearsAfter  <- exceedingYearsVec[exceedingYearsVec > threshold]
    lambda <- c(rep(0, length(previousYears)),
                utils::tail(seq(0, 1, length.out = length(exceedingYearsBefore) + 1), -1),
                rep(1, length(exceedingYearsAfter)))
    names(lambda) <- as.character(c(previousYears, exceedingYearsVec))
    return(as.magpie(lambda))
  }

  #---- Parameters and Mappings ------
  structMappingPath <- toolGetMapping(type = "sectoral", name = "structuremappingIO_outputs.csv",
                                      returnPathOnly = TRUE, where = "mrcommons")
  structMapping <- utils::read.csv2(structMappingPath, na.strings = "")

  # Select the relevant part of the mapping
  structMapping <- structMapping[!is.na(structMapping$weight_convertEDGE), ]
  structMapping <- unique(structMapping[c("weight_convertEDGE", "EDGEitems")])

  # Select scenarios
  x[is.na(x)] <- 0
  getSets(x) <- c("region", "year", "scenario", "item")
  scenarios <- list(SSPs  = paste0("SSP", 1:5),
                    SSP2s = paste0("SSP2", c("_lowEn", "_NAV_all", "IndiaMedium", "IndiaHigh")))
  x <- purrr::map(scenarios$SSP2s,
                  ~ add_dimension(mselect(x, scenario = "SSP2", collapseNames = TRUE),
                                  dim = 3.1,
                                  add = "scenario",
                                  nm = .x)) %>%
    mbind(x)
  x <- mselect(x, scenario = Reduce(c, scenarios))


  #---- Explanations
  # For the historical data, weights are directly taken from the IEA
  # to ensure the consistency at the country level
  # for the future evolution, weights depend on last final energy data point available
  # multiplied by the growth rate of the country

  # Load the regional mapping which depends upon the model used

  mappingfile <- toolGetMapping(type = "regional", name = "regionmappingREMIND.csv",
                                returnPathOnly = TRUE, where = "mappingfolder")
  mapping <- utils::read.csv2(mappingfile)
  regionCol <- which(names(mapping) == "RegionCode")
  isoCol <- which(names(mapping) == "CountryCode")

  #--- Load the Weights
  #--- First load the GDP data. Set average2020 to False to get yearly data as far as possible.
  wg <- calcOutput("GDP", scenario = c("SSPs", "SSP2IndiaDEAs"), average2020 = FALSE, aggregate = FALSE)

  # duplicate SSP2 for SSP2_lowEn and SSP2_highDemDEU
  wg <- mbind(
    wg,
    setItems(wg[, , "SSP2"], 3, "SSP2_lowEn"),
    setItems(wg[, , "SSP2"], 3, "SSP2_highDemDEU")
  )

  #--- Then load the final energy data
  histFeStationary <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE", aggregate = FALSE)
  histFeBuildings <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)

  wfe <- mbind(histFeStationary, histFeBuildings)

  #---- Process Data -----------------

  # Replace NAs
  x[is.na(x)] <- 0

  if (any(wfe < 0)) {
    warning("calcOutput('IOEdgeBuildings', subtype = X), with X in (output_EDGE, output_EDGE_buildings) produces negative values, set to 0") # nolint
    wfe[wfe < 0] <- 0
  }

  # Select last year of X available in the historical data set
  maxYearXInFE <- max(getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) %in%
                                                       getYears(wfe, as.integer = TRUE)])
  # Deduce the scenario periods
  exceedingYears <- getYears(x, as.integer = TRUE)[getYears(x, as.integer = TRUE) > maxYearXInFE]


  # FE_stationary projections are not updated. Therefore, we correct here for the newly published past data
  # For historical years, the data is substituted. For projections years, there is first a transition period,
  # before the FE_stationary projections are fully taken up

  # The years exceeding maxYear might not be meaningful. Therefore we exclude them
  helper <- getYears(histFeStationary)[getYears(histFeStationary, TRUE) <= maxYearXInFE]
  feStationary <- time_interpolate(histFeStationary[, helper, ],
                                   interpolated_year = c(maxYearXInFE, exceedingYears),
                                   integrate_interpolated_years = TRUE,
                                   extrapolation_type = "constant")
  feStationary <- addSSPnames(feStationary)

  # change the regional resolution of feStationary to match the EDGE_stationary resolution
  # isoCol and regionCol are originally designed for the weights, that is why names are confusing here
  feStationary <- toolAggregate(feStationary, mappingfile, from = isoCol, to = regionCol)

  # Item names differ slightly for the input of EDGE_stationary (feStationary) and the output
  # The main issue concerns transport. We therefore restrict to the variables of interest in each data set of
  # historical data
  # Stationary, non-buildings names
  stationaryItems <- grep("^(fenon|feagr|feind|feoth)", getNames(x, TRUE)[[2]], value = TRUE)


  # create lambda vector that gives 0 to the historical data and 1 after 2030
  lambda <-  calcLambda(exceedingYears, 2030, getYears(x)[getYears(x, TRUE) <= maxYearXInFE])
  # Replace

  x[, , stationaryItems] <- feStationary[, getYears(x), stationaryItems] * (1 - lambda) +
    x[, , stationaryItems] * lambda

  # Scale GDP and FE weights so that they can be added
  wg <- wg / dimSums(wg, dim = 1, na.rm = TRUE)
  wfe <- wfe / dimSums(wfe, dim = 1, na.rm = TRUE)

  # Add some corrections
  wg[is.na(wg)] <- 0
  wg[wg == "Inf"] <- 0

  # Add some corrections to the FE data set + add the scenario dimension
  wfe[is.na(wfe)] <- 0
  wfe <- addSSPnames(wfe)

  # Compute lambda
  lambda <- calcLambda(exceedingYears, 2060)
  # For the future periods, the weight will be a linear combination of last FE weight and of the GDP size.
  # until maxYearXInFE this will be exclusively FE, in 2060 (depending on the threshold value above), exclusively GDP

  wfe <- mbind(
    wfe,
    lambda[, exceedingYears, ] * wg[, exceedingYears, ] +
      (1 - lambda[, exceedingYears, ]) * (noYearDim(wfe[, maxYearXInFE, ]))
  )

  # In cases where the variables in EDGE do not exist in the mapping for computing the final energy,
  # e.g. when EDGE produces further disaggregations, or when it gives REMIND items without computing them
  wfe <- mbind(wfe, renameExtraWeights(x, wfe, structMapping))

  # Reduce the dimensions of the weights
  wfe <- wfe[, getYears(x), getNames(x, dim = "item")]

  # Disaggregate and fill the gaps
  xadd <- toolAggregate(x, mappingfile, weight = wfe, from = regionCol, to = isoCol)
  toolCountryFill(xadd, 0, verbosity = 2)
}
