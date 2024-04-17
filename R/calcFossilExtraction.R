#' @title calc Fossil Extraction
#' @description provides coefficients for fossil fuels (oil, gas and coal) and uranium extraction cost equations.
#' @return magpie object of the coefficients for fossil fuels and uranium extraction cost equations
#' @author Renato Rodrigues, Felix Schreyer
#' @param subtype Either 'FossilExtraction' or 'UraniumExtraction'
#' @seealso \code{\link{calcOutput}}
#' @examples
#'
#' \dontrun{
#' calcOutput("FossilExtraction", subtype = 'FossilExtraction')
#' }
#' @importFrom dplyr mutate select rename arrange
calcFossilExtraction <- function(subtype = "FossilExtraction") {

  if (! subtype %in% c("FossilExtraction", "UraniumExtraction")) {
    stop("Not a valid subtype!")
  }

  # Setting the weight to the country reserves potential
  # (same weight used on the convertREMIND_11Regi, fossilExtractionCoeff and uraniumExtractionCoeff subtypes)
  if (subtype == "FossilExtraction") {
    description <- "Coefficients for the 3rd-order polynomial Oil, Gas and Coal extraction costs."
    # Loading coefficients data at country level
    output <- readSource("REMIND_11Regi", subtype = "fossilExtractionCoeff")
    # Upper bound
    upperBoundMaxExtraction <- readSource("REMIND_11Regi", subtype = "ffPolyCumEx")[, , "max"]
    # High cost curve -> empty list :the aggregation function will choose the coefficients even for regions with no
    # extraction potential
    highCostCurve = list()

    # Add specific Austrlian gas extraction cost curve based on Dylan McConnell/GSOO data
    AusRawData <- readSource("DylanAusGasCost")

    RegrData <- as.data.frame(AusRawData["AUS", , ]) %>%
      dplyr::select("Data2", "Value") %>%
      dplyr::rename("Res" = "Data2", "Price" = "Value") %>%
      dplyr::mutate(Price = as.numeric(.data$Price), Res = as.numeric(as.character(.data$Res)))

    s_GJ_2_twa <- 31.71e-12

    RegrData <- RegrData %>%
      # Remove depleted sites (zero resources) %>%
      dplyr::filter(.data$Res > 0) %>%
      # Add extraction between 2005 and 2015
      # DIIS data: 2005-2015 natural gas production: 23EJ
      # assume that 23 EJ were extracted at 1.5AUSD/GJ -> 0.9 USD
      # (1.58 is lowest price in gas extraction data after 2015) -> 0.9 USD
      rbind(data.frame(Res = 23000, Price = 0.9)) %>%
      # Convert from PJ to TWa
      dplyr::mutate(Res = .data$Res * 1e-15 / s_GJ_2_twa) %>%
      # Convert from USD/GJ to tr USD/Twa
      dplyr::mutate(Price = .data$Price / s_GJ_2_twa / 1e12 ) %>%
      dplyr::arrange(.data$Price)

    RegrData$CumRes <- cumsum(RegrData$Res)

    # Remove data points above 6 TWa Cumulative Extraaction, because then Extraction cost constant and would only be
    # outliers to fit, assume that we do not reach with extraction there anyways
    RegrData <- dplyr::filter(RegrData, .data$CumRes < 6)

    # Linear fit to extraction cost data
    linfit <- stats::lm(Price ~ poly(CumRes, 1, raw = TRUE), data = RegrData)

    # subsitute Australia gas extraction polcy coef for medium Scenario with linear fit to Dylan's data
    output["AUS", , "medGas.0"] <- stats::coef(linfit)[1]
    output["AUS", , "medGas.1"] <- stats::coef(linfit)[2]
    output["AUS", , "medGas.2"] <- 0
    output["AUS", , "medGas.3"] <- 0

    # SB & NB edit 2019/09/11:
    # Shift SSP5 coal extraction cost curve down by 33% based on calibration with the SSP IAM project 2017
    output[, , c("highCoal.0","highCoal.1")] <- output[, , c("highCoal.0","highCoal.1")] * (1 - 0.33)
    # Shift SSP5 oil extraction cost curve for USA and CAZ down by 20% based on calibration with the SSP IAM project 2017
    usa_and_caz <- c("USA", "CAN", "AUS", "NZL", "HMD", "SPM")
    output[usa_and_caz, , c("highOil.0", "highOil.1")] <- output[usa_and_caz, , c("highOil.0", "highOil.1")] * (1 - 0.2)

    # SB & NB edit 2019/11/14:
    # Reduce SSP5 coal extraction costs for USA and CAZ by 25%
    output[usa_and_caz, , "highCoal.0"] <- output[usa_and_caz, , "highCoal.0"] * (1 - 0.25)
    # Reduce SSP5 gas extraction costs for all regions by 10%
    output[, , "highGas.0"] <- output[, , "highGas.0"] * (1 - 0.1)

  } else if (subtype == "UraniumExtraction") {

    description <- "Coefficients for the 3rd-order polynomial Uranium extraction costs."
    # Loading coefficients data at country level
    data <- readSource("REMIND_11Regi", subtype = "uraniumExtractionCoeff")
    # maxExtraction (upper limit for function estimation)
    # Remaining extraction potential per country
    BGRuranium <- readSource("BGR", subtype = "uranium")[, , "Remaining_Potential"]
    BGRuranium  <- toolCountryFill(BGRuranium, fill = 0, verbosity = 2)
    totalBGRuranium <- as.numeric(colSums(BGRuranium))
    # 23 -> from s31_max_disp_peur -> max global "maximum amount of cumulative uranium production in Megatonnes of metal
    # uranium (U3O8, the stuff that is traded at 40-60US$/lb)."
    upperBoundMaxExtraction <- 23 * ( BGRuranium/totalBGRuranium )
    upperBoundMaxExtraction <- add_dimension(upperBoundMaxExtraction, dim = 3.1, add = "pe", nm = "peur")
    # Adding a 50% more to the maximum extraction because the countries shares do not take into consideration the
    # different marginal costs of each country
    upperBoundMaxExtraction <- upperBoundMaxExtraction * 1.5
    output <- data
    # Set high cost curve if all countries within a region have zero extraction capacity
    highCostCurve <- list("xi1" = 0.025, "xi2" = 1000, "xi3" = 0, "xi4" = 0)

  }

  list(x = output,
       weight = NULL,
       unit = "tr USD2017/TWa",
       description = description,
       aggregationFunction = toolCubicFunctionAggregate,
       aggregationArguments = list(xUpperBound = upperBoundMaxExtraction, steepCurve = highCostCurve))
}
