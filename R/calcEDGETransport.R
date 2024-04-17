#' @title Prepare EDGETransport inputs
#'
#' @return magpie object of EDGEtransport iterative inputs
#' @seealso \code{\link{readSource}}
#' @param subtype refer to the code for a list of allowed subtypes.
#'
#' @examples
#' \dontrun{ a <- calcOutput(type="EDGETransport", subtype="logit_exponent", aggregate=F)
#' }
calcEDGETransport <- function(subtype = "logit_exponent") {
  value <- i.value <- NULL

  get_weight <- function(data, weightInt) {
    year_inter = getYears(data)
    ## define weight for intensive entries (for weighted average)
    weightInt <- time_interpolate(
      weightInt,
      year_inter,
      extrapolation_type = "constant")[, , getNames(data, dim = 1)]
    ## create an empty object that has the same dimensions as data
    weight <- new.magpie(cells_and_regions = getRegions(data), years = getYears(data), names = getNames(data), fill = 0)
    ## use the GPD for each SSP in data to fill up the empty weight (it needs as many repetitions as SSP* is called in data)
    regions <- getRegions(weightInt)
    years <- getYears(weightInt)
    for (gdpscen in getNames(weightInt)) {
      mselect(weight, region = regions, year = years, data = gdpscen) <- weightInt[regions, years, gdpscen]
    }
    getSets(weight, fulldim = FALSE)[3] <- getSets(data, fulldim = FALSE)[3]
    weight
  }

  if (subtype %in% c("logit_exponent", "ptab4W")) {
      conv = FALSE
   } else {
      conv = TRUE
   }

  weightInt <- calcOutput("GDP", aggregate = FALSE)

  if (subtype == "pm_trp_demand"){
    fe_dem <- readSource("EDGETransport", "fe_demand_tech") %>%
      quitte::as.quitte() %>%
      data.table::as.data.table() %>%
      `[`(, c("model", "scenario", "variable", "unit") := NULL)

    fe2es <- readSource("EDGETransport", "fe2es") %>%
      quitte::as.quitte() %>%
      data.table::as.data.table() %>%
      `[`(, c("model", "scenario", "variable", "unit") := NULL)

    es_dem <- fe_dem[
      fe2es,
      on = c("period", "region", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_teEs")
    ][,
      sum(value * i.value),
      by = c("period", "region", "GDP_scenario", "DEM_scenario", "EDGE_scenario", "all_in")
    ]

    data <- as.magpie(es_dem, spatial = 2, temporal = 1, datacol = 7)
  } else {
    data <- readSource("EDGETransport", subtype, convert = conv)
  }

  switch(
    subtype,
    "logit_exponent" = {
      weight = NULL
      unit = "[-]"
      description = "Logit exponent values for transport alternatives"
    },
    "harmonized_intensities" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport: [EJ/Mpkm]; freight transport: [EJ/Mtkm]"
      description = "Energy intensity for transport modes and fuel, harmonized on IEA balances"
    },
    "value_time" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport: [2005US$/pkm]; freight transport: [2005US$/tkm]"
      description = "Value of time for passenger transport modes"
    },
    "pref" = {
      weight = get_weight(data, weightInt)
      unit = "LDVs 4wheelers: inconvenience cost [2005US$/pkm]; all other modes: [-] share weight, a dimensionless parameter reflecting consumer preferences"
      description = "Inconvenience cost reflecting availability of infrastructure"
    },
    "ptab4W" = {
      weight = NULL
      unit = "[-]"
      description = "LDVs 4wheelers: factors for the inconvenience costs."
    },
    "price_nonmot" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport: [2005US$/pkm]; freight transport: [2005US$/tkm]"
      description = "Price for non motorized transport modes (Walking; Cycling)"
    },
    "UCD_NEC_iso" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport: [2005US$/pkm]; freight transport: [2005US$/tkm]"
      description = "Non energy costs for all motorized transport modes, both total and purchase for LDVs"
    },
    "loadFactor" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport: [pass/veh]; freight transport: [ton/veh]"
      description = "Load factor for all motorized transport modes"
    },
    "annual_mileage" = {
      weight = get_weight(data, weightInt)
      unit = "km/veh/year"
      description = "Annual mileage for selected transport modes"
    },
    "esCapCost" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport [2005US$/pkm]; freight transport: [2005US$/tkm]"
      description = "Transport ES demand trajectories for the CES tree."
    },
    "fe2es" = {
      weight = get_weight(data, weightInt)
      unit = "Passenger transport [trn pkm/Twa], freight transport [trn tkm/Twa]"
      description = "Energy efficiency of CES level nodes for Transport."
    },
    "pm_trp_demand" = {
      weight = NULL
      unit = "Passenger transport [trn pkm], freight transport [trn tkm]"
      description = "CES level transport demand."
    },
    "fe_demand_tech" = {
      weight = NULL
      unit = "TWa"
      description = "FE demand divided by technologies for different ES on the CES level."
    },
    "pm_fe_demand_EDGETbased" = {
      weight = NULL
      unit = "EJ"
      description = "UE demand divided by technologies for different ES on the CES level."
    }
  )

  list(x = data, weight = weight, unit = unit, description = description)
}
