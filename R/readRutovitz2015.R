#' Employment factors for various power production technologies from Rutovitz et al. 2015
#'
#' Rutovitz, J., Dominish, E., & Downes, J. (2015). Calculating global energy sector jobs—2015 methodology update.
#'  Institute for Sustainable Futures, University of Technology, Sydney.
#'  https://opus.lib.uts.edu.au/bitstream/10453/43718/1/Rutovitzetal2015Calculatingglobalenergysectorjobsmethodology.pdf
#'
#' @param subtype Either "oecd_ef","regional_ef","coal_ef","gas_ef", "regional_mult"
#'
#' @return Magpie object of employment factors for different technologies and activities in Jobs/MW
#'   (all except fuel_supply) or Jobs/PJ (fuel_supply). Subtype "regional_mult" is a regional multiplier without units.
readRutovitz2015 <- function(subtype) {
  # Note: Employment factors from rutovitz2015 et al. are wt. average values from OECD countries.
  # (Regional) Exceptions are input in subtype "regional_ef", "coal_ef" and "gas_ef"
  if (subtype == "oecd_ef") {
    input <- readr::read_csv(file = "~/madrat_GDP_update/sources/Rutovitz2015/oecd_ef.csv", na = "", col_types = "cddddc") %>%
      dplyr::rename("tech" = 1, "duration" = 2, "CI" = 3, "Manf" = 4, "OM" = 5, "Fuel_supply" = 6) %>%
      dplyr::filter(!is.na(.data$tech)) %>%
      # for *HP technologies, multiply OM EF by 1.5 as in rutovitz
      tibble::add_row("tech" = "CoalHP", "duration" = 5, "CI" = 11.2, "Manf" = 5.4, "OM" = 0.14 * 1.5,
                      "Fuel_supply" = "Regional") %>%
      tibble::add_row("tech" = "GasHP", "duration" = 2, "CI" = 1.3, "Manf" = 0.93, "OM" = 0.14 * 1.5,
                      "Fuel_supply" = "Regional") %>%
      tibble::add_row("tech" = "BiomassHP", "duration" = 2, "CI" = 14, "Manf" = 2.9, "OM" = 1.5 * 1.5,
                      "Fuel_supply" = "Regional") %>%
      # oil EF= Gas EF as in rutovitz
      tibble::add_row("tech" = "Oil", "CI" = 1.3, "Manf" = 0.93, "OM" = 0.14, "Fuel_supply" = "Regional",
                      "duration" = 2) %>%
      # Remove techs not relevant
      dplyr::filter(!.data$tech %in% grep("Ocean|decommissioning|heat|diesel", x = .data$tech, value = TRUE)) %>%
      dplyr::mutate(tech = sub("Solar Photovoltaics", "Solar|PV", .data$tech),
                    tech = sub("Solar thermal", "Solar|CSP", .data$tech),
                    Fuel_supply = ifelse(.data$Fuel_supply == "0.001 jobs/GWh final demand", 0.001, .data$Fuel_supply),
                    Fuel_supply = ifelse(.data$Fuel_supply == "Regional", 0, .data$Fuel_supply),
                    across(c("CI", "Manf", "OM", "Fuel_supply"), as.numeric)) %>%
      dplyr::select(-"duration") %>%
      tidyr::pivot_longer(2:5, names_to =  "activity") %>%
      # Regional values exist for coal and gas and are read later
      dplyr::mutate(across(c("tech", "activity"), as.factor))

    x <- as.magpie(input, temporal = NULL, spatial = NULL, datacol = 3)
  }

  if (subtype == "regional_ef") {
    input <- readr::read_csv("regional_ef.csv", na = "", col_types = "ccdddd") %>%
      dplyr::rename("tech" = 1, "region" = 2, "CI" = 3, "Manf" = 4, "OM" = 5, "Fuel_supply" = 6) %>%
      dplyr::filter(!is.na(.data$tech)) %>%
      dplyr::mutate(tech = sub("Solar PV", "Solar|PV", .data$tech),
                    tech = sub("Solar Thermal power", "Solar|CSP", .data$tech),
                    tech = sub("Wind-offshore", "Wind offshore", .data$tech),
                    tech = sub("Wind-onshore", "Wind onshore", .data$tech)) %>%
      tidyr::pivot_longer(c("CI", "Manf", "OM", "Fuel_supply"), names_to = "activity", values_to = "value") %>%
      # Remove OECD average values
      dplyr::filter(!grepl("average", .data$region)) %>%
      stats::na.omit() %>%
      dplyr::select("region", "tech", "activity", "value")

    x <- as.magpie(input, spatial = 1, temporal = NULL, datacol = 4)
  }

  if (subtype == "coal_ef") {
    input <- readr::read_csv("coal_ef.csv", col_types = "cddd") %>%
      dplyr::select(-"Year", -"Productivity") %>%
      dplyr::rename("region" = 1, "value" = 2) %>%
      dplyr::filter(!is.na(.data$region), !grepl("World", .data$region)) %>%
      dplyr::mutate(value = ifelse(.data$region == "Middle East", 39.7, .data$value),
                    region = ifelse(.data$region == "Eastern Europe/Eurasia", "Eurasia", .data$region),
                    tech = "Coal",
                    activity = "Fuel_supply",
                    value = as.numeric(.data$value))
    input <- input[, c("region", "tech", "activity", "value")]

    x <- as.magpie(input, temporal = NULL, datacol = 4, spatial = 1)
  }

  if (subtype == "gas_ef") {
    input <- readr::read_csv("gas_ef.csv", na = "", col_types = "cdcc") %>%
      dplyr::select("region" = 1, "value" = 2) %>%
      # using world average values from dataset
      rbind(data.frame(region = c("India", "Latin America", "Developing Asia", "Middle East"), value = 15.1)) %>%
      dplyr::filter(!is.na(.data$value), !grepl("World", .data$region))  %>%
      dplyr::mutate(region = ifelse(.data$region == "Eastern Europe/Eurasia", "Eurasia", .data$region),
                    tech = "Gas",
                    activity = "Fuel_supply")

    input <- input[, c("region", "tech", "activity", "value")]

    x <- as.magpie(input, temporal = NULL, datacol = 4, spatial = 1)
  }

  if (subtype == "regional_mult") {
    input <- readr::read_csv("regional_mult.csv", na = "", col_types = "c") %>%
      tidyr::pivot_longer(cols = 2:4, names_to = "year") %>%
      dplyr::mutate(region = ifelse(.data$region == "Eastern Europe/Eurasia", "Eurasia", .data$region))

    x <- as.magpie(input, spatial = 1, temporal = 2)
  }
  x
}
