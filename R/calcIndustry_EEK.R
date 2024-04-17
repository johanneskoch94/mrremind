#' Industry Energy Efficiency Capital
#'
#' @param kap General internal capital stock, as calculated internally by
#'   `calcCapital()`.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, and `description` fields.
#'
#' @importFrom dplyr bind_rows filter group_by lag lead mutate n row_number select
#' @export
calcIndustry_EEK <- function(kap) {
  # setup ----
  i <- log(4) / 50    # assuming 50 year lifetime of EEK
  base_year <- 2015

  # read data ----
  ## subsector activity projections ----
  industry_VA <- calcOutput(
    type = 'Industry_Value_Added',
    subtype = 'economic',
    match.steel.historic.values = TRUE,
    match.steel.estimates = 'IEA_ETP',
    China_Production = readSource(type = 'ExpertGuess',
                                  subtype = 'Chinese_Steel_Production',
                                  convert = FALSE) %>% quitte::madrat_mule(),
    aggregate = FALSE,
    years = base_year,
    supplementary = FALSE,
    warnNA = FALSE
  ) %>%
    `[`(, , 'gdp_SSP2EU') %>%
    quitte::magclass_to_tibble() %>%
    select('iso3c', subsector = 'name', VA = 'value') %>%
    mutate(subsector = sub('_VA$', '', .data$subsector))

  ## investment volumes into energy efficiency ----
  IEA_WEIO_2014 <- readSource('IEA_WEIO_2014', convert = FALSE) %>%
    quitte::madrat_mule()

  ## industry subsector activity and FE projections ----
  FEdemand <- calcOutput(type = 'FEdemand', aggregate = FALSE, supplementary = FALSE)

  # calculate EEK ----
  ## split industry VA into IEA investment sectors ----
  # Cement, Chemicals, and Steel are 'energy intensive', Other Industry is 'non-energy intensive'
  industry_VA <- industry_VA %>%
    dplyr::full_join(IEA_WEIO_2014$country_groups, by = "iso3c") %>%
    assertr::assert(assertr::not_na, tidyselect::everything()) %>%
    dplyr::mutate(name = ifelse('otherInd' == .data$subsector, 'Non-energy intensive', 'Energy intensive')) %>%
    # Calculate country/sector share in IEA region VA
    dplyr::mutate(VA.share = .data$VA / sum(.data$VA), .by = c("IEA region", "name"))

  ## Calculate EEK from investments ----
  EEK <- IEA_WEIO_2014$data %>%
    # Combine investment estimates with subsector VA figures
    dplyr::inner_join(industry_VA, by = c("IEA region", "name")) %>%
    # Assuming a "steady state", where investments only replace existing EEK stock
    dplyr::mutate(EEK = .data$VA.share * .data$value / i) %>%
    dplyr::select("iso3c", "subsector", "EEK")

  ## split steel EEK ----
  EEK <- EEK %>%
    # split steel EEK based on primary/secondary steel FE shares (higher FE
    # shares should lead to higher EEK shares)
    dplyr::left_join(
      FEdemand %>%
        `[`(,base_year,'gdp_SSP2EU.fe', pmatch = 'left') %>%
        `[`(,,'steel', pmatch = TRUE) %>%
        quitte::magclass_to_tibble() %>%
        dplyr::select(iso3c = 'region', 'item', FE = 'value') %>%
        # everything not 'steel_secondary' is 'steel_primary'
        dplyr::mutate(item = sub('steel$', 'steel_primary', .data$item)) %>%
        extract('item', 'foo', '^fe.*_(steel_.*)$') %>%
        # calculate primary/secondary steel FE shares
        group_by(!!!syms(c('iso3c', 'foo'))) %>%
        dplyr::summarise(FE = sum(.data$FE), .groups = 'drop_last') %>%
        dplyr::mutate(FE.share = tidyr::replace_na(.data$FE / sum(.data$FE), 0), subsector = 'steel') %>%
        ungroup() %>%
        dplyr::select(-'FE'),
      by = c('iso3c', 'subsector')
    ) %>%
    dplyr::mutate(subsector = ifelse(is.na(.data$foo), .data$subsector, .data$foo),
                  EEK       = ifelse(is.na(.data$foo), .data$EEK, .data$EEK * .data$FE.share)) %>%
    dplyr::select('iso3c', 'subsector', 'EEK')

  ## Converting from billion 2012 to trillion 2017 dollars
  EEK <- EEK %>%
    dplyr::rename("value" = "EEK") %>%
    GDPuc::convertGDP(unit_in = "constant 2012 Int$PPP",
                      unit_out = "constant 2017 Int$PPP",
                      replace_NAs = "with USA") %>%
    dplyr::mutate(value = .data$value * 1e-3) %>%
    dplyr::rename("EEK" = "value")

  ## temper EEK share in total capital ----
  # Temper industry EEK share in total capital by applying a geometric average
  # between the regional shares and the global share.
  temper <- full_join(EEK %>%
                        dplyr::summarise(EEK = sum(.data$EEK), .by = "iso3c") %>%
                        quitte::sum_total_(group = 'iso3c', value = 'EEK', name = 'World'),
                      quitte::sum_total_(kap, group = 'iso3c', value = 'kap', name = 'World'),
                      by = "iso3c") %>%
    assertr::assert(assertr::not_na, tidyselect::everything()) %>%
    dplyr::mutate(kap_ind_share = .data$EEK / .data$kap,
                  temper = .data$kap_ind_share['World' == .data$iso3c],
                  kap_ind_share_tempered = ifelse(
                    'World' == .data$iso3c,
                    FALSE,
                    (.data$kap_ind_share * .data$temper ^ 2) ^ (1 / 3)),
                  kap_ind_tempered = .data$kap_ind_share_tempered * .data$kap,
                  kap_ind_tempered = ifelse('World' == .data$iso3c,
                                            sum(.data$kap_ind_tempered),
                                            .data$kap_ind_tempered),
                  temper = .data$kap_ind_tempered / .data$EEK) %>%
    dplyr::filter('World' != .data$iso3c)

  EEK <- EEK %>%
    dplyr::full_join(temper %>% dplyr::select("iso3c", "temper"), by = "iso3c") %>%
    dplyr::mutate(EEK = .data$EEK * .data$temper, .keep = "unused")

  ## calculate EEK growth rates ----
  # EEK is assumed to stay constant in relation to subsector output, so it
  # grows/shrinks as the output grows/shrinks.  Shrinking of EEK is limited by
  # the depreciation rate i.
  EEK_change <- FEdemand %>%
    # select relevant subsector outputs, transform into usable format
    `[`(, , 'ue_', pmatch = 'left') %>%
    quitte::magclass_to_tibble(c('iso3c', 'year', 'scenario', 'subsector', 'value')) %>%
    filter(.data$subsector %in% c('ue_cement', 'ue_chemicals',
                                  'ue_steel_primary', 'ue_steel_secondary',
                                  'ue_otherInd')) %>%
    # calculate baseline change rates relative to base_year
    group_by(!!!syms(c('iso3c', 'scenario', 'subsector'))) %>%
    mutate(
      # Replace zeros with the first non-zero data.  This keeps EEK constant
      # back in time for countries/subsectors that have no historic production.
      value = ifelse(0 != .data$value,
                     .data$value,
                     first(.data$value[0 != .data$value],
                           order_by = .data$year)),
      # change in production relative to base year
      change = .data$value / .data$value[base_year == .data$year],
      # temper change down to avoid unduly high EEK in developing regions,
      # especially SSA
      change = sqrt(.data$change)
    )

  # find all countries/scenarios/subsectors where capital depreciation rate is
  # exceeded
  EEK_change_invalid_forward <- EEK_change %>%
    filter(base_year <= .data$year) %>%
    summarise(
      valid = all(.data$change >= ( lag(.data$change, order_by = .data$year)
                                  * (1 - i)
                                  ^ (.data$year - lag(.data$year,
                                                      order_by = .data$year))
                                  ),
                  na.rm = TRUE),
      .groups = 'drop') %>%
    filter(!.data$valid) %>%
    select(-'valid')

  EEK_change_invalid_backward <- EEK_change %>%
    filter(base_year >= .data$year) %>%
    summarise(
      valid = all(.data$change <= ( lag(.data$change,
                                        order_by = desc(.data$year))
                                  * (1 - i)
                                  ^ (.data$year - lag(.data$year,
                                                      order_by = desc(.data$year))
                                    )
                                  ),
                  na.rm = TRUE),
      .groups = 'drop') %>%
    filter(!.data$valid) %>%
    select(-'valid')

  # recalculate capital change rates
  EEK_change_valid_forward <- EEK_change %>%
    ungroup() %>%
    filter(base_year <= .data$year) %>%
    semi_join(
      EEK_change_invalid_forward,

      c('iso3c', 'scenario', 'subsector')
    ) %>%
    # duplicate year, as the variable gets lost during nesting
    mutate(year2 = .data$year) %>%
    group_by(.data$year2) %>%
    tidyr::nest() %>%
    pull(.data$data) %>%
    # Sequentially operate on two one-row data frames, x being the result of
    # the previous operation, or starting at the base year row, y the
    # 'current' row.  So y holds the baseline change rate computed earlier
    # (below), while x holds the change rate conforming to the capital
    # depreciation limit, as it has already been processed.  reduce() returns
    # only the last row of the computation, so we get one output row for each
    # of the input rows.
    purrr::reduce(
      .f = function(x, y) {
        bind_rows(x, y) %>%
          group_by(.data$iso3c, .data$scenario, .data$subsector) %>%
          # re-calculate change rate
          mutate(
            change = ifelse(
              # first row has already been processed
              1 == row_number(),
              .data$change,
              # capital after the base year can decrease by no more than the
              # capital depreciation rate i
              pmax(.data$change,
                   ( lag(.data$change, order_by = .data$year)
                     * (1 - i)
                     ^ (.data$year - lag(.data$year, order_by = .data$year))
                   )))) %>%
          ungroup() %>%
          select(-'value')
      })

  # do the same for backwards data
  EEK_change_valid_backward <- EEK_change %>%
    ungroup() %>%
    filter(base_year >= .data$year) %>%
    semi_join(
      EEK_change_invalid_backward,

      c('iso3c', 'scenario', 'subsector')
    ) %>%
    mutate(year2 = .data$year) %>%
    group_by(.data$year2) %>%
    tidyr::nest() %>%
    pull(.data$data) %>%
    purrr::reduce(
      .f = function(x, y) {
        bind_rows(x, y) %>%
          group_by(.data$iso3c, .data$scenario, .data$subsector) %>%
          # re-calculate change rate
          mutate(
            change = ifelse(
              # first row has already been processed (but is the last row as we
              # work backwards)
              n() == row_number(),
              .data$change,
              # capital before the base year can only have been higher by
              # the inverse of the depreciation rate i
              pmin(.data$change,
                   ( lead(.data$change, order_by = .data$year)
                   * (1 - i)
                   ^ (.data$year - lead(.data$year, order_by = .data$year))
                   )))) %>%
          ungroup() %>%
          select(-'value')
      },
      .dir = 'backward')

  # combine all valid change data
  EEK_change <- bind_rows(
    # valid data forward
    EEK_change %>%
      filter(base_year <= .data$year) %>%
      anti_join(
        EEK_change_invalid_forward,

        c('iso3c', 'scenario', 'subsector')
      ),

    # valid data backward
    EEK_change %>%
      filter(base_year > .data$year) %>%
      anti_join(
        EEK_change_invalid_backward,

        c('iso3c', 'scenario', 'subsector')
      ),
    # fixed rates
    EEK_change_valid_forward,
    EEK_change_valid_backward
  ) %>%
    distinct(.data$iso3c, .data$scenario, .data$subsector, .data$year, .data$change)

  EEK <- full_join(EEK %>% mutate(subsector = paste0('ue_', .data$subsector)),
                   EEK_change,
                   by = c("iso3c", "subsector")) %>%
    assertr::assert(assertr::not_na, tidyselect::everything()) %>%
    mutate(value = .data$EEK * .data$change,
           subsector = sub('^ue_', 'kap_', .data$subsector)) %>%
    select('iso3c', 'year', 'scenario', 'subsector', 'value')

  # quick-fix to infeasible 2025 SSA kap_steel_primary ----
  SSA_iso3c <- toolGetMapping('regionmappingH12.csv', 'regional', where = "mappingfolder") %>%
    as_tibble() %>%
    filter('SSA' == .data$RegionCode) %>%
    pull('CountryCode')

  EEK <- bind_rows(
    EEK %>%
      dplyr::anti_join(tibble::tibble(iso3c = SSA_iso3c,
                                      year = 2025,
                                      scenario = 'gdp_SSP5',
                                      subsector = 'kap_steel_primary'),
                       c('iso3c', 'year', 'scenario', 'subsector')),
    EEK %>%
      dplyr::semi_join(tibble::tibble(tidyr::crossing(iso3c = SSA_iso3c, year = c(2020, 2025, 2030)),
                                      scenario = 'gdp_SSP5',
                                      subsector = 'kap_steel_primary'),
                       by = c('iso3c', 'year', 'scenario', 'subsector')) %>%
      dplyr::summarise(value = mean(.data$value), year = 2025L, .by = c("iso3c", "scenario", "subsector"))
  )


  list(x = as.magpie(EEK, spatial = 1, temporal = 2, data = ncol(EEK)),
       weight = NULL,
       unit = 'trillion 2017US$',
       description = 'Industry energy efficiency capital stock')
}
