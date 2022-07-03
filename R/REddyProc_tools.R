#' Fill gaps using EddyProc
#' dataset should contain a column called `gap` which is 0 where there is not gap and 1 wehre there is a gap
#' @param data
#' @param var character of variable to be gap-filled
fill_gaps_EProc <- function(data, var){
  EProc <- REddyProc::sEddyProc$new("ID", Data = data, ColNames = names(select(data, -TIMESTAMP_END)), ColPOSIXTime = "TIMESTAMP_END")
  EProc$sMDSGapFill(var, QFVar = "gap", QFValue = 0, V1 = "SW_IN", V2 = "VPD", V3 = "Tair", FillAll = FALSE)
  EProc$sExportResults()
}

#' Creates a random gap of given length and fill it
gap_fill_random_gap_EProc <- function(data, var, gap_length) {
  data_gap <- data %>%
    mutate(gap = random_gap(gap_length, nrow(data)))
  filled <- data_gap %>%
    fill_gaps_EProc(var)

  data_filled <- bind_cols(data_gap, filled) %>%
    filter(gap !=0 ) # the interesting part is only where there is the gap

  tibble(gap_length = gap_length, data = list(data_filled))
}

#' Vectorized version of gap legths
gap_fill_random_gap_EProc_vec <- function(data, var, gap_lengths){
  map_dfr(gap_lengths, ~gap_fill_random_gap_EProc(data, var, .x))
}

#' calculate RMSE or gap filled data
#' @param data
#' @param var variable to gap fill
gap_rmse <- function(data, var){
  rmse(pull(data, !!sym(var)), pull(data, !!sym(str_glue("{var}_f"))))
}

pretty_gap_len <- function(gap_length){
  str_glue("{prettyunits::pretty_dt(as.difftime(gap_length * 30, units=\"mins\"))} ({gap_length} obs.)")

}