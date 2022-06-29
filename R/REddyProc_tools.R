#' Fill gaps using EddyProc
#' dataset should contain a column called `gap` which is 0 where there is not gap and 1 wehre there is a gap
#' @param data
#' @param var character of variable to be gap-filled
fill_gaps_EProc <- function(data, var){
  EProc <- REddyProc::sEddyProc$new("ID", Data = data, ColNames = names(select(data, -TIMESTAMP_END)), ColPOSIXTime = "TIMESTAMP_END")
  EProc$sMDSGapFill(var, QFVar = "gap", QFValue = 0, V1 = "SW_IN", V2 = "VPD", V3 = "Tair", FillAll = FALSE)
  EProc$sExportResults()
}

random_gap <- function(gap_length, total_length) {
  # constructing an array to indetify gaps
  # it should be always false (no gap) except for a continous section of lenght `gap_length`
  if(gap_length >= total_length){
    return(rep(1, total_length))
  }
  gap_length <- ifelse(gap_length > total_length, total_length, gap_length)
  gap_start <- sample.int(total_length - gap_length, 1) # gap cannot start too close to the end
  c(
          rep(0, gap_start),
          rep(1, gap_length),
          rep(0, total_length - (gap_length + gap_start))
  )
}

#' Fill an artificially created gap of the given length and calculate RMSE
#' @param data
#' @param var variable to gap fill
#' @param gap_length the lenght of the gap
gap_fill_EProc_rmse_single <- function(data, var, gap_length){
   data_gap <- data %>%
      mutate(gap = random_gap(gap_length , nrow(.)))
  filled <- data_gap %>%
      fill_gaps_EProc(var)
  # some rlang magic needed to have filter() working properly (as it doesn't work with string variable names) with var being a string
  var_fqc <- sym(str_glue("{var}_fqc"))
  var_f <- sym(str_glue("{var}_f"))

  var_filled <- filled %>%
      filter(!!var_fqc != 0) %>%
      pull(!!var_f)

  var_orig <- data_gap %>%
      filter(gap != 0) %>%
      pull(!!var)

  tibble(
      rmse = rmse(var_orig, var_filled),
      gap_length = gap_length
  )
}

#' Vectorized version
gap_fill_EProc_rmse <- function(data, var, gap_lengths){
  map_dfr(gap_lengths, ~gap_fill_EProc_rmse_single(data, var, .x))
}