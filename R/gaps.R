#' Put a random gap of length gap_length
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

#' makes the gap having a fixed legth
#' if the gap is longer is selecting a random subset of the desired length
#' if is short is adding data around taking from the site_data
#' Requirements:
#' - gap and site_data should have a column called `TIMESTAMP_END`
#' - gap filled columns should end with 'f'
#' @param gap the gapfilled dataframe
#' @param site_data the original dataframe that contains all obersvations
#' @param len the number of elements for the final gap. Defaults to one week
#' @return gap with `len` number of obeservations
to_fixed_gap_length <- function(gap, site_data, len = 7 * 24 * 12) {
  if (nrow(gap) < len) {
    # need to add more data around the gap
    # understand when the gap fits in the rest of data
    start_gap <- which(site_data$TIMESTAMP_END == gap$TIMESTAMP_END[1])
    start_around <- max(start_gap - (len - nrow(gap)) / 2, 1) %>% as.integer() # ensure it doesn't get below 1
    data_around <- site_data %>%
      slice(start_around:(start_around + len - 1))
    gap %>%
      select(TIMESTAMP_END, ends_with("f")) %>%  # getting only gap filled data not original cols
      right_join(data_around, by = "TIMESTAMP_END")
  }

  else {
    # find a week inside the gap
    selection <- random_gap(len, nrow(gap)) # find a random period of right length in the gap
    gap %>%
      filter(selection == 1)
  }
}