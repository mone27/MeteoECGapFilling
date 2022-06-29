#' split vector in equal chuncks
split_vector<-function(x,k) split(x, ceiling(seq_along(x)/k))

#' Logaritmically spaced sequence
#' @param start
#' @param end
#' @param offset The offset from which the log scale should be started.
#'          The farther is is from 0 the more linear is the log scale
#' @seealso `seq`
seq_log <- function(start, end, offset, ...) {
  exp(seq(log(start + offset), log(end + offset), ...)) - offset
}