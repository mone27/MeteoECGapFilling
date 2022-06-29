#' Source a Rmd file
#' extract the R code an R Markdown and sources everything
#' @param file_path notebook path
#' @export
source_rmd <- function(file_path = NULL, text = NULL) {
  if(!is.null(file_path)){
    stopifnot(is.character(file_path) && length(file_path) == 1)
    full_rmd <- read_file(file_path)
  }
  if(!is.null(text)){
    full_rmd <- text
  }
  else{
    rlang::abort("Either file_path or text needs to be non NULL")
  }

  .tmpfile <- tempfile(fileext = ".R")
  rlang::inform(stringr::str_glue("Sourcing from {.tmpfile}"))
  .con <- file(.tmpfile) 
  on.exit(close(.con))

  codes <- stringr::str_match_all(string = full_rmd, pattern = "```(?s)\\{r[^{}]*\\}\\s*\\n(.*?)```")
  stopifnot(length(codes) == 1 && ncol(codes[[1]]) == 2)
  codes <- paste(codes[[1]][, 2], collapse = "\n")
  writeLines(codes, .con)
  flush(.con)
  cat(sprintf("R code extracted to tempfile: %s\nSourcing tempfile...", .tmpfile))
  source(.tmpfile)
}
