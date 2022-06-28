#' Knit template
#' @description 
#' takes a markdown file and expands it using `knit_expand` and then `knit_child`.
#' use this in a chunk only with this function call and `results = 'asis'`
#' @param files paths of the templates 
#' @param ... the variables values used during template expansion
#' @details The variables values can be a vector, in this case the function will render the template for each value of the vector
#'  In case of multiple variables it iterates together
knit_template <- function(files, ..., exec = T) {
  args <- tibble::tibble(!!!rlang::list2(...))
  files %>% walk(
    function(file) {
      src <- pmap(args, function(...) {
        knitr::knit_expand(file, ...)
      })
      if (isTRUE(getOption("knitr.in.progress"))) {
        res <- knitr::knit_child(text = src, quiet = TRUE)
        cat(res, sep = "\n")
      } else {
        if (exec) {
          source_rmd(text = src)
        }
      }
    }
  )
}

#' save knit expand
#' @description 
#' save result of `knit_expand` as a notebook file
#' @export
save_knit_expand <- function(template, out_name=tempfile(fileext = ".Rmd"), ..., open=T){
  res <- knitr::knit_expand(template, ...)
  write(res, out_name)
  if(open) rstudioapi::navigateToFile(out_name)
}