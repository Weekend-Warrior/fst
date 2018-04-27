#' @export
write_fst.sparseMatrix <- function(x, path, compress = 50, uniform_encoding = TRUE) {
  if (!is.character(path)) stop("Please specify a correct path.")

  x <- attributes(as(x, 'TsparseMatrix'))

  if (any(names(x) %in% 'x')) indices <- c('i', 'j', 'x')
  else indices <- c('i', 'j')

  dt <- fststore(normalizePath(path, mustWork = FALSE),
                 'attr<-'('attr<-'(x[indices], 'row.names', 1:length(x$i)), 'class', 'data.frame'),
                 as.integer(compress),
                 uniform_encoding)

  if (inherits(dt, "fst_error")) {
    stop(dt)
  }

  return(invisible(x))
}
