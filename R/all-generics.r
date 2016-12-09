setGeneric("mindex",
  function(object,
           probes = NULL) standardGeneric("mindex"))

#' @rdname marray
#' @export
setGeneric("marray",
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE, ...) standardGeneric("marray"))

#' @rdname mimage
#' @export
setGeneric("mimage",
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           empty.rows  = "fill",
           empty.thresh = 0.6,
           transform,
           trim = 0.01, ...) {
    empty.rows <- match.arg(empty.rows, c("fill", "drop", "ignore"))
    stopifnot(empty.thresh > 0 & empty.thresh <= 1)
    standardGeneric("mimage")
})

