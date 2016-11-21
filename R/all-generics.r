setGeneric("mindex",
  function(object,
           probes = NULL) standardGeneric("mindex"))

setGeneric("marray",
  function(object,
           probes = NULL,
           select = NULL,
           transpose = FALSE, ...) standardGeneric("marray"))

setGeneric("ma_values",
  function(object,
           probes = NULL,
           select = NULL, ...) standardGeneric("ma_values"))

setGeneric("ma_image",
  function(object,
           colors,
           select = NULL,
           legend.label,
           nrow = NULL,
           ncol = NULL,
           fixed = FALSE,
           range = NULL,
           empty.rows  = "fill",
           empty.thresh = 0.6,
           transform, ...) {
    empty.rows <- match.arg(empty.rows, c("fill", "drop", "ignore"))
    stopifnot(empty.thresh > 0 & empty.thresh <= 1)
    standardGeneric("ma_image")
})

