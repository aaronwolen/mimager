check_probes <- function(x) {
  if(is.null(x)) x <- "pm"
  match.arg(tolower(x), c("pm", "mm", "both"))
}


check_probe <- function(object, probe) {
  if(is.null(probe)) probe <- "pm"
  choices <- switch(class(object),
    AffyBatch            = c("all", "pm", "mm"),
    ExpressionFeatureSet = c("all", "pm", "mm"),
    GeneFeatureSet       = c("all", "pm", "bg"),
    ExonFeatureSet       = c("all", "pm", "mm", "bg")
  )
  if (all(probe %in% choices)) return(probe)
  stop(
    "Invalid probe for ", class(object), " should be one of ",
    paste(choices, collapse = ", "),
    call. = FALSE
  )
}