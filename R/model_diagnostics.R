#' Compare fitted survival models using AIC and BIC
#'
#' Creates a model-comparison table containing Akaike information criterion
#' (AIC) and Bayesian information criterion (BIC) values for each fitted model.
#'
#' @param fits A fitted-model object returned by
#'   [fit_survival_models()].
#'
#' @return A data frame containing treatment arm, distribution, AIC, and BIC.
#'
#' @export
#'
#' @examples
#' lung_data <- data.frame(
#'   time = survival::lung$time,
#'   status = survival::lung$status - 1,
#'   treat = survival::lung$sex
#' )
#'
#' fits <- fit_survival_models(
#'   lung_data,
#'   dists_to_fit = c("weibull", "lnorm")
#' )
#'
#' model_diagnostics(fits)
model_diagnostics <- function(fits) {
  
  diagnostic_tables <- lapply(names(fits), function(arm_name) {
    
    models <- fits[[arm_name]]$models
    
    if (length(models) == 0) {
      return(NULL)
    }
    
    data.frame(
      arm = arm_name,
      distribution = names(models),
      AIC = vapply(models, stats::AIC, numeric(1)),
      BIC = vapply(models, stats::BIC, numeric(1)),
      row.names = NULL
    )
  })
  
  diagnostic_tables <- diagnostic_tables[
    !vapply(diagnostic_tables, is.null, logical(1))
  ]
  
  if (length(diagnostic_tables) == 0) {
    return(
      data.frame(
        arm = character(),
        distribution = character(),
        AIC = numeric(),
        BIC = numeric()
      )
    )
  }
  
  diagnostics <- do.call(rbind, diagnostic_tables)
  rownames(diagnostics) <- NULL
  
  return(diagnostics)
}