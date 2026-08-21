#' Predict extrapolated survival probabilities
#'
#' Uses fitted parametric survival models to estimate survival probabilities
#' at user-specified time points.
#'
#' @param fits A fitted-model object returned by
#'   [fit_survival_models()].
#' @param times A numeric vector of non-negative time points at which
#'   survival probabilities should be predicted.
#'
#' @return A data frame containing time, treatment arm, distribution,
#'   and predicted survival probability.
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
#'   dists_to_fit = "weibull"
#' )
#'
#' predict_survival(
#'   fits,
#'   times = c(0, 100, 200)
#' )
predict_survival <- function(fits, times) {
  
  if (!is.numeric(times) || length(times) == 0) {
    stop("`times` must be a non-empty numeric vector.")
  }
  
  if (any(is.na(times)) || any(times < 0)) {
    stop("`times` must contain non-negative, non-missing values.")
  }
  
  times <- sort(unique(times))
  
  prediction_tables <- list()
  table_id <- 1
  
  for (arm_name in names(fits)) {
    
    models <- fits[[arm_name]]$models
    
    for (dist_name in names(models)) {
      
      model <- models[[dist_name]]
      
      prediction <- summary(
        model,
        t = times,
        type = "survival"
      )[[1]]
      
      prediction_tables[[table_id]] <- data.frame(
        time = prediction$time,
        arm = arm_name,
        distribution = dist_name,
        survival = prediction$est,
        row.names = NULL
      )
      
      table_id <- table_id + 1
    }
  }
  
  if (length(prediction_tables) == 0) {
    return(
      data.frame(
        time = numeric(),
        arm = character(),
        distribution = character(),
        survival = numeric()
      )
    )
  }
  
  predictions <- do.call(rbind, prediction_tables)
  rownames(predictions) <- NULL
  
  return(predictions)
}