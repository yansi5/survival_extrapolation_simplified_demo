#' Fit parametric survival models by treatment arm
#'
#' Fits one or more standard parametric survival distributions separately
#' for each treatment arm using `flexsurv::flexsurvreg()`.
#'
#' @param data A data frame containing the variables `time`, `status`,
#'   and `treat`. `status` should be coded as 0 for censored observations
#'   and 1 for events.
#' @param dists_to_fit A character vector containing the names of the
#'   parametric distributions to fit.
#' @param arm_names An optional named character vector used to label the
#'   treatment arms. The names should correspond to the values of `treat`.
#'
#' @return A named list containing the successfully fitted survival models
#'   for each treatment arm.
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
#'   data = lung_data,
#'   dists_to_fit = c("weibull", "lnorm"),
#'   arm_names = c(
#'     "1" = "Treatment 1",
#'     "2" = "Treatment 2"
#'   )
#' )
fit_survival_models <- function(
    data,
    dists_to_fit = c(
      "exp",
      "weibull",
      "gamma",
      "lnorm",
      "llogis",
      "gompertz",
      "gengamma"
    ),
    arm_names = NULL
) {
  
  # Check required columns
  required_columns <- c("time", "status", "treat")
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0) {
    stop(
      "`data` is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # Check survival data
  if (any(is.na(data$time)) ||
      any(is.na(data$status)) ||
      any(is.na(data$treat))) {
    stop("`time`, `status`, and `treat` must not contain missing values.")
  }
  
  if (any(data$time < 0)) {
    stop("`time` must contain non-negative values.")
  }
  
  if (!all(data$status %in% c(0, 1))) {
    stop("`status` must be coded 0 for censored and 1 for event.")
  }
  
  arm_codes <- unique(data$treat)
  
  # Use treatment codes as labels if arm_names is not supplied
  if (is.null(arm_names)) {
    arm_names <- stats::setNames(
      as.character(arm_codes),
      as.character(arm_codes)
    )
  }
  
  if (is.null(names(arm_names)) ||
      !all(as.character(arm_codes) %in% names(arm_names))) {
    stop(
      "`arm_names` must be a named vector covering all values in `data$treat`."
    )
  }
  
  results_by_arm <- list()
  
  for (arm_code in arm_codes) {
    
    arm_name <- unname(arm_names[as.character(arm_code)])
    
    arm_data <- data[
      data$treat == arm_code,
      ,
      drop = FALSE
    ]
    
    fitted_models <- stats::setNames(
      lapply(dists_to_fit, function(dist_name) {
        
        tryCatch(
          flexsurv::flexsurvreg(
            survival::Surv(time, status) ~ 1,
            data = arm_data,
            dist = dist_name
          ),
          error = function(e) {
            message(
              "Model failed for arm '", arm_name,
              "' using distribution '", dist_name, "'."
            )
            NULL
          }
        )
      }),
      dists_to_fit
    )
    
    # Remove models that failed
    fitted_models <- fitted_models[
      !vapply(fitted_models, is.null, logical(1))
    ]
    
    if (length(fitted_models) == 0) {
      warning(
        "No models were successfully fitted for arm '",
        arm_name,
        "'."
      )
    }
    
    results_by_arm[[arm_name]] <- list(
      arm_code = arm_code,
      models = fitted_models
    )
  }
  
  return(results_by_arm)
}