#' Plot observed and extrapolated survival curves
#'
#' Plots observed Kaplan-Meier survival estimates together with fitted
#' parametric survival curves. Treatment arms are shown in separate panels.
#'
#' @param fits A fitted-model object returned by
#'   [fit_survival_models()].
#' @param data A data frame containing `time`, `status`, and `treat`.
#' @param times A numeric vector of non-negative time points used for
#'   parametric survival prediction and extrapolation.
#'
#' @return A `ggplot2` plot object.
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
#' plot_survival(
#'   fits,
#'   data = lung_data,
#'   times = seq(0, 1500, by = 50)
#' )
plot_survival <- function(fits, data, times) {
  
  predictions <- predict_survival(fits, times)
  
  if (nrow(predictions) == 0) {
    stop("No fitted models are available to plot.")
  }
  
  km_tables <- lapply(names(fits), function(arm_name) {
    
    arm_code <- fits[[arm_name]]$arm_code
    
    arm_data <- data[
      data$treat == arm_code,
      ,
      drop = FALSE
    ]
    
    km_fit <- survival::survfit(
      survival::Surv(time, status) ~ 1,
      data = arm_data
    )
    
    data.frame(
      time = c(0, km_fit$time),
      survival = c(1, km_fit$surv),
      arm = arm_name,
      row.names = NULL
    )
  })
  
  km_data <- do.call(rbind, km_tables)
  rownames(km_data) <- NULL
  
  ggplot2::ggplot(
    predictions,
    ggplot2::aes(
      x = time,
      y = survival,
      colour = distribution
    )
  ) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_step(
      data = km_data,
      mapping = ggplot2::aes(
        x = time,
        y = survival
      ),
      inherit.aes = FALSE,
      colour = "black",
      linewidth = 0.7
    ) +
    ggplot2::facet_wrap(~arm) +
    ggplot2::coord_cartesian(
      xlim = range(times),
      ylim = c(0, 1)
    ) +
    ggplot2::labs(
      x = "Time",
      y = "Survival probability",
      colour = "Parametric model"
    ) +
    ggplot2::theme_minimal()
}