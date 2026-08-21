test_that("plot_survival returns a ggplot object", {
  
  lung_data <- data.frame(
    time = survival::lung$time,
    status = survival::lung$status - 1,
    treat = survival::lung$sex
  )
  
  fits <- fit_survival_models(
    data = lung_data,
    dists_to_fit = c("weibull", "lnorm"),
    arm_names = c(
      "1" = "Treatment 1",
      "2" = "Treatment 2"
    )
  )
  
  plot <- plot_survival(
    fits,
    data = lung_data,
    times = seq(0, 1500, by = 50)
  )
  
  expect_s3_class(
    plot,
    "ggplot"
  )
})