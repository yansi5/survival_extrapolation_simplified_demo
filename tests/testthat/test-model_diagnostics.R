test_that("model_diagnostics returns AIC and BIC for fitted models", {
  
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
  
  diagnostics <- model_diagnostics(fits)
  
  expect_equal(
    names(diagnostics),
    c("arm", "distribution", "AIC", "BIC")
  )
  
  expect_equal(
    nrow(diagnostics),
    4
  )
  
  expect_true(
    all(is.finite(diagnostics$AIC))
  )
  
  expect_true(
    all(is.finite(diagnostics$BIC))
  )
})