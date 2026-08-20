test_that("fit_survival_models fits models for two arms", {
  
  lung_data <- data.frame(
    time = survival::lung$time,
    status = survival::lung$status - 1,
    treat = survival::lung$sex
  )
  
  fits <- fit_survival_models(
    data = lung_data,
    dists_to_fit = c("weibull", "lnorm"),
    arm_names = c(
      "1" = "Male",
      "2" = "Female"
    )
  )
  
  expect_equal(
    names(fits),
    c("Male", "Female")
  )
  
  expect_equal(
    names(fits$`Male`$models),
    c("weibull", "lnorm")
  )
  
  expect_equal(
    names(fits$`Female`$models),
    c("weibull", "lnorm")
  )
})