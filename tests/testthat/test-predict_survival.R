test_that("predict_survival returns survival predictions", {
  
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
  
  predictions <- predict_survival(
    fits,
    times = c(0, 100, 200)
  )
  
  expect_equal(
    names(predictions),
    c("time", "arm", "distribution", "survival")
  )
  
  expect_equal(
    nrow(predictions),
    12
  )
  
  expect_true(
    all(predictions$survival >= 0 &
          predictions$survival <= 1)
  )
})


test_that("predict_survival rejects negative time values", {
  
  lung_data <- data.frame(
    time = survival::lung$time,
    status = survival::lung$status - 1,
    treat = survival::lung$sex
  )
  
  fits <- fit_survival_models(
    data = lung_data,
    dists_to_fit = "weibull",
    arm_names = c(
      "1" = "Treatment 1",
      "2" = "Treatment 2"
    )
  )
  
  expect_error(
    predict_survival(
      fits,
      times = c(-1, 0, 100)
    ),
    "non-negative"
  )
})