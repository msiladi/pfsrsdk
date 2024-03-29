#' @author Natasha Mora natasha.mora@thermofisher.com
#' @description \code Tests for getExperimentContainers

context("Tests for getExperimentContainers")

test_that(paste("test getExperimentContainers() on:", env$auth), {
  result <- getExperimentContainers(con$coreApi, data$experimentType, data$experimentBarcode, useVerbose = verbose)

  expect_equal(result$response$status_code, 200)

  expect_gt(length(result$entity), 0)
})
