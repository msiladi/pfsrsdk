#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Tests for getEntityMetadata().
context("Tests for getEntityMetadata")

test_that(paste("test getEntityMetadata() on:", env$auth), {
  out <- getEntityMetadata(con$coreApi, data$testPocoType, useVerbose = verbose)

  # Check to verify that the function returned a list. dependending on tenant this list may be empty
  expect_equivalent(class(out$template), "list", all = verbose)
})

test_that(paste("test getEntityMetadata() errors out if entityType does not exist on:", env$auth), {
  expect_error(getEntityMetadata(con$coreApi, data$testPocoNonexistentType, useVerbose = verbose))
})
