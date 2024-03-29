#' @author Adam Wheeler adam.wheeler@thermofisher.com
#' @author Scott Russell scott.russell@thermofisher.com
#' @description \code Test for odataCleanName.

context("Tests for odataCleanName")

test_that(paste("odataCleanName() replaces spaces with '_' and converts everything to uppercase"), {
  CleanedUp <- odataCleanName("1 make spaces underscores What about numbers 123453 32 _ and symbols!@#$%^&*()_+ -=`~ ")
  expect_equivalent(CleanedUp, "_1_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+__=`~_")

  CleanedUp <- odataCleanName("1 make spaces underscores What about numbers 123453 32 _ and symbols!@#$%^&*()_+ -=`~ ", "alias")
  expect_equivalent(CleanedUp, "1_make_spaces_underscores_What_about_numbers_123453_32___and_symbols!@#$%^&*()_+__=`~_")
})
