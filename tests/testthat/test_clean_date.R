library(datacleanr)
context("date check")

test_that("blanks are removed from dates", {
  expect_equal(clean_date("14 /2 /2016"), "2016-02-14")
  expect_equal(clean_date("1 4/2/ 16"), "2016-02-14")
  expect_equal(clean_date("    14/2/2016"), "2016-02-14")
})

test_that("periods are removed from dates", {
  expect_equal(clean_date("14.2.2016"), "2016-02-14")
  expect_equal(clean_date("14..2..16"), "2016-02-14")
  expect_equal(clean_date(".....14/2/2016"), "2016-02-14")
})

test_that("additional slashes are removed from dates", {
  expect_equal(clean_date("14//2/2016"), "2016-02-14")
  expect_equal(clean_date("14/2//16"), "2016-02-14")
  expect_equal(clean_date("14/////2/////2016"), "2016-02-14")
})

test_that("leading or trailing slashes are removed from dates", {
  expect_equal(clean_date("////14/2/2016"), "2016-02-14")
  expect_equal(clean_date("14/2/16/////"), "2016-02-14")
})

test_that("dates with a two/four digit specifier for year", {
  expect_equal(clean_date("14/2/16"), "2016-02-14")
  expect_equal(clean_date("14/2/2016"), "2016-02-14")
})

test_that("dates with a month-day swap", {
  expect_equal(clean_date("02/14/16"), "2016-02-14")
  expect_equal(clean_date("02/14/02"), "2002-02-14")
  expect_equal(clean_date("14/2/2016"), "2016-02-14")
})

test_that("dates with an Excel five digit specifier are getting transformed", {
  expect_equal(clean_date("42414"), "2016-02-14")
})

test_that("non-dates and other unclear data are identified and returned", {
  expect_equal(clean_date(c("14/2/16", "xyz")), c("2016-02-14", "xyz"))
  expect_equal(clean_date(c("14/2/16", "xyz"), unresolved=TRUE)$unresolved, c(FALSE, TRUE))
})
