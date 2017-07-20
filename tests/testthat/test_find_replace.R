library(datacleanr)
context("find_replace check")

model <- rownames(mtcars)
new= rep("Merc 1", dim(mtcars)[1])
mcars <- cbind(mtcars,
               model,
               new,
               stringsAsFactors=FALSE)

make <- c("(?i)^merc"="BMW")


test_that("one variable replacement takes place",{
  mcars <- find_replace(mcars,
                        .replace="model",
                        .name_vec=make)
  index <- grepl("BMW", mcars$model)
  expect_equal(all(grepl("BMW",mcars$model[index])), TRUE)
})


test_that("multiple variable replacement takes place",{
  mcars <- find_replace(mcars,
                        .replace=c("mpg", "model", "new"),
                        .name_vec=make)
  index <- grepl("BMW", mcars$model)
  expect_equal(all(grepl("BMW",mcars$mpg[index])), FALSE)
  expect_equal(all(grepl("BMW",mcars$model[index])), TRUE)
  expect_equal(all(grepl("BMW",mcars$new[index])), TRUE)
})


test_that("all variable replacement takes place",{
  mcars <- find_replace(mcars,
                        .all_variables=TRUE,
                        .name_vec=make)
  index <- grepl("BMW", mcars$model)
  expect_equal(all(grepl("BMW",mcars$mpg[index])), FALSE)
  expect_equal(all(grepl("BMW",mcars$model[index])), TRUE)
  expect_equal(all(grepl("BMW",mcars$new[index])), TRUE)
})


test_that("variable replacement on an index takes place",{
  index <- with(mcars, mpg == 24.4 &
                       cyl == 4 &
                       disp == 146.7)
  mcars <- find_replace(mcars,
                        .replace="new",
                        .index=index,
                        .name_vec=make)
  expect_equal(all(grepl("BMW", mcars$new[index])), TRUE) 
})

