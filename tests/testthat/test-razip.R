test_that("zip", {
  zip=system.file("sampledata/Cell07PNs-rds.zip", package = 'razip')
  expect_true(is.R6(raz <- RAZip$new(zip)))
  expect_true(is.data.frame(zl <- raz$ziplist()))
  expect_equal(digest::digest(raz$mget(zl$filename)),
               "d2e2acc4fb33945b584cfd0e74859266")
})
