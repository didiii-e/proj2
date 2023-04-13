test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("check class", {
  expect_s3_class(linreg_model(data = mobility, train_val = .5, xvar1 =  foreign_share2010, yvar = kfr_pooled_pooled_p25, xvar2 = frac_coll_plus2010, xvar3 = share_black2010), "data.frame")
})
