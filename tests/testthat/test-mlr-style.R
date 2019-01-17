context("test-mlr-style")

test_that("line break for multi-line function declaration", {
  test_collection("mlr-style", "line-break",
    transformer = style_text,
    style = mlr_style
  )
})
