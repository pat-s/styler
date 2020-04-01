context("test-mlr-style")

test_that("line break for multi-line function declaration", {
  test_collection("mlr-style", "line-break",
    transformer = style_text,
    style = mlr_style
  )

  test_collection("mlr-style", "fun-decs-comments",
    transformer = style_text,
    style = mlr_style
  )
  test_collection("mlr-style", "data-table",
                  transformer = style_text,
                  style = mlr_style
  )
  test_collection("mlr-style", "eq-sub-replacement",
                  transformer = style_text,
                  style = mlr_style
  )
})
