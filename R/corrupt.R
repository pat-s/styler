#' Corrupt a file
#'
#' Randomize token position before serialization to make people wonder.
#' @inheritParams style_file
#' @inheritParams permute
#' @export
corrupt_file <- function(path,
                         extend = 0.3,
                         flat = FALSE,
                         transformers = get_transformers(flat = flat)) {
  withr::with_dir(dirname(path),
                  prettify_one_random(transformers,
                                      flat,
                                      basename(path),
                                      extend))
}

prettify_one_random <- function(transformers, flat, path, extend) {
  if (!grepl("\\.[Rr]$", path)) stop(path, " is not a .R file")
  transform_files_nested_random(path, transformers, extend)
}

transform_files_nested_random <- function(files, transformers, extend) {
  transformer <- make_transformer_nested_random(transformers, extend)

  changed <- utf8::transform_lines_enc(files, transformer)
  if (any(changed)) {
    message("Please review the changes carefully!")
  }
  invisible(changed)
}


make_transformer_nested_random <- function(transformers, extend) {
  function(text) {
    text <- gsub(" +$", "", text)
    text <- gsub("\t", "        ", text)

    pd_nested <- compute_parse_data_nested(text)
    transformed_pd_nested <- visit(pd_nested, transformers)
    # TODO verify_roundtrip
    new_text <- serialize_parse_data_nested_random(transformed_pd_nested, extend)
    new_text
  }
}

serialize_parse_data_nested_random <- function(pd_nested, extend) {
  out <- c(add_newlines(start_on_line(pd_nested) - 1),
           serialize_parse_data_nested_helper(pd_nested, pass_indent = 0)) %>%
    unlist() %>%
    permute(extend) %>%
    paste0(collapse = "") %>%
    strsplit("\n", fixed = TRUE) %>%
    .[[1L]]
  out
}

#' Permute elements in a vector
#'
#' @param vec A vector to permute.
#' @param extend The extend of permutation. Zero means no permutation, 1
#'   means permute all tokens in one pool, 0.5 for example means permute
#'   the first half of the token among themselves and the second half of the
#'   token among themselves.
permute <- function(vec, extend) {
  if (extend == 0) {
    extend <- 1 / length(vec)
  }
  scale <- round(1 / extend)
  draw <- sort(c(sample(scale, scale, replace = FALSE),
                 sample(scale, length(vec) - scale, replace = TRUE)))
  split(vec, draw) %>%
    map(sample) %>%
    unlist()
}
