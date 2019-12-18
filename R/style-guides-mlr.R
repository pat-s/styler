
#' The mlr style
#'
#' Style code according to the mlr style guide.
#' @inheritParams tidyverse_style
#' @param min_lines_for_break The minimal number of lines required within a
#'   function declaration to make styler adding a blank line between header and
#'   body.
#' @details The following options for `scope` are available.
#'
#' * "none": Performs no transformation at all.
#' * "spaces": Manipulates spacing between token on the same line.
#' * "indention": In addition to "spaces", this option also manipulates the
#'   indention level.
#' * "line_breaks": In addition to "indention", this option also manipulates
#'   line breaks.
#' * "tokens": In addition to "line_breaks", this option also manipulates
#'   tokens.
#'
#' As it becomes clear from this description, more invasive operations can only
#' be performed if all less invasive operations are performed too.
#' @family obtain transformers
#' @family style_guides
#' @examples
#' style_text("call( 1)", style = mlr_style, scope = "spaces")
#' style_text("call( 1)", transformers = mlr_style(strict = TRUE))
#' @importFrom purrr partial
#' @export
mlr_style <- function(scope = "tokens",
                      strict = TRUE,
                      indent_by = 2,
                      start_comments_with_one_space = FALSE,
                      reindention = tidyverse_reindention(),
                      math_token_spacing = tidyverse_math_token_spacing(),
                      min_lines_for_break = 10) {
  scope <- character_to_ordered(
    scope,
    c("none", "spaces", "indention", "line_breaks", "tokens")
  )

  space_manipulators <- if (scope >= "spaces") {
    lst(
      indent_braces = partial(indent_braces, indent_by = indent_by),
      indent_op = partial(indent_op, indent_by = indent_by),
      indent_eq_sub = partial(indent_eq_sub, indent_by = indent_by),
      indent_without_paren = partial(indent_without_paren,
                                     indent_by = indent_by
      ),
      fix_quotes,
      remove_space_before_closing_paren,
      remove_space_before_opening_paren = if (strict) remove_space_before_opening_paren,
      add_space_after_for_if_while,
      add_space_before_brace,
      remove_space_before_comma,
      style_space_around_math_token = partial(
        style_space_around_math_token, strict,
        math_token_spacing$zero,
        math_token_spacing$one
      ),
      style_space_around_tilde = partial(
        style_space_around_tilde, strict = strict
      ),
      spacing_around_op = purrr::partial(
        set_space_around_op, strict = strict
      ),
      spacing_around_comma = if (strict) {
        #set_space_after_comma
        add_space_after_comma
      } else {
        add_space_after_comma
      },
      remove_space_after_opening_paren,
      remove_space_after_excl,
      set_space_after_bang_bang,
      remove_space_before_dollar,
      remove_space_after_fun_dec,
      remove_space_around_colons,
      start_comments_with_space = partial(start_comments_with_space,
                                          force_one = start_comments_with_one_space
      ),
      remove_space_after_unary_pm_nested,
      spacing_before_comments = if (strict) {
        set_space_before_comments
      } else {
        add_space_before_comments
      },
      set_space_between_levels,
      set_space_between_eq_sub_and_comma
    )
  }

  use_raw_indention <- scope < "indention"

  line_break_manipulators <- if (scope >= "line_breaks") {
    lst(
      remove_line_break_before_round_closing_fun_dec =
        if (strict) remove_line_break_before_round_closing_fun_dec,
      style_line_break_around_curly = partial(style_line_break_around_curly,
                                              strict
      ),
      remove_line_break_in_empty_fun_call,
      add_line_break_after_pipe,
      # this breaks }) into separate lines, see https://github.com/r-lib/styler/issues/514#issue-443293104
      # add_line_break_before_round_closing_after_curly,

      # should be last because it depends on other line breaks via n_lines()
      partial(set_line_break_after_fun_dec_header,
              min_lines_for_break = min_lines_for_break
      )
    )
  }

  token_manipulators <- if (scope >= "tokens") {
    lst(
      force_assignment_eq,
      resolve_semicolon,
      add_brackets_in_pipe,
      remove_terminal_token_before_and_after,
      wrap_if_else_while_for_fun_multi_line_in_curly =
        if (strict) wrap_if_else_while_for_fun_multi_line_in_curly
    )
  }


  indention_modifier <- lst()

  create_style_guide(
    # transformer functions
    initialize          = default_style_guide_attributes,
    line_break          = line_break_manipulators,
    space               = space_manipulators,
    token               = token_manipulators,
    indention           = indention_modifier,
    # transformer options
    use_raw_indention   = use_raw_indention,
    reindention         = reindention,
    style_guide_name    = "styler::mlr_style@https://github.com/pat-s/styler/tree/mlr-style",
    style_guide_version = styler_version
  )
}

style_mlr <- function(text, ...) {
  style_text(text, transformers = mlr_style(...))
}
