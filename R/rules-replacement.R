force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}


resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) return(pd)
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}

remove_comments <- function(pd) {
  is_comment <- pd$token == "COMMENT"
  if (any(is_comment) && any(pd$lag_newlines[is_comment]) > 0L) {
    lag_newlines <- reduce(rev(which(is_comment)),
      function(x, index) add_to_element_above(x, index),
      .init = pd$lag_newlines
    )
    pd$lag_newlines <- lag_newlines
    pd <- pd[!is_comment,]

  }
  pd
}

#' Add elements in a vector to the elment before it
#'
#' @param x A vector
#' @param index A scalar indicating from which elements in the vector should
#'   be added to its left neighbour.
#' @examples
#' add_to_element_above(c(1, 2, 3), 2)
add_to_element_above <- function(x, index) {
  if (x[index] > 0L) {
    x[index - 1L] <- sum(x[c(index - 1L, index)])
  }
  x
}
