force_assignment_op <- function(pd) {
  to_replace <- pd$token == "EQ_ASSIGN"
  pd$token[to_replace] <- "LEFT_ASSIGN"
  pd$text[to_replace] <- "<-"
  pd
}

force_assignment_eq <- function(pd) {
  to_replace <- pd$token == "LEFT_ASSIGN" & pd$text == "<-"
  if (any(to_replace) && next_terminal(pd)$token == "'('") {
    pd$token[to_replace] <- "EQ_ASSIGN"
    pd$text[to_replace] <- "="
  }
  pd
}



resolve_semicolon <- function(pd) {
  is_semicolon <- pd$token == "';'"
  if (!any(is_semicolon)) {
    return(pd)
  }
  pd$lag_newlines[lag(is_semicolon)] <- 1L
  pd <- pd[!is_semicolon, ]
  pd
}
