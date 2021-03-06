% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse.R
\name{ensure_valid_pd}
\alias{ensure_valid_pd}
\title{Ensure that the parse data is valid}
\usage{
ensure_valid_pd(pd)
}
\arguments{
\item{pd}{A parse table.}
}
\description{
Test whether all non-terminals have at least one child and throw an error
otherwise. As this is check is rather expensive, it is only
carried out for configurations we have good reasons to expect problems.
}
\keyword{internal}
