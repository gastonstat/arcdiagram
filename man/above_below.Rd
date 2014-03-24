\name{above_below}
\alias{above_below}
\title{Above or Below}
\usage{
  above_below(edgelist, above)
}
\arguments{
  \item{edgelist}{two-column matrix}

  \item{above}{optional numeric or logical vector
  indicating what edges (arcs) should be plotted above (or
  to the right of) of chosen axis If \code{above = NULL}
  then all arcs are plotted above (or to the right) If
  \code{above} is numeric, it cannot contain both positive
  and negative indices. If \code{above} is logical, its
  length must equal the number of rows in \code{edgelist}}
}
\value{
  a logical vector indicating how arcs should be displayed
}
\description{
  Determines how arcs should be displayed
}
\details{
  If \code{horizontal = TRUE} then arcs can be plotted
  above or below the horizontal axis \cr If
  \code{horizontal = FALSE} then arcs can be plotted to the
  right or left of the vertical axis
}
\keyword{internal}

