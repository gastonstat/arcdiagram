\name{graph_info}
\alias{graph_info}
\title{Graph Information}
\usage{
  graph_info(edgelist, vertices, sorted = FALSE,
    decreasing = FALSE, ordering = NULL, labels = NULL)
}
\arguments{
  \item{edgelist}{basically a two-column matrix with edges
  (see \code{\link{graph}})}

  \item{vertices}{optional vector of vertex names
  corresponding with those in the edgelist}

  \item{sorted}{logical to indicate if nodes should be
  sorted (default \code{FALSE})}

  \item{decreasing}{logical to indicate type of sorting
  (used only when \code{sorted=TRUE})}

  \item{ordering}{optional numeric or string vector
  providing the ordering of nodes. When provided, this
  parameter overrides \code{sorted=TRUE}). See the details
  section for more information.}

  \item{labels}{optional string vector with labels for the
  nodes}
}
\description{
  Gets graph information
}
\keyword{internal}

