\name{node_coords}
\alias{node_coords}
\title{Node Coordinates}
\usage{
  node_coords(edgelist, sorted = FALSE, decreasing = FALSE,
    ordering = NULL, labels = NULL)
}
\arguments{
  \item{edgelist}{basically a two-column matrix with edges
  (see \code{\link{graph}})}

  \item{sorted}{logical to indicate if nodes should be
  sorted}

  \item{decreasing}{logical to indicate type of sorting
  (used only when \code{sorted=TRUE})}

  \item{ordering}{optional numeric vector providing the
  ordering of nodes (when provided, this parameter
  overrides \code{sorted=TRUE})}

  \item{labels}{character vector with labels for the nodes}
}
\value{
  a vector with the location of nodes in the x-axis
}
\description{
  Computes axis locations of each node. This function can
  be helpful when you want to separately plot the node
  labels using the function mtext.
}
\examples{
\dontrun{
 # generate a graph
 some_graph = graph.ring(10)

 # add names to nodes
 V(some_graph)$name = letters[1:vcount(some_graph)]

 # extract edgelist
 edgelist = get.edgelist(some_graph)

 # (default) arc diagram
 arcplot(edgelist, labels=V(some_graph)$name, las=1)

 # get x-axis coordinates of nodes
 xcoords = node_coords(edgelist, labels=V(some_graph)$name)

 # arc diagram with various labels
 arcplot(edgelist, show.labels=FALSE, show.nodes=TRUE)
 mtext(V(some_graph)$name, side=1, line=0, at=xcoords)
 mtext(rep("node",10), side=1, line=1, at=xcoords, col="gray90")
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{arcplot}}
}

