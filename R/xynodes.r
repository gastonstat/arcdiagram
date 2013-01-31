#'@title X or Y coordinates of node locations
#'
#'@description
#'Gives axis locations of each node. This function can be helpful when you want
#'to separately plot the node labels using the function mtext.
#'
#'@param edgelist basically a two-column matrix with edges (see \code{\link{graph}})
#'@param sorted logical to indicate if nodes should be sorted
#'@param decreasing logical to indicate type of sorting (used only when \code{sorted=TRUE})
#'@param ordering optional numeric vector providing the ordering of nodes
#'(when provided, this parameter overrides \code{sorted=TRUE})
#'@param labels character vector with labels for the nodes
#'@return a vector with the location of nodes in the x-axis
#'@author Gaston Sanchez
#'@seealso \code{\link{arcplot}}
#'@export
#'@examples
#'
#'  \dontrun{
#'  # generate a graph
#'  some_graph = graph.ring(10)
#'  
#'  # add names to nodes
#'  V(some_graph)$name = letters[1:vcount(some_graph)]
#'  
#'  # extract edgelist
#'  edgelist = get.edgelist(some_graph)
#'
#'  # (default) arc diagram
#'  arcplot(edgelist, labels=V(some_graph)$name, las=1)
#'  
#'  # get x-axis coordinates of nodes
#'  xcoords = xynodes(edgelist, labels=V(some_graph)$name)
#'  
#'  # arc diagram with various labels
#'  arcplot(edgelist, show.labels=FALSE, show.nodes=TRUE)
#'  mtext(V(some_graph)$name, side=1, line=0, at=xcoords)
#'  mtext(rep("node",10), side=1, line=1, at=xcoords, col="gray90")
#'  }
#'

#'
xynodes <- function(
  edgelist, sorted = FALSE, decreasing = FALSE, ordering = NULL, labels = NULL)
{
  # ======================================================
  # Checking edgelist
  # ======================================================
  # make sure edgelist is a two-column matrix
  if (!is.matrix(edgelist) || ncol(edgelist) != 2)
    stop("\nSorry, 'edgelist' must be a two column matrix")
  # how many edges
  num_edges = nrow(edgelist)
  # get nodes
  nodes = unique(as.vector(edgelist))
  # how many nodes
  num_nodes = length(nodes)  
  # node numeric sequence
  nums = seq_along(nodes)
  # check labels (ie node names)
  if (!is.null(labels))
  {
    if (length(nodes) != length(labels))
      stop("\nLength of 'labels' differs from number of nodes")
  } else {
    labels = nodes
  }
  # auxiliar order (this may change if sorted or ordering required)
  aux_ord = 1:num_nodes  
  # If sorted is required, ennumerate nodes
  if (sorted) {
    ord_nodes = order(nodes, decreasing=decreasing)
    nodes = nodes[ord_nodes]
    nums = nums[ord_nodes]
    labels = labels[ord_nodes]
    # auxiliar order
    aux_ord = ord_nodes
  }
  # If ordering is provided, re-ennumerate nodes
  if (!is.null(ordering)) {
    if (length(ordering) != num_nodes)
      stop("\nLength of 'ordering' differs from number of nodes")
    nodes = nodes[ordering]
    nums = nums[ordering]
    labels = labels[ordering]
    # auxiliar order
    aux_ord = ordering
  }
  
  # ======================================================
  # Coordinates of nodes (i.e. vertices)
  # ======================================================
  # node labels at equal distances from each other
  nf = rep(1 / num_nodes, num_nodes)
  # center coordinates of node labels
  fin = cumsum(nf)
  ini = c(0, cumsum(nf)[-num_nodes])
  centers = (ini + fin) / 2
  names(centers) = labels[aux_ord]
  # result
  centers

  # --------------------------------
  # brought to you by Gaston Sanchez
  # www.gastonsanchez.com
  # --------------------------------
}
