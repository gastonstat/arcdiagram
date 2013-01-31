#'@title Arc Diagram Plot
#'
#'@description
#'Give me an edgelist and I'll help you plot a pretty damn arc diagram
#'
#'@details
#'The arcs are scaled such that they fit in a plot region with its
#'x-axis ranging from zero to one. Node symbols and labels can be
#'optionally displayed. Node symbols are displayed through
#'the function \code{points}. In turn, node labels are displayed
#'through the function \code{mtext}.
#'
#'@param edgelist basically a two-column matrix with edges (see \code{\link{graph}})
#'@param sorted logical to indicate if nodes should be sorted (default \code{FALSE})
#'@param decreasing logical to indicate type of sorting (used only when \code{sorted=TRUE})
#'@param ordering optional numeric vector providing the ordering of nodes
#'(when provided, this parameter overrides \code{sorted=TRUE})
#'@param horizontal logical indicating whether to plot in horizontal orientation
#'@param col.arcs color for the arcs (default \code{"gray50"})
#'@param lwd.arcs line width for the arcs (default 1)
#'@param lty line type for the arcs (see \code{\link{par}})
#'@param lend the line end style for the arcs (see \code{\link{par}})
#'@param ljoin the line join style for the arcs (see \code{\link{par}})
#'@param lmitre the line mitre limit fort the arcs (see \code{\link{par}})
#'@param show.nodes logical indicating whether to show node symbols
#'@param pch.nodes plotting 'character', i.e. symbol to use when
#'plotting nodes (\code{pch.nodes=0:25})
#'@param cex.nodes expansion of the node symbols (default 1)
#'@param col.nodes color of the node symbols (default \code{"gray50"})
#'@param bg.nodes background (fill) color for the node symbols 
#'given by \code{pch.nodes=21:25}
#'@param lwd.nodes line width for drawing node symbols (see \code{\link{points}})
#'@param show.labels logical indicating whether to show node labels
#'@param labels character vector with labels for the nodes
#'@param col.labels color of the node labels (default \code{"gray50"})
#'@param cex.labels expansion of node labels (default \code{"gray50"})
#'@param las numeric in {0,1,2,3}; the style of axis labels (see \code{\link{par}})
#'@param font font used for node labels (see \code{\link{par}})
#'@param line on which MARgin line the node labels are displayed, 
#'starting at 0 counting outwards (see \code{\link{mtext}})
#'@param outer use outer margins, if available, to plot node labels
#'(see \code{\link{mtext}})
#'@param adj adjustment for each string in reading direction (see \code{\link{mtext}})
#'@param padj adjustment for each string perpendicular to the reading direction
#'(see \code{\link{mtext}})
#'@param axes logical indicating whether to show axes (see \code{\link{par}})
#'@param ... further graphical parameters (see \code{\link{par}}), including
#' \code{family}, \code{xpd}, \code{main}, \code{asp}, etc.
#'@author Gaston Sanchez
#'@seealso \code{\link{xynodes}}
#'@export
#'@examples
#'
#'  \dontrun{
#'  # generate graphs
#'  ring_graph = graph.ring(10)
#'  star_graph = graph.star(10, mode="out")
#'  tree_graph = graph.tree(10, 2)
#'  
#'  # add names to nodes
#'  V(ring_graph)$name = letters[1:vcount(ring_graph)]
#'  V(star_graph)$name = paste("Node", 1:vcount(star_graph))
#'  V(tree_graph)$name = paste("V", 1:vcount(tree_graph), sep='')
#'  
#'  # extract edgelist
#'  ring_edges = get.edgelist(ring_graph)
#'  star_edges = get.edgelist(star_graph)
#'  tree_edges = get.edgelist(tree_graph)
#'
#'  # arc diagram
#'  arcplot(ring_edges, labels=V(ring_graph)$name, las=1)
#'  arcplot(star_edges, labels=V(star_graph)$name, las=2)
#'  arcplot(tree_edges, labels=V(tree_graph)$name, las=2)
#'  
#'  # compare to plot.igraph
#'  plot(ring_graph, vertex.label=V(ring_graph)$name)
#'  plot(star_graph, vertex.label=V(star_graph)$name)
#'  plot(tree_graph, vertex.label=V(tree_graph)$name)
#'  }
#'
arcplot <- function(
  edgelist, sorted = FALSE, decreasing = FALSE, ordering = NULL, horizontal = TRUE,
  col.arcs = "#5998ff77", lwd.arcs = 1.8, lty = 1, lend = 1, ljoin = 2, lmitre = 1,
  show.nodes = FALSE, pch.nodes = 19, cex.nodes = 1, 
  col.nodes = "gray80", bg.nodes = "gray80", lwd.nodes = 1,
  show.labels = TRUE, labels = NULL, col.labels = "gray55",
  cex.labels = 0.9, las = 2, font = 1, line = 0, 
  outer = FALSE, adj = NA, padj = NA,
  axes = FALSE, ...)
{
  # ======================================================
  # Checking arguments
  # ======================================================
  # make sure edgelist is a two-column matrix
  if (!is.matrix(edgelist) || ncol(edgelist) != 2)
    stop("\nSorry, 'edgelist' must be a two column matrix")
  # how many edges
  num_edges = nrow(edgelist)
  # get nodes (this could be numeric or character)
  nodes = unique(as.vector(t(edgelist)))
  # how many nodes
  num_nodes = length(nodes)
  # node numeric sequence
  nums = seq_along(nodes)
  # check labels (i.e. node names)
  if (!is.null(labels))
  {
    if (length(labels) != num_nodes)
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
  # Setting graphical parameters
  # ======================================================
  # color of arcs
  if (length(col.arcs) != num_edges) 
    col.arcs = rep(col.arcs, length=num_edges)
  # line widths of arcs
  if (length(lwd.arcs) != num_edges) 
    lwd.arcs = rep(lwd.arcs, length=num_edges)
  # line type of arcs
  if (length(lty) != num_edges) 
    lty = rep(lty, length=num_edges)

  # pch symbol of nodes
  if (length(pch.nodes) != num_nodes) 
    pch.nodes = rep(pch.nodes, length=num_nodes)
  pch.nodes = pch.nodes[aux_ord]
  # cex of nodes
  if (length(cex.nodes) != num_nodes) 
    cex.nodes = rep(cex.nodes, length=num_nodes)
  cex.nodes = cex.nodes[aux_ord]
  # color of nodes
  if (length(col.nodes) != num_nodes) 
    col.nodes = rep(col.nodes, length=num_nodes)
  col.nodes = col.nodes[aux_ord]
  # bg of nodes
  if (length(bg.nodes) != num_nodes) 
    bg.nodes = rep(bg.nodes, length=num_nodes)
  bg.nodes = bg.nodes[aux_ord]
  # line widths of nodes
  if (length(lwd.nodes) != num_nodes) 
    lwd.nodes = rep(lwd.nodes, length=num_nodes)
  lwd.nodes = lwd.nodes[aux_ord]
    
  # color of labels
  if (length(col.labels) != num_nodes) 
    col.labels = rep(col.labels, length=num_nodes)
  col.labels = col.labels[aux_ord]
  # cex of labels
  if (length(cex.labels) != num_nodes) 
    cex.labels = rep(cex.labels, length=num_nodes)
  cex.labels = cex.labels[aux_ord]
  
  # ======================================================
  # Coordinates of nodes (i.e. vertices)
  # ======================================================
  # node labels at equal distances from each other
  nf = rep(1 / num_nodes, num_nodes)
  # center coordinates of node labels
  fin = cumsum(nf)
  ini = c(0, cumsum(nf)[-num_nodes])
  centers = (ini + fin) / 2
  
  # ======================================================
  # Coordinates of arcs (i.e. edges)
  # ======================================================
  # 'edgelist' in character format or numeric format?
  if (mode(edgelist) == "numeric") tmp = nums else tmp = nodes
  # handy matrix with numeric indices '1:FROM' , '2:TO'
  e_num = matrix(0, nrow(edgelist), 2)
  for (i in 1:num_edges)
  {
    e_num[i,1] = centers[which(tmp == edgelist[i,1])]
    e_num[i,2] = centers[which(tmp == edgelist[i,2])]
  }    
  # maximum arc radius
  radios = abs(e_num[,1] - e_num[,2]) / 2
  max_radios = which(radios == max(radios))
  max_rad = unique(radios[max_radios] / 2)
  # arc locations
  locs = rowSums(e_num) / 2
  
  # ======================================================
  # Plot arc diagram horizontally
  # ======================================================
  if (horizontal)
  {
    # open empty plot window
    plot(0.5, 0.5, xlim=c(-0.015, 1.015), ylim=c(-0.01, 1*max_rad*2),
         type="n", xlab="", ylab="", axes=axes, ...)
    # auxiliar variable for plotting arcs
    z = seq(0, pi, l=100)
    # for each edge
    for (i in 1:num_edges)
    {
      # get radius length
      radio = radios[i]
      # x-y coords of each arc
      x = locs[i] + radio * cos(z)
      y = radio * sin(z)
      # plot arc connecting nodes
      lines(x, y, col=col.arcs[i], lwd=lwd.arcs[i], lty=lty,
            lend=lend, ljoin=ljoin, lmitre=lmitre)
    }
    # add points
    if (show.nodes) {
      points(x=centers, y=rep(0,num_nodes), pch=pch.nodes, 
             col=col.nodes, bg=bg.nodes, cex=cex.nodes, lwd=lwd.nodes)    
    }
    # add node names
    if (show.labels) {
      mtext(labels, side=1, line=line, at=centers, cex=cex.labels, outer=outer,
            col=col.labels, las=las, font=font, adj=adj, padj=padj, ...)    
    }    
  }
  
  # ======================================================
  # Plot arc diagram vertically
  # ======================================================
  if (!horizontal)
  {
    # open empty plot window
    plot(0.5, 0.5, ylim=c(-0.015, 1.015), xlim=c(-0.01, 1*max_rad*2),
         type="n", xlab="", ylab="", axes=axes, ...)
    # auxiliar variable for plotting arcs
    z = seq(0, pi, l=100)
    # for each edge
    for (i in 1:num_edges)
    {
      # get radius length
      radio = radios[i]
      # x-y coords of each arc
      y = locs[i] + radio * cos(z)
      x = radio * sin(z)
      # plot arc connecting nodes
      lines(x, y, col=col.arcs[i], lwd=lwd.arcs[i], lty=lty,
            lend=lend, ljoin=ljoin, lmitre=lmitre)
    }
    # add node symbols with points
    if (show.nodes) {
      points(x=rep(0,num_nodes), y=centers, pch=pch.nodes, 
             col=col.nodes, bg=bg.nodes, cex=cex.nodes, lwd=lwd.nodes)    
    }
    # add node labels with mtext
    if (show.labels) {
      mtext(labels, side=2, line=line, at=centers, cex=cex.labels, outer=outer,
            col=col.labels, las=las, font=font, adj=adj, padj=padj, ...)    
    }
  }

  # --------------------------------
  # brought to you by Gaston Sanchez
  # www.gastonsanchez.com
  # --------------------------------
}
