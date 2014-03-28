#' @title Graph Information
#' 
#' @description Gets graph information
#' 
#' @param edgelist basically a two-column matrix with edges 
#' (see \code{\link{graph}})
#' @param sorted logical to indicate if nodes should be sorted 
#' (default \code{FALSE})
#' @param decreasing logical to indicate type of sorting 
#' (used only when \code{sorted=TRUE})
#' @param ordering optional numeric or string vector providing the 
#' ordering of nodes. When provided, this parameter overrides 
#' \code{sorted=TRUE}). See the details section for more information.
#' @param labels optional string vector with labels for the nodes
#' @export
#' @keywords internal
graph_info <- 
  function(edgelist, vertices, sorted = FALSE, decreasing = FALSE, 
           ordering = NULL, labels = NULL, ... )
  {
    # ======================================================
    # Checking arguments
    # ======================================================
    # edgelist as a two-column matrix
    if (!is.matrix(edgelist) || ncol(edgelist) != 2)
      stop("\nSorry, 'edgelist' must be a two column matrix")
    
    num_edges = nrow(edgelist)
    # get nodes (this could be numeric or character)
    if(hasArg(vertices)){
    #to deal with singleton nodes
    nodes = vertices 
    }else{
    nodes = unique(as.vector(t(edgelist)))	
    }
    num_nodes = length(nodes)
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
      ordered_nodes = order(nodes, decreasing = decreasing)
      nodes = nodes[ordered_nodes]
      labels = labels[ordered_nodes]
      # auxiliar order
      aux_ord = ordered_nodes
    }
    
    # If ordering is provided, re-ennumerate nodes
    if (!is.null(ordering)) 
    {
      if (length(ordering) != num_nodes) {
        stop("\nLength of 'ordering' differs from number of nodes")      
      }
      
      if (is.character(ordering)) {
        # make sure labels contains elements in ordering
        unmatched_ordering <- !(ordering %in% labels)
        if (any(unmatched_ordering)) {
          undetected = ordering[unmatched_ordering]
          stop(sprintf("\nUnrecognized values in ordering: '%s'", undetected))
        }
        ordering = match(ordering, labels)
      }
      
      nodes = nodes[ordering]
      labels = labels[ordering]
      # auxiliar order
      aux_ord = ordering
    }
    
    ## output
    list(
      nodes = nodes,
      labels = labels,
      num_nodes = num_nodes,
      num_edges = num_edges,
      aux_ord = aux_ord
    )
  }


#' @title X or Y coordinates of node locations
#' 
#' @description
#' Gives axis locations of each node
#'  
#' @param num_nodes number of nodes
#' @param aux_ord vector with the index number for ordering the nodes
#' @param labels optional string vector with labels for the nodes
#' @export
xynodes <- function(num_nodes, aux_ord, labels)
{
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
  
  # output
  centers
}


#' @title Arc Radius Locations
#' 
#' @description Computes the location and radius of each arc
#' 
#' @param edgelist 2-column matrix
#' @param nodes vector of nodes
#' @param centers vector with xy-positions of nodes
#' @return a list with locations and radios
#' @return \item{locs}{locations}
#' @return \item{radios}{radius values}
#' @export
#' @keywords internal
arc_radius_locs <- function(edgelist, nodes, centers)
{
  # ======================================================
  # Coordinates of arcs (i.e. edges)
  # ======================================================
  # handy matrix with numeric indices '1:FROM' , '2:TO'
  edges_from_to = matrix(0, nrow(edgelist), 2)
  for (i in 1L:nrow(edgelist))
  {
    edges_from_to[i,1] = centers[which(nodes == edgelist[i,1])]
    edges_from_to[i,2] = centers[which(nodes == edgelist[i,2])]
  }
  
  # maximum radius of arcs 
  radios = abs(edges_from_to[,1] - edges_from_to[,2]) / 2
  max_radios = which(radios == max(radios))
  max_rad = unique(radios[max_radios] / 2)    
  
  # arc locations
  locs = rowSums(edges_from_to) / 2
  
  # output
  list(locs = locs, radios = radios)
}


#' @title Above or Below
#' 
#' @description Determines how arcs should be displayed
#' @details 
#' If \code{horizontal = TRUE} then arcs can be plotted above or 
#' below the horizontal axis \cr
#' If \code{horizontal = FALSE} then arcs can be plotted to the right or 
#' left of the vertical axis
#' @param edgelist two-column matrix
#' @param above optional numeric or logical vector indicating what edges 
#' (arcs) should be plotted above (or to the right of) of chosen axis
#' If \code{above = NULL} then all arcs are plotted above (or to the right) 
#' If \code{above} is numeric, it cannot contain both positive and negative
#' indices.
#' If \code{above} is logical, its length must equal the number of rows in
#' \code{edgelist}
#' @return a logical vector indicating how arcs should be displayed
#' @export
#' @keywords internal
above_below <- function(edgelist, above)
{
  # ======================================================
  # Coordinates of arcs (i.e. edges) below the axis
  # ======================================================
  # check above
  if (is.null(above)) {
    above = rep(TRUE, nrow(edgelist))     
  } else {
    if (length(above) > nrow(edgelist))
      stop("\nlength of 'above' exceeds number of rows in 'edgelist'")
    # check numeric above and convert to logical
    if (is.numeric(above)) {
      above_positive <- any(above > 0)
      above_negative <- any(above < 0)
      if (above_positive & above_negative)
        stop("\n'above' cannot contain both negative and positive indices")
      # convert to logical
      if (all(above > 0)) {
        above = 1:nrow(edgelist) %in% above
      }
      if (all(above < 0)) {
        above <- !(-(1:nrow(edgelist)) %in% above)
      }
      if (all(above == 0)) {
        above = rep(FALSE, nrow(edgelist))          
      }
    }
    # check logical above
    if (is.logical(above)) {
      if (length(above) != nrow(edgelist))
        stop("\nlength of 'above' must equal number of rows in 'edgelist'")
    }
  }
  
  # output
  above
}


#' @title Minimum and Maximum Margin Limits
#' @description Computes the minimum and maximum margin limits of 
#' plotting region
#' @param radios vector of arc radius
#' @param above logical vectors indicating whether arcs should be displayed
#' @return list with minimum and maximum margin limits
#' @export 
#' @keywords internal
min_max_margin <- function(radios, above)
{
  # determine maximum radius
  max_radios = which(radios == max(radios))
  
  # minimum and maximum margin limits
  lim_min = 0
  lim_max = 0
  
  above_radios = radios[above]
  if (length(above_radios > 0)) {
    max_above_radios = which(above_radios == max(above_radios))[1]
    lim_max = above_radios[max_above_radios]
  }
  
  below_radios = radios[!above]
  if (length(below_radios > 0)) {
    max_below_radios = which(below_radios == max(below_radios))[1]
    lim_min = -1 * below_radios[max_below_radios]
  }
  
  # margin limits
  list(min = lim_min, max = lim_max)
}




#' @title Arc Diagram Plot
#' 
#' @description
#' Give me an edgelist and I'll help you plot a pretty damn arc diagram
#' 
#' @details
#' The arcs are scaled such that they fit in a plot region with its
#' x-axis ranging from zero to one. Node symbols and labels can be
#' optionally displayed. Node symbols are displayed through
#' the function \code{points}. In turn, node labels are displayed
#' through the function \code{mtext}.
#' 
#' When \code{ordering} is provided in numeric format and node labels are 
#' strings, the labels are alphabetically ordered first, and then nodes are 
#' sorted according to the provided \code{ordering}.
#' 
#' If \code{ordering} is provided in string format, the node labels must be 
#' strings as well. The nodes will be sorted according to \code{ordering}.
#' 
#' @param edgelist basically a two-column matrix with edges 
#' (see \code{\link{graph}})
#' @param sorted logical to indicate if nodes should be sorted 
#' (default \code{FALSE})
#' @param decreasing logical to indicate type of sorting 
#' (used only when \code{sorted=TRUE})
#' @param ordering optional numeric or string vector providing the 
#' ordering of nodes. When provided, this parameter overrides 
#' \code{sorted=TRUE}). See the details section for more information.
#' @param labels optional string vector with labels for the nodes
#' @param horizontal logical indicating whether to plot 
#' in horizontal orientation
#' @param above optional vector indicating which arcs should be displayed
#' above (or to the right) and below (or to the left) of the axis
#' @param col.arcs color for the arcs (default \code{"gray50"})
#' @param lwd.arcs line width for the arcs (default 1)
#' @param lty.arcs line type for the arcs (see \code{\link{par}})
#' @param lend the line end style for the arcs (see \code{\link{par}})
#' @param ljoin the line join style for the arcs (see \code{\link{par}})
#' @param lmitre the line mitre limit for the arcs (see \code{\link{par}})
#' @param show.nodes logical indicating whether to show node symbols
#' @param pch.nodes plotting 'character', i.e. symbol to use when
#' plotting nodes (\code{pch.nodes=0:25})
#' @param cex.nodes expansion of the node symbols (default 1)
#' @param col.nodes color of the node symbols (default \code{"gray50"})
#' @param bg.nodes background (fill) color for the node symbols 
#' given by \code{pch.nodes=21:25}
#' @param lwd.nodes line width for drawing node symbols 
#' (see \code{\link{points}})
#' @param show.labels logical indicating whether to show node labels
#' @param col.labels color of the node labels (default \code{"gray50"})
#' @param cex.labels expansion of node labels (default \code{"gray50"})
#' @param las numeric in {0,1,2,3}; the style of axis labels 
#' (see \code{\link{par}})
#' @param font font used for node labels (see \code{\link{par}})
#' @param line on which margin line the node labels are displayed, 
#' starting at 0 counting outwards (see \code{\link{mtext}})
#' @param outer use outer margins, if available, to plot node labels
#' (see \code{\link{mtext}})
#' @param adj adjustment for each string in reading direction 
#' (see \code{\link{mtext}})
#' @param padj adjustment for each string perpendicular to 
#' the reading direction (see \code{\link{mtext}})
#' @param axes logical indicating whether to plot the axes 
#' (default \code{FALSE})
#' @param ... further graphical parameters (see \code{\link{par}}), including
#' \code{family}, \code{xpd}, \code{main}, \code{asp}, etc.
#' @author Gaston Sanchez
#' @seealso \code{\link{xynodes}}
#' @export
#' @examples
#'
#'  \dontrun{
#'  # create an edgelist
#'  un_graphe <- rbind(
#'  c("fromage", "pain"), 
#'  c("pain", "vin"),
#'  c("vin", "biere"),
#'  c("cidre", "biere"),
#'  c("foie", "fromage"),
#'  c("pain", "foie"))
#'  
#'  # deafult arcplot
#'  arcplot(un_graphe)
#'  # vertical display
#'  arcplot(un_graphe, horizontal=FALSE)
#'  # arcplot with arcs above and below axis
#'  arcplot(un_graphe, above = c(1, 3, 5))
#'  # nodes sorted alphabetically (increasing)
#'  arcplot(un_graphe, sorted=TRUE)
#'  # nodes sorted alphabetically (decreasing)
#'  arcplot(un_graphe, sorted=TRUE, decreasing = TRUE)
#'  # provided order for nodes
#'  new_order = c("vin", "biere", "cidre", "fromage", "foie", "pain")
#'  arcplot(un_graphe, ordering = new_order)
#'  
#'  
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
  edgelist, vertices, sorted = FALSE, decreasing = FALSE, ordering = NULL, main,
  labels = NULL, horizontal = TRUE, above = NULL, 
  col.arcs = "#5998ff77", lwd.arcs = 1.8, lty.arcs = 1, 
  lend = 1, ljoin = 2, lmitre = 1, show.nodes = TRUE, pch.nodes = 19, 
  cex.nodes = 1, col.nodes = "gray80", bg.nodes = "gray80", lwd.nodes = 1,
  show.labels = TRUE, col.labels = "gray55",
  cex.labels = 0.9, las = 2, font = 1, line = 0, 
  outer = FALSE, adj = NA, padj = NA, axes = FALSE, ...)
{
  # Get graph information
  if(hasArg(vertices)) { 
  nodes_edges = graph_info(edgelist, vertices = vertices, sorted = sorted, decreasing = decreasing, 
                           ordering = ordering, labels = labels)
  }else{
  nodes_edges = graph_info(edgelist, sorted = sorted, decreasing = decreasing, 
                           ordering = ordering, labels = labels)
  }
  
  nodes = nodes_edges$nodes
  num_nodes = nodes_edges$num_nodes
  num_edges = nodes_edges$num_edges
  aux_ord = nodes_edges$aux_ord
  labels = nodes_edges$labels
  
  # x-y node coordinates
  centers = xynodes(num_nodes, aux_ord, labels)
  
  # determine above or below display of arcs
  above = above_below(edgelist, above)
  
  # arc radius and locations
  radios_locs = arc_radius_locs(edgelist, nodes, centers)
  radios = radios_locs$radios
  locs = radios_locs$locs
  
  # ======================================================
  # Graphical parameters for Arcs
  # ======================================================
  # color of arcs
  if (length(col.arcs) != num_edges) 
    col.arcs = rep(col.arcs, length=num_edges)
  # line width of arcs
  if (length(lwd.arcs) != num_edges) 
    lwd.arcs = rep(lwd.arcs, length=num_edges)
  # line type of arcs
  if (length(lty.arcs) != num_edges) 
    lty.arcs = rep(lty.arcs, length=num_edges)
  
  # ======================================================
  # Graphical parameters for Nodes
  # ======================================================
  # pch symbol of nodes
  if (length(pch.nodes) != num_nodes) {
    pch.nodes = rep(pch.nodes, length = num_nodes)    
  }
  pch.nodes = pch.nodes[aux_ord]
  # cex of nodes
  if (length(cex.nodes) != num_nodes) {
    cex.nodes = rep(cex.nodes, length = num_nodes)    
  }
  cex.nodes = cex.nodes[aux_ord]
  # color of nodes
  if (length(col.nodes) != num_nodes) {
    col.nodes = rep(col.nodes, length = num_nodes)    
  }
  col.nodes = col.nodes[aux_ord]
  # bg of nodes
  if (length(bg.nodes) != num_nodes) {
    bg.nodes = rep(bg.nodes, length = num_nodes)    
  }
  bg.nodes = bg.nodes[aux_ord]
  # line widths of nodes
  if (length(lwd.nodes) != num_nodes) {
    lwd.nodes = rep(lwd.nodes, length = num_nodes)    
  }
  lwd.nodes = lwd.nodes[aux_ord]
  
  # ======================================================
  # Graphical parameters for Node Labels
  # ======================================================
  # color of labels
  if (length(col.labels) != num_nodes) {
    col.labels = rep(col.labels, length = num_nodes)    
  } 
  col.labels = col.labels[aux_ord]
  # cex of labels
  if (length(cex.labels) != num_nodes) {
    cex.labels = rep(cex.labels, length = num_nodes)    
  }
  cex.labels = cex.labels[aux_ord]
  
  # ======================================================
  # Plot arc diagram (horizontally or vertically)
  # ======================================================
  # auxiliar vector for plotting arcs
  z = seq(0, pi, length.out = 100)
  
  if (horizontal) {
    side = 1
    xlim = c(-0.015, 1.015)
    ylims = min_max_margin(radios, above)
    ylim = c(ylims$min, ylims$max)
    x_nodes = centers
    y_nodes = rep(0, num_nodes)    
  } else {
    side = 2
    ylim = c(-0.015, 1.015)
    xlims = min_max_margin(radios, above)
    xlim = c(xlims$min, xlims$max)
    x_nodes = rep(0, num_nodes)    
    y_nodes = centers
  }
  
  
  # open empty plot window
  plot(0.5, 0.5, xlim = xlim, ylim = ylim, type = "n", main = main,
       xlab = "", ylab = "", axes = axes, ...)
  # add each edge
  for (i in 1L:num_edges)
  {
    # get radius length
    radio = radios[i]
    if (horizontal) {
      # x-y coords of each arc
      x_arc = locs[i] + radio * cos(z)
      if (above[i]) { # above axis
        y_arc = radio * sin(z)
      } else {  # below axis
        y_arc = radio * sin(-z)
      }
    } else {
      # x-y coords of each arc
      y_arc = locs[i] + radio * cos(z)
      if (above[i]) { # above axis
        x_arc = radio * sin(z)
      } else {  # below axis
        x_arc = radio * sin(-z)
      }      
    }
    
    # plot arc connecting nodes
    lines(x_arc, y_arc, col=col.arcs[i], lwd=lwd.arcs[i], lty=lty.arcs[i],
          lend=lend, ljoin=ljoin, lmitre=lmitre)
    # add node symbols with points
    if (show.nodes) {
      points(x=x_nodes, y=y_nodes, pch=pch.nodes, 
             col=col.nodes, bg=bg.nodes, cex=cex.nodes, lwd=lwd.nodes)    
    }
    # add node labels with mtext
    if (show.labels) {
      mtext(labels, side=side, line=line, at=centers, cex=cex.labels, outer=outer,
            col=col.labels, las=las, font=font, adj=adj, padj=padj, ...)    
    }
  }
}


#' @title Node Coordinates
#' 
#' @description
#' Computes axis locations of each node. This function can be helpful when 
#' you want to separately plot the node labels using the function mtext.
#'
#' @param edgelist basically a two-column matrix with edges 
#' (see \code{\link{graph}})
#' @param sorted logical to indicate if nodes should be sorted
#' @param decreasing logical to indicate type of sorting 
#' (used only when \code{sorted=TRUE})
#' @param ordering optional numeric vector providing the ordering of nodes
#' (when provided, this parameter overrides \code{sorted=TRUE})
#' @param labels character vector with labels for the nodes
#' @return a vector with the location of nodes in the x-axis
#' @author Gaston Sanchez
#' @seealso \code{\link{arcplot}}
#' @export
#' @examples
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
node_coords <- function(
  edgelist, sorted = FALSE, decreasing = FALSE, ordering = NULL, 
  labels = NULL)
{
  # Get graph information
  nodes_edges = graph_info(edgelist, sorted = sorted, decreasing = decreasing, 
                           ordering = ordering, labels = labels)
  
  nodes = nodes_edges$nodes
  num_nodes = nodes_edges$num_nodes
  num_edges = nodes_edges$num_edges
  aux_ord = nodes_edges$aux_ord
  labels = nodes_edges$labels
  
  # x-y node coordinates
  centers = xynodes(num_nodes, aux_ord, labels)
}
