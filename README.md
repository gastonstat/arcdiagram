# `"arcdiagram"`

`arcdiagram` is a minimalist package that provides a basic function to plot 
pretty arc diagrams in R

## Installation

`arcdiagram` is only available in github (but not in CRAN). To install the 
development version, use `install_github()` from R package `"devtools"`:

```ruby
# install 'arcdiagram' (development version)
devtools::install_github('gastonstat/arcdiagram')
```

## Some Examples

```ruby
library(arcdiagram)

# create a star graph with 10 nodes
star_graph = graph.star(10, mode="out")

# extract edgelist
star_edges = get.edgelist(star_graph)

# inspect star_edges
star_edges

# plot 1: default arc diagram
arcplot(star_edges)

# plot 2: show nodes as circles, in decreasing order
arcplot(star_edges, show.nodes=TRUE, sorted=TRUE, decreasing=TRUE, las=1)

# plot 3: different ordering, arc widths, arc colors, and node sizes
set.seed(120)
arcplot(star_edges, ordering=sample(1:10), labels=paste("node",1:10,sep="-"),
   lwd.arcs=4*runif(10,.5,2), col.arcs=hsv(runif(9,0.6,0.8),alpha=0.4),
   show.nodes=TRUE, pch.nodes=21, cex.nodes=runif(10,1,3), 
   col.nodes="gray80", bg.nodes="gray90", lwd.nodes=2)
   
# plot 4: same as plot 3 but vertically oriented
set.seed(120)
op = par(mar = c(0.5, 5, 0.5, 3))
arcplot(star_edges, ordering=sample(1:10), horizontal=FALSE,
   labels=paste("node",1:10,sep="-"),
   lwd.arcs=4*runif(10,.5,2), col.arcs=hsv(runif(9,0.6,0.8),alpha=0.4),
   show.nodes=TRUE, pch.nodes=21, cex.nodes=runif(10,1,3), 
   col.nodes="gray80", bg.nodes="gray90", lwd.nodes=2)
par(op)
```
