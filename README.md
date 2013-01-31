arcdiagram
============================

The R package **arcdiagram** implements a basic function to plot pretty arc diagrams in R

## Installation

Install the prototype version on [github](https://github.com/gastonstat/arcdiagram) using the R package **devtools**:
```
# if you haven't installed 'devtools'
install.packages("devtools") 

# load devtools
library(devtools)

# install 'arcdiagram'
install_github('arcdiagram',  username='gastonstat')
```

## Example Usage
```
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

More info at [www.gastonsanchez.com/arcdiagram](http://www.gastonsanchez.com/arcdiagram)

Links
-----
[arcdiagram package github](http://github.com/gastonstat/arcdiagram)

[arcdiagram documentation](http://www.gastonsanchez.com/arcdiagram)


Author Contact
--------------
Gaston Sanchez (gaston.stat at gmail.com)
