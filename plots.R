library(plotly)
library(quantmod)

## A simple example with a couple of actors
## The typical case is that these tables are read in from files....
actors <- c("Alice", "Bob", "Cecil", "David", "Esmeralda")
g <- make_graph(edges=c("Bob", "Cecil", "Cecil", "David","David", "Esmeralda", "Alice", "Bob", "Alice", "Alice", "Bob", "Alice"))
g <- graph_from_data_frame(relations, directed=TRUE)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")

library(plotly)
library(igraph)

data(karate, package="igraphdata")
#g<-read_graph("graph.gml", format = c("gml"))
G <- upgrade_graph(g)
L <- layout.circle(G)

vs <- V(G)
es <- as.data.frame(get.edgelist(G))

Nv <- length(vs)
Ne <- length(es[1]$V1)

Xn <- L[,1]
Yn <- L[,2]

network <- plot_ly(x = ~Xn, y = ~Yn, mode = "markers", text = actors, hoverinfo = "text", type="scatter")

edge_shapes <- list()
for(i in 1:Ne) 
{
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#030303", width = 0.3),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}


axis <- list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)

p <- layout(
  network,
  title = 'Karate Network',
  shapes = edge_shapes,
  xaxis = axis,
  yaxis = axis
)

p

