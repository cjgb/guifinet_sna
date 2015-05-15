library(RJSONIO)
library(igraph)

bcn <- fromJSON("guifi-d3-barcelones.json")

nodos <- do.call(rbind, lapply(bcn$nodes, as.data.frame))
links <- do.call(rbind, lapply(bcn$links, as.data.frame))
links <- links[,c("source", "target", "id", "KMS", "STATUS", "type", "NODE1_NAME", "NODE2_NAME", "coordinates")]

links <- links[links$source %in% nodos$id,]
links <- links[links$target %in% nodos$id,]

g <- graph.data.frame(links, directed = F, vertices = nodos)

g.working <- subgraph(g, V(g)$STATUS %in% c("Working"))

my_layout <- layout.fruchterman.reingold(g.working)
plot(g.working, layout = my_layout, vertex.label = NA, vertex.size = 0.3)

# mayor componente conexa

kk <- clusters(g.working)

g.wc <- subgraph(g.working, kk$membership == 1)

my_layout <- layout.fruchterman.reingold(g.wc)
plot(g.wc, layout = my_layout, vertex.label = NA, vertex.size = 0.3)

g.wc <- set.edge.attribute(g.wc, name = "betweenness", E(g.wc), edge.betweenness(g.wc))
g.wc <- set.vertex.attribute(g.wc, name = "btw", V(g.wc), betweenness(g.wc))

my_layout <- layout.fruchterman.reingold(g.wc)
plot(g.wc, layout = my_layout, vertex.label = NA, vertex.size = (1 + V(g.wc)$btw) / 3000)

geom.layout <- do.call(rbind, strsplit(V(g.wc)$coordinates, ","))
geom.layout <- apply(geom_layout, 2, as.numeric)

plot(g.wc, layout = geom.layout, vertex.label = NA, vertex.size = (1 + V(g.wc)$btw) / 3000)




library(XML)
library(plyr)
library(igraph)


tmp <- readLines("https://guifi.net/en/guifi/cnml/2435/detail")
tmp <- xmlParse(tmp)

nodos <- xpathApply(tmp, "//*/node")

lista.nodos <- rbind.fill(lapply(nodos, function(x) data.frame(t(xmlAttrs(x)), 
                                                               stringsAsFactors = FALSE)))

lista.links <- lapply(nodos, function(x){
  from <- xmlAttrs(x)["id"]
  to   <- xpathApply(x, ".//link", xmlAttrs)
  
  if (length(to) == 0)
    return(NULL)
  
  to <- sapply(to, function(x) x["linked_node_id"])
  
  data.frame(from = from, to = to, stringsAsFactors = FALSE)
  
})

lista.links <- do.call(rbind, lista.links)

lista.links <- lista.links[lista.links$from %in% lista.nodos$id,]
lista.links <- lista.links[lista.links$to   %in% lista.nodos$id,]

lista.links <- lista.links[lista.links$to != lista.links$from,]



g <- graph.data.frame(lista.links, directed = F, lista.nodos)

g.working <- subgraph(g, V(g)$status %in% c("Working"))

my_layout <- layout.fruchterman.reingold(g.working)
plot(g.working, layout = my_layout, vertex.label = NA, vertex.size = 0.3)

# mayor componente conexa

kk <- clusters(g.working)

g.wc <- subgraph(g.working, kk$membership == 1)

my_layout <- layout.fruchterman.reingold(g.wc)
plot(g.wc, layout = my_layout, vertex.label = NA, vertex.size = 0.3)

g.wc <- set.edge.attribute(g.wc, name = "betweenness", E(g.wc), edge.betweenness(g.wc))
g.wc <- set.vertex.attribute(g.wc, name = "btw", V(g.wc), betweenness(g.wc))

my_layout <- layout.fruchterman.reingold(g.wc)
plot(g.wc, layout = my_layout, vertex.label = NA, vertex.size = (1 + V(g.wc)$btw) / 3000)

#geom.layout <- do.call(rbind, strsplit(V(g.wc)$coordinates, ","))
geom.layout <- cbind(as.numeric(V(g.wc)$lat),
                     as.numeric(V(g.wc)$lon))

plot(g.wc, layout = geom.layout, vertex.label = NA, vertex.size = (1 + V(g.wc)$btw) / 3000)

