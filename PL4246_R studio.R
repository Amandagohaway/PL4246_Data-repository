library(igraph)
library(igraphdata)
library(qgraph)
library(influenceR)
library(bootnet)
library(ggplot2)
library(tidyverse)


rawdata = read.csv("Stress CSV.csv", header= TRUE)
rawdata = rawdata[,2:11]


network_rawdata <- estimateNetwork(rawdata, default = "pcor",
                                   threshold = 'sig', sign = "positive")
summary(network_rawdata)
par(mar=c(0,0,0,0)+.1)

plot(network_rawdata,
     label.cex=3)

# convert to a igraph network object 
network_rawdata_g <- graph_from_adjacency_matrix(network_rawdata$graph, 
                                                 mode = 'undirected', 
                                                 weighted = TRUE)

plot(network_rawdata_g,vertex.size=0.25,
     vertex.size=node_size/2, vertex.label.family = 'sans',
     vertex.label.cex=2)
data = igraph::simplify(network_rawdata_g)
is.simple(network_rawdata_g)

get.edge.ids(network_rawdata_g,c("T..","F.R"))
get.edge.ids(network_rawdata_g,c("T..","A.E"))
                  
 Graph_removed_node <- delete.edges(network_rawdata_g,5)
  plot(Graph_removed_node)
    
 Graph_removed_two_nodes <- delete.edges(Graph_removed_node,2)
  plot(Graph_removed_two_nodes)



  # Get node size based on average rating of stressors for each node
  node_ratings <- colMeans(rawdata)
  node_size <- node_ratings / max(node_ratings) * 50
  
  # Plot network with node size corresponding to average rating
  plot(Graph_removed_two_nodes, vertex.size=node_size/2,
       vertex.label.family='sans',
       vertex.label.cex=2,
       vertex.label=V(network_rawdata_g)$name,
       vertex.label.dist=0.5)
  
 ecount(Graph_removed_two_nodes)
  
  #Community detection

  louvain_Graph_removed_two_nodes <- cluster_louvain(Graph_removed_two_nodes)
  
  modularity(louvain_Graph_removed_two_nodes)
  print(modularity)
  
  V(Graph_removed_two_nodes)$community <- louvain_Graph_removed_two_nodes$membership

  louvain_Graph_removed_two_nodes_col <- adjustcolor(c("tomato", "gold", "yellowgreen"), alpha=.6)
  
  plot(Graph_removed_two_nodes, 
       vertex.color=louvain_Graph_removed_two_nodes_col[V(Graph_removed_two_nodes)$community], 
       vertex.label = V(Graph_removed_two_nodes)$name,
       vertex.label.color = 'black',
       vertex.size = 10, 
       vertex.frame.color = 'white',
       layout = layout_with_graphopt, 
       main = "Community Structure")

#LCC
local_clustering <- rbind(V(Graph_removed_two_nodes)$name, 
                          transitivity(Graph_removed_two_nodes, type = 'local', weights = NA) %>% 
                            round(3))
print(local_clustering)

#Degree
degree <- degree(Graph_removed_two_nodes)
print(degree)

#Strength
strength(Graph_removed_two_nodes) %>% round(3)

node_strength <- strength(Graph_removed_two_nodes)
average_node_strength <- mean(node_strength)
print(average_node_strength) %>% round(3)

#KPP
L = layout_with_graphopt(Graph_removed_two_nodes)

plot(Graph_removed_two_nodes, layout=L)

set.seed(1)

KPP = keyplayer(Graph_removed_two_nodes, k=3)

V(Graph_removed_two_nodes)$color = 'lavender'
  
V(Graph_removed_two_nodes)[KPP]$color = 'lightpink' 
  
plot(Graph_removed_two_nodes, layout = L, vertex.size = 10, vertex.color = V(Graph_removed_two_nodes)$color, vertex.label.cex = 0.8)

