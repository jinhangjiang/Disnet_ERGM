library(ergm)
library(network)
set.seed(0)
num_nodes <- 5
my_matrix <- matrix(c(0,1,1,0,1,
                      1,0,1,1,1,
                      1,1,0,0,0,
                      0,1,0,0,1,
                      1,1,0,1,0),
                    nrow = num_nodes,
                    ncol = num_nodes)

my_matrix

net <- as.network(x = my_matrix, # the network object
                  directed = FALSE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  bipartite = FALSE,
                  matrix.type = "adjacency" # the type of input
)
network.vertex.names(net) <- c("Alzheimer's disease", "Covid-19", "Parkinson's disease","Pure hypercholesterolemia","Hyperlipidemia") # g309, g20, e7800

Category <- c("Diseases of the nervous system", "Special", "Diseases of the nervous system","Endocrine, nutritional and metabolic diseases","Endocrine, nutritional and metabolic diseases")

net %v% "Category" <- Category

summary.network(net)

plot.network(net, # our network object
             vertex.col = c("lightblue","maroon","lightblue","green","green"), # color nodes by gender
             vertex.cex = c(100,150,100,100,100), # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5, # display the names directly over nodes
             object.scale = 0.001
)

model <- ergm(net~ edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, diff=TRUE),verbose = TRUE)
summary(model)
? nodal_attributes