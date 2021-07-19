library(ergm)
library(tidyverse)
library(igraph)
packageVersion("ergm")
set.seed(0)

icd10.g1 <- icd10[c("Id","Category","Prevalence_20192")]
icd10.g1 <- na.omit(icd10.g1)
edge.g1 <- Edgelist_all[c("Source","Target","Weight20192")]
edge.g1 <- na.omit(edge.g1)

g1<-graph_from_data_frame(edge.g1[c("Source","Target")],directed = FALSE,vertices = icd10.g1)
set.edge.value(g1,"weight",index=E(g1),value = )

vcount(g1)
ecount(g1)

get.adjacency(g1)
plot(g1)

E(g1)
edge.g1$Weight20192
