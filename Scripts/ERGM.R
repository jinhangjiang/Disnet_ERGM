library(ergm)
library(tidyverse)
library(network)

#packageVersion("ergm")
set.seed(0)

#?network

####### generate g1 ###########

icd10.g1 <- icd101[c("Id","Category","Prevalence_20192")]
edge.g1 <- edge1[c("Source","Target","Weight20192")]



start.time <- Sys.time()

g1 <- network(edge.g1[c("Source","Target")],directed = FALSE, vertices=icd10.g1)
set.edge.attribute(g1,"weight",edge.g1$Weight20192)
summary(g1)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


plot(g1)


####### generate g2 ###########
icd10.g2 <- icd102[c("Id","Category","Prevalence_20201")]
edge.g2 <- edge2[c("Source","Target","Weight20201")]

g2 <- network(edge.g2[c("Source","Target")],directed = FALSE, vertices=icd10.g2)
set.edge.attribute(g2,"weight",edge.g2$Weight20201)

#vertex_attr_names(g2)


####### generate g3 ###########
icd10.g3 <- icd103[c("Id","Category","Prevalence_20202")]
edge.g3 <- edge3[c("Source","Target","Weight20202")]

g3 <- network(edge.g3[c("Source","Target")],directed = FALSE, vertices=icd10.g3)
set.edge.attribute(g3,"weight",edge.g3$Weight20202)


####### estimate  minimal model ###########
summary(g1 ~ edges+triangle)
summary(g2 ~ edges+triangle)
summary(g3 ~ edges+triangle)

#### Minimal 01 ####
minimal.01 <- ergm(g1 ~ edges)
summary(minimal.01)

####### estimate  g1 model ###########

#### Model 01 ####
start.time <- Sys.time()
model.g1 <- ergm(g1 ~ edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, levels=NULL, diff=TRUE))
sum01<-summary(model.g1)
sum01
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### estimate  g2 model ###########

#### Model 02 ####
start.time <- Sys.time()
model.g2 <- ergm(g2 ~ edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, levels=NULL, diff=TRUE))
sum02<-summary(model.g2)
sum02
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### estimate  g3 model ###########

#### Model 03 ####
start.time <- Sys.time()
model.g3 <- ergm(g3 ~ edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, levels=NULL, diff=TRUE))
sum03<-summary(model.g3)
sum03
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



####### estimate ego network model ###########

icd10.ego <- icd104[c("Id","Category","Prevalence_20202")]
edge.ego <- edge4[c("source","target","weight")]

ego <- network(edge.ego[c("source","target")],directed = FALSE, vertices=icd10.ego)
set.edge.attribute(ego,"weight",edge.ego$weight)

#### draw ego ####
vertexcolor <- c(rep("lightblue",2),
                 rep("blue",4),
                 rep("seagreen1",3),
                 rep("orange",5),
                 rep("gold2",3),
                 rep("gray66",1),
                 rep("hotpink1",5),
                 rep("khaki3",1),
                 "maroon",
                 rep("thistle",8))

vertexcex <- c(rep(60,24),90)
names(ego)
plot.network(ego, # our network object
             vertex.col = vertexcolor, # color nodes by gender
             vertex.cex = vertexcex, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5, # display the names directly over nodes
             object.scale = 0.00085
)

#### Model ego ####
summary(ego)
start.time <- Sys.time()
model.ego <- ergm(ego ~ edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, levels=NULL, diff=TRUE))
sumego<-summary(model.ego)
sumego
qend.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



######################################### STERGM #########################################################
tnetworks<-list()
tnetworks[[1]]<-g1
tnetworks[[2]]<-g2
tnetworks[[3]]<-g3

library(statnet)
?stergm
model.tergm <- stergm(tnetworks,
                      formation = ~edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, levels=NULL, diff=TRUE),
                      dissolution = ~edges+nodefactor(~Category,levels=NULL)+nodematch(~Category, levels=NULL, diff=TRUE),
                      estimate = "CMLE",
                      times = 1:3)
