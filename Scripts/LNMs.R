#### Load Package #####
library(eigenmodel)
library(igraph)

#### Load data ####
set.seed(42)
edge1 <- read.csv("edge1.csv")
edge2 <- read.csv("edge2.csv")
edge3 <- read.csv("edge3.csv")

icd101 <- read.csv("icd101.csv")
icd102 <- read.csv("icd102.csv")
icd103 <- read.csv("icd103.csv")

#### Convert Graph ####

start.time <- Sys.time()
g1 <- graph_from_edgelist(as.matrix(edge1[,1:2]), directed = FALSE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
g2 <- graph_from_edgelist(as.matrix(edge2[,1:2]), directed = FALSE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
g3 <- graph_from_edgelist(as.matrix(edge3[,1:2]), directed = FALSE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

#### Add attributes ####
###### re-index the icd10 files
icd101<-icd101[match(V(g1)$name,icd101[,1]),]
icd102<-icd102[match(V(g2)$name,icd102[,1]),]
icd103<-icd103[match(V(g3)$name,icd103[,1]),]

###### add prevalence
V(g1)$Prevalence<-icd101[,5]
V(g2)$Prevalence<-icd102[,6]
V(g3)$Prevalence<-icd103[,7]

##### add categories
V(g1)$Category<-icd101[,4]
V(g2)$Category<-icd102[,4]
V(g3)$Category<-icd103[,4]

##### add color
V(g1)$Color<-icd101$Color
V(g2)$Color<-icd102$Color
V(g3)$Color<-icd103$Color

##### add alphabet
V(g1)$alphabet<-icd101$alphabet
V(g2)$alphabet<-icd102$alphabet
V(g3)$alphabet<-icd103$alphabet

#### Fit LNMs ####
#### convert matrix
A <- get.adjacency(g1, sparse = FALSE)
B <- get.adjacency(g2, sparse = FALSE)
C <- get.adjacency(g3, sparse = FALSE)
#### base and featured model1
start.time <- Sys.time()
model1.1 <- eigenmodel_mcmc(A, R = 2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

model1.1.lo <- eigen(model1.1$ULU_postmean)$vec[,1:2]
v1.colors <- V(g1)$Color
v1.size <- log(V(g1)$Prevalence)/1.3
#select.label <- c("e","f","i","j","z"), vertex.label=ifelse(V(g1)$alphabet==select.label, V(g1)$alphabet, NA)
par(mfrow=c(1,2))
plot(simplify(simplify(g1)), vertex.color=v1.colors,vertex.size = v1.size, vertex.label=NA)
plot(g1, layout=model1.1.lo, vertex.color=v1.colors,vertex.size = v1.size, vertex.label=NA,margin=-.1)

apply(model1.1$L_postsamp, 2, mean)

#### base and featured model2
start.time <- Sys.time()
model2.1 <- eigenmodel_mcmc(B, R = 2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

model2.1.lo <- eigen(model2.1$ULU_postmean)$vec[,1:2]
v2.colors <- V(g2)$Color
v2.size <- log(V(g2)$Prevalence)/1.3
#select.label <- c("e","f","i","j","z"), vertex.label=ifelse(V(g1)$alphabet==select.label, V(g1)$alphabet, NA)
par(mfrow=c(1,2))
plot(simplify(simplify(g2)), vertex.color=v2.colors,vertex.size = v2.size, vertex.label=NA)
plot(g2, layout=model2.1.lo, vertex.color=v2.colors,vertex.size = v2.size, vertex.label=NA,margin=-.1)

apply(model2.1$L_postsamp, 2, mean)

#### base and featured model3
start.time <- Sys.time()
model3.1 <- eigenmodel_mcmc(C, R = 2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

model3.1.lo <- eigen(model3.1$ULU_postmean)$vec[,1:2]
v3.colors <- V(g3)$Color
v3.size <- log(V(g3)$Prevalence)/1.3
#select.label <- c("e","f","i","j","z"), vertex.label=ifelse(V(g1)$alphabet==select.label, V(g1)$alphabet, NA)
par(mfrow=c(1,2))
plot(simplify(simplify(g3)), vertex.color=v3.colors,vertex.size = v3.size, vertex.label=NA)
plot(g3, layout=model3.1.lo, vertex.color=v3.colors,vertex.size = v3.size, vertex.label=NA,margin=-.1)

apply(model3.1$L_postsamp, 2, mean)


######### compare three plots
par(mfrow=c(1,3))
plot(g1, layout=model1.1.lo, vertex.color=v1.colors,vertex.size = v1.size, vertex.label=NA,margin=-.1)
plot(g2, layout=model2.1.lo, vertex.color=v2.colors,vertex.size = v2.size, vertex.label=NA,margin=-.1)
plot(g3, layout=model3.1.lo, vertex.color=v3.colors,vertex.size = v3.size, vertex.label=NA,margin=-.1)

