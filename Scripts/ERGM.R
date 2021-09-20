library(ergm)
library(network)

set.seed(0)


edge1 <- read.csv("edge1.csv")
edge2 <- read.csv("edge2.csv")
edge3 <- read.csv("edge3.csv")

icd101 <- read.csv("icd101.csv")
icd102 <- read.csv("icd102.csv")
icd103 <- read.csv("icd103.csv")


####### generate g1 ###########

icd10.g1 <- icd101[c("Id","Category","Prevalence_20192")]
edge.g1 <- edge1[c("Source","Target","Weight20192")]

start.time <- Sys.time()
g1 <- network(edge.g1[c("Source","Target")],directed = FALSE, vertices=icd10.g1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g2 ###########

icd10.g2 <- icd102[c("Id","Category","Prevalence_20201")]
edge.g2 <- edge2[c("Source","Target","Weight20201")]

start.time <- Sys.time()
g2 <- network(edge.g2[c("Source","Target")],directed = FALSE, vertices=icd10.g2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g3 ###########

icd10.g3 <- icd103[c("Id","Category","Prevalence_20202")]
edge.g3 <- edge3[c("Source","Target","Weight20202")]

start.time <- Sys.time()
g3 <- network(edge.g3[c("Source","Target")],directed = FALSE, vertices=icd10.g3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


###### unweighted model1 with selected categories ######
start.time <- Sys.time()
model01 <- ergm(g1 ~ edges+nodecov(~Prevalence_20192)
                +nodefactor("Category", levels=c("Other health services","Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model01)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model2 with selected categories ######
start.time <- Sys.time()
model02 <- ergm(g2 ~ edges+nodecov(~Prevalence_20201)
                +nodefactor("Category", levels=c("Other health services","Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model02)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
###### unweighted model3 with selected categories ######
start.time <- Sys.time()
model03 <- ergm(g3 ~ edges+nodecov(~Prevalence_20202)
                +nodefactor("Category", levels=c("Other health services","Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model03)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### Goodness and diagnostics of model1 ######
start.time <- Sys.time()
gof.model1 <-gof(model01)
par(mfrow=c(2,2))
plot(gof.model1)
par(mfrow=c(2,2))
mcmc.diagnostics(model01)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### Goodness and diagnostics of model2 ######
start.time <- Sys.time()
gof.model2 <-gof(model02)
par(mfrow=c(2,2))
plot(gof.model2)
par(mfrow=c(2,2))
mcmc.diagnostics(model02)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### Goodness and diagnostics of model3 ######
start.time <- Sys.time()
gof.model1 <-gof(model03)
par(mfrow=c(2,2))
plot(gof.model1)
par(mfrow=c(2,2))
mcmc.diagnostics(model03)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken



