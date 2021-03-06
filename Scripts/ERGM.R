library(ergm)
library(network)

set.seed(0)


edge1 <- read.csv("Edge20181.csv")
edge2 <- read.csv("Edge20182.csv")
edge3 <- read.csv("Edge20191.csv")
edge4 <- read.csv("Edge20192.csv")
edge5 <- read.csv("Edge20201.csv")
edge6 <- read.csv("Edge20202.csv")
edge7 <- read.csv("Edge20211.csv")

icd101 <- read.csv("icd181.csv")
icd102 <- read.csv("icd182.csv")
icd103 <- read.csv("icd191.csv")
icd104 <- read.csv("icd192.csv")
icd105 <- read.csv("icd201.csv")
icd106 <- read.csv("icd202.csv")
icd107 <- read.csv("icd211.csv")


####### generate g1 ###########
icd10.g1 <- icd101[c("Id","Category","Prevalence_20181","Cost_support_20181","Chronic.Indicator")]
edge.g1 <- edge1[c("Source","Target","Weight")]

start.time <- Sys.time()
g1 <- network(edge.g1[c("Source","Target")],directed = FALSE, vertices=icd10.g1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g2 ###########
icd10.g2 <- icd102[c("Id","Category","Prevalence_20182","Cost_support_20182","Chronic.Indicator")]
edge.g2 <- edge2[c("Source","Target","Weight")]

start.time <- Sys.time()
g2 <- network(edge.g2[c("Source","Target")],directed = FALSE, vertices=icd10.g2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g3 ###########
icd10.g3 <- icd103[c("Id","Category","Prevalence_20191","Cost_support_20191","Chronic.Indicator")]
edge.g3 <- edge3[c("Source","Target","Weight")]

start.time <- Sys.time()
g3 <- network(edge.g3[c("Source","Target")],directed = FALSE, vertices=icd10.g3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g4 ###########
icd10.g4 <- icd104[c("Id","Category","Prevalence_20192","Cost_support_20192","Chronic.Indicator")]
edge.g4 <- edge4[c("Source","Target","Weight")]

start.time <- Sys.time()
g4 <- network(edge.g4[c("Source","Target")],directed = FALSE, vertices=icd10.g4)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g5 ###########
icd10.g5 <- icd105[c("Id","Category","Prevalence_20201","Cost_support_20201","Chronic.Indicator")]
edge.g5 <- edge5[c("Source","Target","Weight")]

start.time <- Sys.time()
g5 <- network(edge.g5[c("Source","Target")],directed = FALSE, vertices=icd10.g5)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g6 ###########
icd10.g6 <- icd106[c("Id","Category","Prevalence_20202","Cost_support_20202","Chronic.Indicator")]
edge.g6 <- edge6[c("Source","Target","Weight")]

start.time <- Sys.time()
g6 <- network(edge.g6[c("Source","Target")],directed = FALSE, vertices=icd10.g6)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

####### generate g7 ###########
icd10.g7 <- icd107[c("Id","Category","Prevalence_20211","Cost_support_20211","Chronic.Indicator")]
edge.g7 <- edge7[c("Source","Target","Weight")]

start.time <- Sys.time()
g7 <- network(edge.g7[c("Source","Target")],directed = FALSE, vertices=icd10.g7)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken










###### unweighted model1 with selected categories ######
start.time <- Sys.time()
model01 <- ergm(g1 ~ edges+nodecov(~Prevalence_20181)
                +nodecov(~Cost_support_20181)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model01)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model2 with selected categories ######
start.time <- Sys.time()
model02 <- ergm(g2 ~ edges+nodecov(~Prevalence_20182)
                +nodecov(~Cost_support_20182)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model02)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model3 with selected categories ######
start.time <- Sys.time()
model03 <- ergm(g3 ~ edges+nodecov(~Prevalence_20191)
                +nodecov(~Cost_support_20191)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model03)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model4 with selected categories ######
start.time <- Sys.time()
model04 <- ergm(g4 ~ edges+nodecov(~Prevalence_20192)
                +nodecov(~Cost_support_20192)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model04)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model5 with selected categories ######
start.time <- Sys.time()
model05 <- ergm(g5 ~ edges+nodecov(~Prevalence_20201)
                +nodecov(~Cost_support_20201)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model05)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model6 with selected categories ######
start.time <- Sys.time()
model06 <- ergm(g6 ~ edges+nodecov(~Prevalence_20202)
                +nodecov(~Cost_support_20202)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model06)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### unweighted model7 with selected categories ######
start.time <- Sys.time()
model07 <- ergm(g7 ~ edges+nodecov(~Prevalence_20211)
                +nodecov(~Cost_support_20211)
                +nodefactor("Category", levels=c("Endocrine, nutritional and metabolic diseases",
                                                 "Diseases of the circulatory system", "Mental and behavioural disorders",
                                                 "Diseases of the respiratory system"))
                +nodefactor("Chronic.Indicator", levels=c("a","c")),  
                control=control.ergm(MCMC.interval = 10000 ,MCMLE.maxit = 100 ,seed = 42, force.main = TRUE))
summary(model07)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken








gof(model01)

###### Goodness and diagnostics of model1 ######
#start.time <- Sys.time()
#gof.model1 <-gof(model01)
#par(mfrow=c(2,2))
#plot(gof.model1)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model01)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

###### Goodness and diagnostics of model2 ######
#start.time <- Sys.time()
#gof.model2 <-gof(model02)
#par(mfrow=c(2,2))
#plot(gof.model2)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model02)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

###### Goodness and diagnostics of model3 ######
#start.time <- Sys.time()
#gof.model13 <-gof(model03)
#par(mfrow=c(2,2))
#plot(gof.model3)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model03)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

###### Goodness and diagnostics of model4 ######
#start.time <- Sys.time()
#gof.model4 <-gof(model04)
#par(mfrow=c(2,2))
#plot(gof.model4)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model04)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

###### Goodness and diagnostics of model5 ######
#start.time <- Sys.time()
#gof.model5 <-gof(model05)
#par(mfrow=c(2,2))
#plot(gof.model5)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model05)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

###### Goodness and diagnostics of model6 ######
#start.time <- Sys.time()
#gof.model6 <-gof(model06)
#par(mfrow=c(2,2))
#plot(gof.model6)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model06)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken

###### Goodness and diagnostics of model7 ######
#start.time <- Sys.time()
#gof.model7 <-gof(model07)
#par(mfrow=c(2,2))
#plot(gof.model7)
#par(mfrow=c(2,2))
#mcmc.diagnostics(model07)
#end.time <- Sys.time()
#time.taken <- end.time - start.time
#time.taken