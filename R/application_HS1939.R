##################################################################################
# Goal: the application of the Hetcor PC algorithm to the HolzingerSwineford1939
#       dataset, which is publicly available in package 'lavaan'.
##################################################################################


#### 0. Dependencies and Parameters ####

## packages
library(lavaan)
library(infotheo)
library(polycor)
library(sbgcop)
library(pcalg)
source('R/gaussCItestLocal.R')

## parameters
# significance level used in the PC algorithm
alpha <- 0.05


#### 1. Load Data ####

## load the full data set
data(HolzingerSwineford1939)
## select the 9 variables of interest
HS9.data <- HolzingerSwineford1939[, paste0('x', 1:9)]
## basic information
n <- nrow(HS9.data)
p <- ncol(HS9.data)
var.names <- names(HS9.data)
## obtain normal scores
Z <- sapply(HS9.data, function(x) qnorm(rank(x)/(n+1)))


#### 2. Causal Discovery ####

### 'true' graph (here it means the resulting graph based on continuous data)
graph.true <- pc(suffStat = list(C = cor(Z), n = n), 
                 indepTest = gaussCItest, labels = var.names, alpha = alpha,
                 skel.method = "stable", maj.rule = T, solve.confl = T)

### discretize some variables to get mixed data Y
Y <- data.frame(Z)
iy2 <- c(2,5,8) # the index of ordinal variables with 2 categories
iy4 <- c(3,6,9) # the index of ordinal variables with 4 categories
Y[,iy2] <- matrix(unlist(discretize(Z[,iy2],nbins = 2)), byrow=FALSE, nrow=n )
Y[,iy4] <- matrix(unlist(discretize(Z[,iy4],nbins = 4)), byrow=FALSE, nrow=n )
Y[,c(iy2,iy4)] <- as.data.frame(lapply(Y[,c(iy2,iy4)], factor))

### causal discovery from mixed data Y
## inference for Hetcor
het.obj <- hetcor(Y)
## correlations
het.corr <- het.obj$correlations
## standard deviations
het.sd <- het.obj$std.errors
## effective sample size
het.ess <- ((1-het.corr^2)/het.sd)^2
## call the PC algorithm (order independent version)
graph.het <- pc(suffStat = list(C = het.corr, n = n, ESS.Mat = het.ess), 
                 indepTest = gaussCItestLocal, labels = var.names, alpha = alpha,
                 skel.method = "stable", maj.rule = T, solve.confl = T)

### check the results
par(mfrow = c(1,2))
plot(graph.true, main = 'True graph')
plot(graph.het, main = 'Hetcor PC')
