##################################################################################
# Goal: This is a demo to show how the Hetcor PC algorithm works.
##################################################################################

#### 0. Dependent Packages ####

library(polycor)
library(pcalg)
source('R/gaussCItestLocal.R')

#### 1. Read Data ####

## an example
X <- iris[,-4]

#### 2. Learning Correlation Matrix and Effective Sample Size ####

## inference
het.obj <- hetcor(X)
## heterogeneous correlations
het.corr <- het.obj$correlations
## standard deviations
het.sd <- het.obj$std.errors
## effective sample size
het.ess <- ((1-het.corr^2)/het.sd)^2

#### 3. Call the PC Algorithm for Causal Discovery ####

##
graph.het <- pc(suffStat = list(C = het.corr, n = nrow(X), ESS.Mat = het.ess), 
                indepTest = gaussCItestLocal, alpha = 0.01, p = ncol(X))
