####

##
library(pcalg)
library(BDgraph)

#### 1. ####

n <- 50
p <- 10

##
g <- randomDAG(p, 2/(p-1))
C0 <- cov2cor(trueCov(g))
Z <- rmvDAG(n, g)

#### 2. ####

m <- 1000

##
samp.C1 <- array(NA, dim = c(p, p, m))
G1 <- matrix(0, p, p)
G1[upper.tri(G1)] <- 1
samples1 <- rgwish(m, adj.g = G1, b = n, D = crossprod(Z))
#
for(i in 1:m){
  samp.C1[,,i] <- cov2cor(solve(samples1[,,i])) 
}
#
C1 <- apply(samp.C1, c(1,2), mean)


##
samp.C2 <- array(NA, dim = c(p, p, m))
G2 <- (round(cov2cor(solve(C0)), 2) != 0) * 1 - diag(p)
G2[lower.tri(G2)] <- 0
samples2 <- rgwish(m, adj.g = G2, b = n, D = crossprod(Z))
#
for(i in 1:m){
  samp.C2[,,i] <- cov2cor(solve(samples2[,,i])) 
}
#
C2 <- apply(samp.C2, c(1,2), mean)

##
sqrt(mean((C0-C1)^2))
sqrt(mean((C0-C2)^2))

