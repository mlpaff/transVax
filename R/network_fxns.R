###########################################################
## Contains all network differential equation functions for fitting
## and running the DE models
## Spencer Fox October 2015
###########################################################

trans_parms <- function(beta = .07416408,
                        gamma = 1/3.38,
                        N = 10000,
                        init_infected = 5,
                        deg_dist=normalize(dexp(x=1:N, rate = 1/5)),
                        init_susc = rep((N-init_infected)/N, N),
                        phiI0 = init_infected/N,
                        phiS0 = 1-phiI0,
                        phiR0 = 0
)
  return(as.list(environment()))

get_er_deg_dist <- function(mean_deg, n = 10000){
  degrees <- seq(0, n)
  dpois(x = degrees, lambda=mean_deg)
}

normalize <- function(values){
  ## Normalizes a degree distribution
  values/sum(values,na.rm = T)
}

pLawUnnormalized <- function(degree, alpha, kappa){
  degree^(-alpha)*exp(-degree / kappa)
}

pLawProbs <- function(alpha, kappa, degrees){
  p <- sapply(degrees, FUN = pLawUnnormalized, alpha, kappa)
  normalize(p)
}

calcMeanDeg <- function(deg_dist){
  ## takes in a degree distribution, and returns the mean
  return(sum(seq_along(deg_dist)*deg_dist))
}

calcMeanSqDeg <- function(deg_dist){
  ## takes in a degree distribution, and returns the mean squared degree
  return(sum(seq_along(deg_dist)^2 * deg_dist))
}

calcTheoRNot <- function(beta, deg_dist, gamma=1/3.38){
  ## Given a degree distribution and a beta (per contact transmissibility)
  ## returns the theoretical R not value for the epidemic
  beta/(beta+gamma) * ((calcMeanSqDeg(deg_dist)  - calcMeanDeg(deg_dist)) / calcMeanDeg(deg_dist))
}

findBeta <- function(x, r_not, deg_dist, gamma){
  ## function for finding beta that gives a specific R0
  ## used with uniroot to solve for the beta value
  r_not - calcTheoRNot(x, deg_dist, gamma)
}


getBeta <- function(r_not, deg_dist, gamma=1/3.38, justRoot=TRUE){
  ## for a specific degree distribution and a gamma
  ## finds the beta that gives rise to a specific R not value (supplied).
  ## Returns either just the required beta, or the whole uniroot output.

  if(justRoot){
    uniroot(findBeta, interval = c(0,1), r_not=r_not, deg_dist=deg_dist, gamma=gamma)$root
  } else{
    uniroot(findBeta, interval = c(0,1), r_not=r_not, deg_dist=deg_dist, gamma=gamma)
  }
}



############################################################
## Functions not used right now
##
############################################################
# f_x <- function(pjs, x){
#   ## PGF for the number of infected nodes in generation 1
#   ## pjs <- vector of pj = probability that the index case directly infects j neighbors
#   ## x <- single probability [0,1]
#   ## Returns single numeric value
#   sum(pjs * x^(seq_along(pjs)-1))
# }
#
# p_j <- function(deg_dist, j, tOuts, pTOuts){
#   ## returns probability that index case infects j neighbors
#   ## deg_dist = vector with degree distribution probabilities for network
#   ## j = #neighbors infected
#   ## tOuts = vector of possible transmissibility outs (To in paper)
#   ## pTOuts = probability distribution for transmissibility out
#   ## returns single value
#
#   probs <- vector(mode = "numeric", length = length(deg_dist))
#   for(k in j:length(deg_dist)){
#     probs[k] <- deg_dist[k] * sum(dbinom(x = j, size = k, prob = tOuts) * pTOuts)
#   }
#   sum(probs)
# }
# vecPJ <- Vectorize(p_j, vectorize.args = c("j"))
# getX <- function(x, deg_dist, tOuts, pTOuts){
#   ## function to solve for x for calculating emergence probability
#
#   meanK <- calcMeanDeg(deg_dist)
#   vals <- vector(mode = "numeric", length = length(tOuts))
#   for(ii in 1:length(tOuts)){
#     vals[ii] <- (pTOuts[ii] / meanK) *
#       sum((1+tOuts[ii]*(x-1))^(seq_along(deg_dist)-1) *
#             deg_dist * seq_along(deg_dist))
#   }
#
#   x - sum(vals)
# }
#
# getT <- function(beta, recoveryTime){
#   ## Returns the transmissibility for a node that has
#   ## a specific transmission rate (beta) and recovery time
#   ## recoveryTime can be a vector or single number
#   ## beta can only be a single number
#   1 - exp(-beta*recoveryTime)
# }

# calcTheoPEmerge <- function(beta, gamma, deg_dist){
#   ## Fxn to give the theoretical emergence probability on a
#   ## network under gillespie algorithm simulation
#   ## beta - per contact rate of transmission
#   ## gamma - node recovery rate
#   ## deg_dist - vector giving the degree distribution of the network of interest
#
#   n <- length(deg_dist)
#
#   ## Sample as if network is 100000 nodes
#   nodes <- n*100
#
#   ## First sample from degree distribution to get realistic node degrees
#   netDegs <- sample(1:n, size = nodes, prob = deg_dist, replace = T)
#
#   ## Sample from exponential distribution to assign each node a recovery time
#   netRecTime <- rexp(n = nodes, rate = gamma)
#
#   ## Once each node has recovery time, get the transmissibility for each node
#   nodeTOuts <- getT(beta, netRecTime)
#
#   ## Now weight each nodes transmissibility by it's degree
#   weightedTOuts <- rep(nodeTOuts, netDegs)
#
#   ## The density of those weighted transmissibilities gives our
#   ## Transmissibility distribution
#   tDensity <- density(weightedTOuts, n = 10000)
#   tVals <- tDensity$x[tDensity$x>=0 & tDensity$x<=1]
#   tProbs <- normalize(tDensity$y[tDensity$x>0])
#
#   ## Solve for x that satisfies equation
#   xSolved <- try(uniroot(f = getX, interval = c(0,0.99999999), deg_dist=deg_dist, tOuts=tVals, pTOuts=tProbs), silent = TRUE)
#   if(class(xSolved) == "try-error"){
#     return(0)
#   } else {
#     ## in most cases p_j of j=20 equals 0, so we don't need to solve for larger j values
#     ## However, this may not always be true, so need to check ##
#
#     if(p_j(deg_dist=deg_dist, j=20, tOuts=tVals, pTOuts=tProbs)==0){
#       js <- 0:20
#     } else{
#       js <- 0:50
#     }
#     pjs <- vecPJ(deg_dist=deg_dist, j=js, tOuts=tVals, pTOuts=tProbs)
#     1 - f_x(pjs = pjs, x = xSolved$root)
#   }
# }



