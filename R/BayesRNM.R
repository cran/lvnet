# Functions needed:

# Compute S:
computeS <- function(Y, mu, Lambda, Omega){
  Reduce("+",
    lapply(
      seq_len(nrow(Y)), function(i){
        (Y[i,] - mu - Lambda %*% Omega[i,]) %*% t(Y[i,] - mu - Lambda %*% Omega[i,])
      }
    ))
}


# Method from Pan, J., Ip, E., & Dube, L. (in press). An alternative to post-hoc model modification in confirmatory factor analysis: The bayesian lasso. Psychological Methods.
BayesRNM <- function(data, 
                     lambda, 
                     startValues=list(),
                     nBurnin = 100, # Pan suggests 10k
                     nIter = 100 # Pan suggests 10k
                       ){
  
  # Number of variables:
  nVar <- ncol(data)
  nPerson <- nrow(data)
  
  # Construct empty lambda if needed:
  if (missing(lambda)){
    lambda <- matrix(,ncol=0,nrow=nVar)
  }
  
  # Number of latents:
  nLatent <- ncol(lambda)
  
  # Generate start values:
  # Factor loadings:
  Lambda <- start("lambda",startValues,ifelse(is.na(lambda),1,lambda))
  
  # Latent cov matrix (termed Phi in Pan et al)
  Psi <- start("psi",startValues,diag(ncol(lambda)))
  
  # Tau is upper triangle?
  Tau <- Psi[upper.tri(Psi,diag=TRUE)]
  
  # Residual cov matrix (termed Psi in Pan et al):
  Theta <- start("theta",startValues,diag(nVar))
  Theta_inverse <- solve(Theta)

  # Means (not used by lvnet), termed mu in Pan et al:
  mu <- colMeans(data, na.rm=TRUE)
  
  # Omega (latent variables):
  Omega <- matrix(0, nPerson, nLatent)
  
  # Y is data:
  Y <- data
  if (any(is.na(Y))){
    stop("Missing data not yet implemented")
  }
  
  # S:
  S <- computeS(Y, mu, Lambda, Omega)
  
  # Update 
  
}