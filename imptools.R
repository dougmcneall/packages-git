# imptools.R
# A collection of functions useful for working with implausibility
# measures, and emulators.
# D.J. McNeall
# dougmcneall@gmail.com

# ---------------------------------------------------------------------
# 1. Define implausibility
# ----------------------------------------------------------------------

impl <- function(em, em.sd, disc, disc.sd, obs, obs.sd){
  # implausibility function
  # All uncertainties should be expressed as a single standard deviation.

  impl.squared <-  (em - disc - obs)^2 / (em.sd^2 + disc.sd^2 + obs.sd^2)

  impl <- sqrt(impl.squared)

  impl

  
}



# ---------------------------------------------------------------------
# 2. emulate implausibility using Gaussian Process emulator
# ---------------------------------------------------------------------

emulate.implausibility.gp <- function(X, y, y.target, B, n.em, disc = 0, disc.sd = 0, obs.sd = 0, X.em = NULL){
  # Emulate to map an input space with a plausibility measure 
  # Inputs:
  # X
  # y
  # y.target
  # B
  
  # Output:
  # X.em
  # impl.em

  # setup output
  impl.em <- NULL
  
  ndims <- ncol(X)
  
  if(is.null(X.em)){

    # sample from a marginally uniform cube
    X.em <- takeunif(n.em, mins = rep(0,ndims),maxes= rep(1, ndims))
    colnames(X.em) <- colnames(X)
  }
  #Pass in the inputs for the emulator
  else X.em <- X.em
  
  # Build emulator
  A <- corr.matrix(X, scales = exp(B$par))
  Ainv <- solve(A)
  
  y.em <- interpolant.quick(x = X.em,
                            d =  y,
                            xold = X,
                            Ainv = Ainv,
                            scales = exp(B$par),
                            give.Z = TRUE
                            )

  # Find implausibility

  impl.em <- impl(em = y.em$mstar.star,
                  em.sd = y.em$Z,
                  disc = disc,
                  disc.sd = disc.sd,
                  obs = y.target,
                  obs.sd = obs.sd
                  )
  
  return(list(X.em = X.em,
              y.em = y.em,
              impl.em = impl.em
              )
         )
  
}

