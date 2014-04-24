# imptools.R
# A collection of functions useful for working with implausibility
# measures, and emulators.
# D.J. McNeall
# dougmcneall@gmail.com


impl <- function(em, em.sd, disc, disc.sd, obs, obs.sd){
  # implausibility function
  # All uncertainties should be expressed as a single standard deviation.

  impl.squared <-  (em - disc - obs)^2 / (em.sd^2 + disc.sd^2 + obs.sd^2)

  impl <- sqrt(impl.squared)

  impl

  
}


impl.dk <- function(X, y, y.target, X.test, disc = 0, disc.sd = 0, obs.sd = 0){
  # Basic Gaussian process (DiceKriging) implausibility function
  
  fit <- km(design = X, response = y)
  
  pred <- predict(fit, newdata = X.test, type = 'UK')
  
  impl.em <- impl(em = pred$mean,
		                    em.sd = pred$sd,
		                    disc = disc,
		                    disc.sd = disc.sd,
		                    obs = y.target,
		                    obs.sd = obs.sd
		                 )
  
  return(list(X.test = X.test,
              pred = pred,
              impl.em = impl.em
              )
         )
}



# ---------------------------------------------------------------------
# 2. 
# ---------------------------------------------------------------------

emulate.implausibility.gp <- function(X, y, y.target, B, n.em, disc = 0, disc.sd = 0, obs.sd = 0, X.em = NULL){
  # Emulate implausibility using Gaussian Process emulator
  # Deprecated (in effect replaced by 
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


prop.thres <- function(x, thres, above = FALSE){
  # propotion of vector x below a threshold thres
  n <- length(x)

  if(above) bt <- length(x[x > thres])
  
  else bt <- length(x[x < thres])

  prop <- bt/n
  
  prop
  
}
