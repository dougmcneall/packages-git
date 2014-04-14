# emtools.R
# A collection of tools for working with emulators.
# Doug McNeall
# dougmcneall@gmail.com

normalize <- function(X, wrt = NULL){
# Normalize a matrix to [0,1] on a per-column basis.
# Normalize relative to matrix wrt if included.

  f <- function(X){
    (X-min(X))/(max(X)-min(X))
  }
  
  # test to see if we have a matrix, array or data frame
  if(length(dim(X))==2){
    out <- apply(X,2,f)
    }
  
  else{	
    out <- f(X)
  }

  if(is.null(wrt) == FALSE){
    # if argument wrt is given
 
    n <- nrow(X)
    mmins <- t(kronecker(apply(wrt,2,min),t(rep(1,n))))
    mmaxs <- t(kronecker(apply(wrt,2,max),t(rep(1,n))))
    
  out <- (X-mmins)/(mmaxs-mmins)
    
  }

  out
}
