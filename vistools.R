# a collection of useful R code functions (mostly plotting)
# By Doug McNeall
# @dougmcneall
# dougmcneall@gmail.com

# pairs.aug
# Multiple pairs plot on the same axes

# dfunc
# Helper function for pairs - plots density of points

# cplot
# scatter plot with colour as a third dimension

# cpoints
# more generic scatter plot with colour as a third dimension

# cpointsTruth 
# helper function for diagonal panel in pairs()
# plot points in colour palette, and the last point in a different colour

# cplotShort
# scatter plot with colour
# as a third dimension (no legend, for small plots)


pairs.aug <- function(..., col = c('grey', 'red'), pch = c(21,19)){
  # A pairs function that takes a number of matrices, and plots them
  # up in different colours

  dat <- list(...)

  column.check <- unlist(lapply(dat,ncol))
  
  stopifnot(length(unique(column.check))==1)

  reps <- unlist(lapply(dat,nrow))

  datmat <- NULL
  
  colvec <- rep(col, c(reps, recursive = TRUE))
  pchvec <- rep(pch, c(reps, recursive = TRUE))
  
  for(i in 1:length(dat)){
    datmat <- rbind(datmat, as.matrix(dat[[i]]))
  }
 
  pairs(datmat, col = colvec, pch = pchvec)
    
}





cplot <- function(x,y,z,cols,legend.title = 'legend.title',...){
  # scatter plot with colour
  # as a third dimension
  
  require(fields)
  par(mar = c(5,4,4,7), las = 1)
  
  ramp <- colorRamp(cols)
  
  plot.col   <-  rgb(ramp((z - min(z))/(max(z) - min(z))), max = 255)
  
  legend.col <-  rgb(ramp(seq(0,1,length = length(z))), max = 255)
  
  zr <- range(z)
  
  plot(x,y, col = plot.col, ...)
  
  image.plot(legend.only = TRUE,
             zlim = zr,
             col = legend.col,
             legend.args = list(text = legend.title, side = 3, line = 1.2)
             )
}


cpoints <- function(x,y,z,cols,legend.title,...){
  # scatter plot with colour
  # as a third dimension
  
  require(fields)
  
  ramp <- colorRamp(cols)
  
  plot.col   <-  rgb(ramp((z - min(z))/(max(z) - min(z))), max = 255)
  
  legend.col <-  rgb(ramp(seq(0,1,length = length(z))), max = 255)
  
  zr <- range(z)
  
  points(x,y, col = plot.col, ...)
  
}



dfunc <- function(x,y,...){
  require(MASS)
  
  # function for plotting 2d kernel density estimates in pairs() plot.
  kde <- kde2d(x,y)
  image(kde, col = br, add = TRUE)
  
}

# Example of adding  density plots to a pairs plot
dfunc.up <- function(x, y, dfunc.col = greys, ...){
  require(MASS)
  
  # function for plotting 2d kernel density estimates in pairs() plot.
  kde = kde2d(x,y)
  image(kde, col = dfunc.col, add = TRUE)
}

  
dfunc.up.truth = function(x,y, dfunc.col = blues9, col = 'black', bg = 'red', cex = 1.5, pch = 21, ...){
  # function for plotting 2d kernel density estimates in pairs() plot,
  # adding a data point overlay in the last row of the input matrix.
  require(MASS)
  
  xtrue <- tail(x,1)
  ytrue <- tail(y,1)
  
  xdash <- head(x, -1)
  ydash <- head(y, -1)
  
  kde <- kde2d(xdash,ydash)
  image(kde, col = dfunc.col, add = TRUE)
  points(xtrue, ytrue, pch = pch, col = col, bg = bg, cex =cex)
}


cpointsTruth <- function(x,y,z,cols,truecol = 'green',legend.title,...){
  # helper function for diagonal panel in pairs()
  # plot points in colour palette, and the last point in a different colour
  
  require(fields)
  
  ramp <- colorRamp(cols)

  xtrue <- tail(x,1)
  ytrue <- tail(y,1)
  
  xdash <- head(x, -1)
  ydash <- head(y, -1)
  zdash <- head(z,-1)
  
  plot.col   <-  rgb(ramp((zdash - min(zdash))/(max(zdash) - min(zdash))), max = 255)
  
  legend.col <-  rgb(ramp(seq(0,1,length = length(zdash))), max = 255)
  
  zr <- range(zdash)
  
  points(xdash,ydash, col = plot.col, ...)
  points(xtrue,ytrue, col = 'black', bg = truecol, pch = 21, cex = 2)
 }


cplotShort <- function(x,y,z,cols,zr,...){
  # scatter plot with colour
  # as a third dimension (no legend, for small plots)
    require(fields)
 
    ramp <- colorRamp(cols)
    
    plot.col   <-  rgb(ramp((z - min(z))/(max(z) - min(z))), max = 255)
    legend.col <-  rgb(ramp(seq(0,1,length = length(z))), max = 255)
    
    zr <- range(z)
    plot(x,y, col = plot.col, ...)
}


