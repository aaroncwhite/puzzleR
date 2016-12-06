library('rgl'); library('magrittr')

cube <- function(x=0,y=0,z=0, bordered=TRUE, 
                 filled = TRUE, lwd=2, scale=1,
                 fillcol = gray(.95),
                 bordercol ='black', ...) {
  
  mycube <- cube3d()
  
  # Reduce size to unit
  mycube$vb[4,] <- mycube$vb[4,]/scale*2
  
#   for (i in 1:length(x)) {
    # Add cube border
    if (bordered) {
      mycube$material$lwd <- lwd
      mycube$material$front <- 'line'
      mycube$material$back <- 'line'
    }
    # Add cube fill
    if (filled) {
      mycube$vb[4,] <- mycube$vb[4,]*1.01
      mycube$material$col <- fillcol
    }
    mycube %>% translate3d(x[i], y[i], z[i]) %>% shade3d
  # }
}

clear3d()
cube(1,1,1, filled=F)
cube(0,0,0, filled=F)
cube(0,1,0, filled=T)
cube(0,0,1, filled=T)

clear3d()
shape <- array('long', c(4,2,1))
for (h in 1:dim(shape)[3]) {
  for (col in 1:dim(shape)[2]) {
    for (row in 1:dim(shape)[1]) {
      cube(row, col, h, filled = T, bordered=F, fillcol = '#FF0000')
    }
  }
}


cube(-1,1:2,-1:-2, bordered=F)
movie3d(spin3d(axis=c(0,0,1), rpm=1), duration=20.95) 
