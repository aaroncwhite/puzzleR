library(rgl)
library(magrittr)
# 6 4x2x1 blocks
# 6 3x2x2 blocks
# 5 1x1x1 blocks
# They must be combined to form a 5x5x5 cube. 


# Piece Interactions ------------------------------------------------------
place_piece <- function(current_cube, area, piece) {
  current_cube[area[[1]], area[[2]], area[[3]]] <- piece$shape
  return(current_cube)
}

test_piece <- function(current_cube, piece) {
  # test if current piece can be placed 
  # if true, return position bounds
  # if false, return false
  cur_view <- examine_cube(current_cube)
  d <- dim(piece$shape)
  
  # need to find if there is a space with the same dims as
  # the piece given
  matches <- list()
  pos <- unique(unlist(as.data.frame(cur_view$base)))
  pos <- pos[!is.na(pos)]
  for (i in pos) {
    test <- cur_view$base == i

    matches <- append(matches, fit_piece(d, test, c(i:(i + d[3] - 1))))
  }
  
  # Now to evaluate which is best?  Need to prioritze how 
  # pieces are placed.
  
  return(matches)
}



check_piece <- function(piece_dims, test_area, height) {
  # Given a piece and a 2 dimensional space, how many 
  # ways can that piece fit onto the space without
  # rotating the piece?
  
  fit <- list()
  for (i in 1:(nrow(test_area))) { # go through each row
    for (j in 1:(ncol(test_area))) { # and each column
      r_end <- i + piece_dims[1] - 1 # see if we would pass the bottom of the area for length
      c_end <- j + piece_dims[2] - 1 # or too far right for width

      if (r_end <= nrow(test_area) & c_end <= ncol(test_area)) { # make sure we're still in bounds
        temp_area <- test_area[i:(r_end), j:(c_end)] 
        if (all(temp_area == T & !is.na(temp_area)) && max(height) <= 5) { # see if the whole area is TRUE that we're testing (ie level for the piece to sit)
          fit <- append(fit, list(list('rows'= i:(i+piece_dims[1] -1),'cols' = j:(j+piece_dims[2]-1),'height' = height)))
        }

      }
    }
  }
  
  return(fit)
  
}

rotate_piece <- function(piece, move) {
  # moves of 'flip', 'spin' 
  # 'flip' will change the height parameter
  # and switch with the length
  # 'spin' will swap the length and width parameters
  # returns the list object with new shape
  
  if (piece$pref != 'cube') { # no point in moving the cube
    switch(move,
           'flip' = { 
             new_piece <- piece
             new_piece$l <- piece$h
             new_piece$h <- piece$l
             new_piece$shape <- array(new_piece$pref, c(new_piece$l, new_piece$w, new_piece$h))
           },
           'spin' = {
             new_piece <- piece
             new_piece$l <- piece$w
             new_piece$w <- piece$l
             new_piece$shape <- array(new_piece$pref, c(new_piece$l, new_piece$w, new_piece$h))
           })
  }
  return(new_piece)
}

# plot_piece <- function(shape) {
#   shp <- melt(shape)
#   shp <- shp[!is.na(shp),]
#   for (i in 1:3) {
#     tmp <- shp[shp[,i] == 1,]
#     tmp[,i] <- 0
#     tmp.1 <- tmp
#     tmp.1[,i] <- 5
#     tmp.1[,-i] <- 0
#     tmp <- rbind.fill(tmp, tmp.1)
#     
#     shp <- rbind.data.frame(shp, tmp)
#   }
#   shp <- rbind(shp, c(0, 0, 5, shp$value[1]))
#   scatterplot3d(shp$Var1, shp$Var2, shp$Var3, pch=16, highlight.3d=TRUE,
#                                type="h", main=paste0('Piece dims: ',paste0(dim(shape), collapse='x')),
#                 xlab = 'Length',tick.marks = F,
#                 ylab = 'Width',
#                 zlab = 'Height'
#                 )
# }

# Cube interactions -------------------------------------------------------
examine_cube <- function(current_cube, height=NA) {
  # goal of this is to "flatten" the cube down to two dimensions
  # as though we were looking straight down at it to see where open space
  # exists
  
  # create an empty view
  view <- matrix(NA, nrow=5, ncol=5)
  positions <- as.data.frame(which(is.na(current_cube), arr.ind = T))
  positions <- positions[order(positions$dim3),]
  for (i in nrow(positions):1) { # for all of the empty positions
    view[positions$dim1[i], positions$dim2[i]] <- positions$dim3[i] # fill in that height spot in the cube
  }# lowest unoccupied position will remain. 
  
  return(list('base' = view, 'max_height' = (5 - view)))
}


# Setup -------------------------------------------------------------------
make_pieces <- function() {
  # make a list with each type of piece
  pieces <- list(list(n = 6, l = 4, w = 2, h = 1, pref = 'long', shape = array('long', c(4, 2, 1))),
                 list(n = 6, l = 3, w = 2, h = 2, pref = 'stump', shape = array('stump', c(3, 2, 2))), 
                 list(n = 5, l = 1, w = 1, h = 1, pref = 'cube', shape = array('cube', c(1, 1, 1))))
  puzzle_pieces <- list()
  for (i in pieces) {
    # make enough pieces to match
    puzzle_pieces <- append(puzzle_pieces, rep(list(i[-1]), i$n))
  }
  return(puzzle_pieces)
}



# Plotting -----------------------------------------------
plot_shape <- function(shape, na_blocks=F, spin=F, spin_axis=c(0,0,1), spin_rpm=5, spin_time=15) {
  clear3d()
  
  for (h in 1:dim(shape)[3]) {
    for (col in 1:dim(shape)[2]) {
      for (row in 1:dim(shape)[1]) {
        val <- shape[row, col, h]
        if (!is.na(val)) {
          make_cube(row, col, h, filled = T, bordered=F, fillcol = switch(val, 
                                                                             'long' = '#FF0000',
                                                                             'stump' = '#0000FF',
                                                                             'cube' = '#00FF00'
                                                                             ), alpha = .5)
        }
        else if (na_blocks) {
          make_cube(row, col, h, filled = T, bordered=F, fillcol=grey(.001), alpha=.015)
        }
        
      }
    }
  }
  if (spin) {
    movie3d(spin3d(axis=spin_axis, rpm=spin_rpm), duration=spin_time) 
    
  }
}

make_cube <- function(x=0,y=0,z=0, bordered=TRUE, 
                      filled = TRUE, lwd=2, scale=1,
                      fillcol = gray(.95), alpha=.5,
                      bordercol ='black', ...) {
  
  mycube <- cube3d(alpha=alpha)
  
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
  mycube %>% translate3d(x, y, z) %>% shade3d
  # }
}

