pz <- make_pieces()
# Init the cube
cb <- array(rep(NA, 125), c(5, 5, 5))

# How to occupy the cube?
cb[1:4, 1:2, 1] <- pz[[1]]$shape
cb[1:2, 3:5, 1:2 ] <- pz[[7]]$shape
cb[5,1,1] <- pz[[17]]$shape
cb[3,3,1] <- pz[[17]]$shape

p <- piece

for (i in rep(c('flip', 'spin'), 3)) {
  print(i)
  p <- rotate_piece(p, i)
  print(p$shape)
}
