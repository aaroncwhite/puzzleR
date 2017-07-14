# load the helper functions
source('functions.R')


# Set up the initial blank cube and generate the pieces we'll use to fill it
pz <- make_pieces()
cb <- array(0, c(5,5,5))

# If computer supports OpenGL, this will plot a 3d image of the empty cube
plot_shape(cb, T)


# Explicity place a few pieces
cb[1:2,1:4,1] <- 2
cb[3:4,1:2, 1:3] <- 1
cb[5,1:2, 1:4] <- 2
cb[3:5, 3:4, 1:2] <- 1
cb[2:5, 5, 1:2] <- 2
cb[1,5,1] <- 3

# See how the cube has now been filled
plot_shape(cb)

# Remove the pieces we just placed
pz <- pz[-c(1,2,7:9, 17)]


# Let the function do the rest
a <- solve_puzzle(cb, pz[7:10], tree_length=1, trim = T, nc=4)

# View the one and only solution
plot_shape(a$results[[1]])
