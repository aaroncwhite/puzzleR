pz <- make_pieces()
cb <- array(0, c(5,5,5))
plot_shape(cb, T)


cb[1:2,1:4,1] <- 2
cb[3:4,1:2, 1:3] <- 1
cb[5,1:2, 1:4] <- 2
cb[3:5, 3:4, 1:2] <- 1
cb[2:5, 5, 1:2] <- 2
cb[1,5,1] <- 3
plot_shape(cb)

pz <- pz[-c(1,2,7:9, 17)]

a <- solve_puzzle(cb, pz[7:10], tree_length=1, trim = T, nc=4)
plot_shape(a$results[[1]])
