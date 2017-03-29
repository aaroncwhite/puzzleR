### This is a scratch pad as I was testing different options to make the algorithm run faster.  No guarantee it all still works. 


pz <- make_pieces()
# Init the cube

# Solves in around a minute
cb <- array(0, c(4, 4, 4))
r2<- list()
for (i in c(1)) {
  a2 <- list(solve_puzzle(cb, pz[c(1:3, 7:9,14:17)], T, i, 4, recursively_trim = F))
  names(a2) <- paste0('level_',i)
  r2 %<>% append(., a2)
}


# Solves in around a minute
cb <- array(0, c(4, 4, 4))
r2_greedy<- list()
for (i in c(1)) {
  a2 <- solve_puzzle(cb, pz[c(1:3, 7:9,14:17)], T, i, 4, greedy = T, recursively_trim = F, greedy_fun = max)
  names(a2) <- paste0('level_',i)
  r2_greedy %<>% append(., a2)
}

cb <- array(0, c(4, 4, 4))
r5<- list()
for (q in 1:10 * .1) {
  for (i in c(1, 2, 3, 5, 6)) {
    a5 <- list(solve_puzzle(cb, pz[c(1:3, 7:9,14:17)], T, i, 4, recursively_trim = F, sample = q))
    r5 %<>% append(., a5)
  }

}

lapply(r5, function(x) c(x$time))


# Solves in around a minute
cb <- array(0, c(4, 4, 4))
r2_no_recurse_trim<- list()
for (i in c(1,2,3,5,6, 7)) {
  a <- list(solve_puzzle(cb, pz[c(1:3, 7:9,14:17)], T, i, 4, greedy = F, recursively_trim = F))
  names(a) <- paste0('level_',i)
  r2_no_recurse_trim %<>% append(., a)
}

# based on the results, recursively trimming seems to add a slight speed advantage; however, greedy
# trimming to keep only the largest branches does not.
# a branch length of either 1 or n_pieces/2 seems to be the best tree length before pruning.

# test trim recursion point.
a <- solve_puzzle(cb, pz[1:2], F,1,4)
cl <- makeCluster(4)
registerDoParallel(cl)
trim_points <- list()
for (x in c(ceiling(length(a$results)/4), ceiling(length(a$results)/2))) {
  j <- list(system.time(a$results %>% trim_cubes.parallel(recurse_cut=x, cl=cl)))
  names(j) <- as.character(x)
  trim_points %<>% append(j)

}
stopCluster(cl)



# A little more difficult now
cb <- array(0, c(4, 4, 5))
r3<- list()
while (i < 300) {
    a <- list(solve_puzzle(cb, pz[c(1:3, 7:11,14:17)], T, 1, 4, recursively_trim = F, greedy=F, sample = .1))
    r3 %<>% append(., a)
    i <- i + 1

}

for (q in 1:6 * .1) {

}





system.time(a <- solve_puzzle(cb, pz[c(1:4, 7:10,14:17)], T, 1, 4))



cb <- array(0, c(5, 5, 5))
system.time(a <- solve_puzzle(cb, pz, T, 9, 4, greedy = F, greedy_fun =median, recursively_trim = F, sample_cubes = F))



system.time(b <- solve_puzzle(cb, pz[1:2], F, 1,4))

test <- data.frame(t(sapply(b$results, as.vector)))
dim(test)

library(dplyr)
test <- copy_to(sc, test)







system.time(b %>% trim_cubes.parallel())

# # Parallel actually runs worse
# system.time(a <- solve_puzzle(cb, pz[c(1:2)], T, nc=2))
#
#
# check <- function(x) {
#   if (length(x) > 1) {
#     return(check(x))
#   }
#   else (break)
# }
#
# check(a)
#
#
#
# # How to occupy the cube?
# plot_shape(cb)
# cb[1:4, 1:2, 1] <- pz[[1]]$shape
# plot_shape(cb)
#
# cb[1:2, 3:5, 1:2 ] <- pz[[7]]$shape
# plot_shape(cb)
#
# cb[5,1,1] <- pz[[17]]$shape
# plot_shape(cb)
#
# cb[3,3,1] <- pz[[17]]$shape
# plot_shape(cb)
#
#
# p <- piece
#
# for (i in rep(c('flip', 'spin'), 4)) {
#   print(i)
#   p <- rotate_piece(p, i)
#   plot_shape(p$shape)
#   readline("Press enter...")
#   print(p$shape)
# }
#
# cb[,,1] <- 'long'
# cb[,,2] <- 'stump'
# cb[,,3] <- 'cube'
#
# plot_shape(cb)
# for (i in 1:3) {
#   cb[,,i] <- rotate(cb[,,i])
#   plot_shape(cb)
#   print(cb)
#   readline('Press enter...')
# }
#
# cb <- array(NA, c(3, 3, 3))
#
#
# n.cb1 <- n.cb
# cb <- n.cb
# for (r in 1:3) {
#   for (col in 1:3) {
#     for (h in 1:3) {
#       n.cb[r,col,h] <- cb[h,col,r]
#     }
#   }
# }
#
#
#
# cb <- array(1:27, c(3, 3, 3))
# p <- permn(1:3)
# for (i in p) {
#   print(i)
#   x <- aperm(cb, perm = c(i[1], i[2],i[3]))
#   print(x)
#   plot_shape(x, na_blocks = T)
#
#   readline('Press enter...')
# }
# aperm(cb, perm = c(2,1,3))
#
#
#
# com <- cb
# cb[c(1,3),1,1] <- 'cube'
# com[3,3,3] <- 'cube'
# melt(cb)
#
# pieces <- make_pieces()
# options <- test_piece(cube, pieces[[1]])
# cubes <- lapply(options, function(x) place_piece(cube, x[1:3],x[[4]]))   %>% trim_cubes()
#
# system.time(trim_cubes.parallel(cubes))
# system.time(cubes %<>% trim_cubes())
#
#
# opts <- test_piece(cubes[[1]], pieces[[2]])
# cubes <- lapply(opts, function(x) place_piece(cubes[[1]], x[1:3], x[[4]])) %>% trim_cubes()
#
#
#
#
#
# cube <- array('blank', c(5,5,5))
# for (k in 1:6) {
#   # cat('--------------', k,'\n')
#   # cl <- makeCluster(4)
#   # registerDoParallel(cl)
#
#   if (length(cubes) > 1000) cubes <- cubes[sample(1:length(cubes), floor(length(cubes) *.5))]; print('trim cube')
#   cubes <- foreach(x=cubes, .combine=append) %dopar% {
#     source('functions.R')
#     lapply(test_piece(x, pieces[[k]]), function(j) place_piece(x, j[1:3], j[[4]])) %>% trim_cubes()
#   }
#   stopCluster(cl)
#
#   # cubes <- list()
#   # for (i in 1:length(x)) {
#   #   cubes %<>% append(x[[i]] %>% trim_cubes()) #  # %>% mergesort())
#   # }
#   #
#   # cubes %<>% trim_cubes()
#
#   for (i in sample(1:length(cubes), min(c(1, length(cubes))))) {
#     plot_shape(cubes[[i]], T)
#     # readline("Press Enter...")
#   }
# }
# x <- lapply(cubes, function(x) {lapply(test_piece(x, pieces[[5]]), function(j) place_piece(x, j[1:3], j[[4]]))}) # %>% trim_cubes()})
#
# cubes <- list()
# for (i in 1:length(x)) {
#   cubes %<>% append(x[[i]] %>% trim_cubes())
# }
#
# cubes %<>% trim_cubes()
#
# for (i in sample(1:length(cubes), min(c(10, length(cubes))))) {
#   plot_shape(cubes[[i]], T)
#   readline("Press Enter...")
# }
#
#



