# library(rgl)
library(magrittr)
library(combinat)
library(foreach)
library(doParallel)
library(RJSONIO)
# 6 4x2x1 blocks
# 6 3x2x2 blocks
# 5 1x1x1 blocks
# They must be combined to form a 5x5x5 cube.

# Solving Interactions
solve_puzzle <- function(cube, pieces, trim=T,tree_length=2, nc=1, plot=F, max_plots=10,
                         sink_dir='sink/', trim_chunk_size=NA, trim_skip=1,
                         level=1) {
  # adding two options here
  start_time <- Sys.time()
  if (nc > 1) {
    if (!dir.exists(sink_dir)) dir.create(sink_dir)
    # make the initial step
    cat(rep('=', 50),'\n')
    cat('Parallel processing:\n',
        'Clusters:',nc,
        '\nTree length:', tree_length,
        '\nTrim:', trim
        )
    cat(rep('=', 50),'\n')

    if (is.array(cube)) {
      # Make the inital branch here if starting with only one cube
      cubes <- list(cube)
    }
    else {cubes <- cube}


    cat(length(cubes), 'starting branches\n')
    cat("Evaluating", tree_length,"levels at a time with", nc, "processes\n")
    cat(rep('-', 50), '\n')

    # Set up the
    i <- 1
    cl <- makeCluster(nc) # the main cluster
    registerDoParallel(cl)

    foreach(1:nc) %dopar% {source('functions.R')}

    while (T) {

      cat(rep('-', 50), '\n')
      n_cubes <- length(cubes)
      cat(n_cubes,'new branches started... step',i,'...\n')


      # the current number of cubes generated will all be filled nc
      cubes <- foreach(j=1:length(cubes), .combine=append) %dopar% {
        # some params for the output for monitoring purposes
        # s_i <- (j %% nc)
        # if (s_i == 0 ) s_i <- nc
        # sink(paste0(sink_dir, s_i, '.txt'), append = T)

        # source the functions
        source('functions.R')
        cat('Branch',j, "->", tree_length,'level(s) deep ------------------------------------------\n')

        cb <- fill_puzzle(cubes[[j]], pieces[i:min(c(i+tree_length - 1), length(pieces))]) # %>% trim_cubes()
        cat(length(cb), 'current cubes... \n')
        # cb
        # cat(length(cb), 'trimmed cubes...\n')
        # sink()
        # cb <- cb[sapply(cb, function(x) !is.null(x))]

        cb
      }


      s_cubes <- length(cubes)

      cat(s_cubes, 'current results...\n')
      # shuffle the cubes around so we aren't just looking at one branch to the next
      # cubes %<>% .[sample(1:s_cubes, s_cubes)]

      if (trim & i %% trim_skip == 0) {
        if (length(cubes) > 400)  cubes %<>% trim_cubes.parallel(nc=nc, cl = cl, level=1,split_size = trim_chunk_size)
        else cubes %<>% trim_cubes()
      }
      # cubes %<>% trim_cubes()
      cat(s_cubes- length(cubes), 'pruned from branches\n')
      i <- i + tree_length
      if (i > length(pieces) | length(cubes) == 0) break
    }
    stopCluster(cl)
    solutions <- cubes #
  }
  else {
    solutions <- fill_puzzle(cube, pieces, trim=trim)
  }
  end_time <- Sys.time()

  if (plot) {
    plots <- solutions[sample(1:length(solutions), min(1, max_plots))]
    for (i in plots) {
      plot_shape(i)
      readline('Press enter...')
    }
  }
  cat(length(solutions), 'final cubes\n')
  return(list('results' = solutions, 'n_results' = length(solutions),
              'time' = list('start' = start_time, 'end' = end_time, 'total' = end_time - start_time),
              'opts' = list('nc' = nc, 'tree_length' = tree_length, 'trim'=trim
                            )))
}


fill_puzzle <- function(cube, pieces) {
  # Put it all together here. Take a blank cube, and start putting
  # pieces in one by one, continually trimming duplicates as we go
  # along.  The pieces go from largest to smallest by volume, so the
  # 'stumps' will go first.  It makes sense to occupy those in the space
  # first in order to take up as much space.  The first task will be to
  # place the 6 stumps into the cube and find non-duplicate solutions.
  # then slide in the 'long' pieces and finally the cubes.
  # Try a recursive solution to fill a cube and follow that path until hitting a dead end.

  # if (!dir.exists(sink_dir)) dir.create(sink_dir)


  if (length(pieces) < 1 & !is.null(cube)) {
    # cat('end of branch\n')
    return(list('cube' = cube))
    break
  }
  if (is.array(cube)) {
    cubes <- puzzle_step(cube, pieces[[1]])
    # # cat(length(pieces), ' piece(s) ')
    # cat(length(cubes), 'new branch(es) -> ')
  }
  else {
    cubes <- cube
  }

  if (length(cubes) > 0) {
    branches <- list()
    for (i in 1:length(cubes)) {
      branch <- fill_puzzle(cubes[[i]], pieces[-1])
      branches %<>% append(branch)
    }
    # cat('trimming cubes \n')
    return(branches) #

  }


 else return(NULL)


}

puzzle_step <- function(cube, piece) {
  cubes <- lapply(test_piece(cube, piece),
                  function(j) {
                    if (!is.null(j)) place_piece(cube, j[1:3], j[[4]])
                    else return(NULL)
                  }
  )

  return(cubes)
}


trim_cubes.parallel <- function(cubes, nc=4, cl=NULL,split_size=NA, max_iter_nochange=50,
                                lazy_stop = T, lazy_stop_point= ceiling(length(cubes)/2), sink_dir='sink/',
                                level=1) {
  # apply trimming in parallel
  cubes <- cubes[sapply(cubes, function(p) !is.null(p))]
  n_cubes <- length(cubes)
  cubes <- cubes[sample(1:length(cubes),length(cubes))]

  # if (missing(compare)) compare <- cubes
  if (is.na(split_size)) split_size <- ceiling(n_cubes/(log(n_cubes)*nc))

  compare <- sapply(cubes, as.vector)

  if (n_cubes > 1) {
    # If not starting already with an existing cluster, make one and have each
    # worker source the appropriate functions
    if (is.null(cl)) {
      cl <- makeCluster(nc)
      registerDoParallel(cl)
      foreach(i=1:nc) %dopar% {source('functions.R')}
      stop_cl <- T
    }
    else {
      stop_cl <- F
    }

    jump <- 0

    if (level == 1) cat('Checking', split_size,'cubes concurrently\n')


    dupes <- rep(F, n_cubes)
    uniques <- c()


    stop_iter <- 0
    i <- 1
    while (T) {
      d.orig <- table(dupes)

      ind.1 <- which(dupes[i:(i+split_size)] == F) + (i-1)
      # ind.1 %<>% .[.<= n_cubes]
      ind <- ind.1[!(cubes[ind.1] %>% trim_cubes(ret_index=T, output=F))] #%>% .[!is.na(.)]

      if (length(ind) > 0) {
        dupes[ind.1][!(ind.1 %in% ind)] <- T
        # splits <- calcSplits(ind, ceiling(length(ind)/nc))
        dupes.ind <- foreach(j=ind, .combine=c) %dopar% {
          # source('functions.R')

          dupes.compared <- compare_cubes(cubes[[j]], cubes[j:n_cubes], dupes[j:n_cubes], ret_index = T)
          return(dupes.compared + (j-1) )
        } #%>% .[!(. %in% ind)]

        dupes[dupes.ind[!(dupes.ind %in% ind)]%>% unique()] <- T
        # dupes[ind] <- F
      }


      d <- table(dupes)

      flush.console()
      i <- i + split_size
      cat('\rdepth',level,"-->", i, "scanned... ", d[1], 'unique...', d[2], 'duplicates...', rep(' ', 10))


      if (all(dupes[i:n_cubes] == T) | i >= length(dupes)) {cat('\nNo unique remaining\t');break}
      if (lazy_stop & d[1] <= lazy_stop_point)  {cat('\nLazy stop point reached\t');break}

      if (identical(d, d.orig)) stop_iter <- stop_iter + 1 else stop_iter <- 0
      if (stop_iter == max_iter_nochange) {cat('\nMax iterations without change reached\t');break}


    }


    cubes %<>% .[!dupes]
    if (stop_cl == T) stopCluster(cl)

  }
  if (level == 1) cat('scan complete', paste0(rep(' ',20), collapse = ''), '\n\n')
  # cat('Trimmed',n_cubes - length(cubes), 'cubes\n')
  return(cubes)

}

trim_cubes <- function(cubes, ret_index=F, output=T) {
  # Reduce the number of cubes seen by removing duplicates
  # if orientation were switched.  Inspired by the seive
  # of erosthenes
  # if (missing(compare)) compare <- cubes

  n_cubes <- length(cubes)
  dupes <- rep(F, length(cubes))

  if (n_cubes> 1) {
    dupes[sapply(cubes, function(p) is.null(p))] <- T
    for (i in 1:n_cubes) {
      if (dupes[i] == F) {
        dupes[i:n_cubes] <- compare_cubes(cubes[[i]], cubes[i:n_cubes], dupes[i:n_cubes])
        dupes[i] <- F
        d <- table(dupes)
        if (output) cat('\rChecking cubes:', d[1], 'unique...', d[2], 'duplicates...', paste0(rep(' ',20), collapse = ''))
      }
      # if (all(dupes == F)) break
    }
    if (output) cat('\n')
    cubes %<>% .[!dupes]
  }
  # cat("removed", n_cubes - length(cubes),'\n')
  # cat('Trimmed',n_cubes - length(cubes), 'cubes\n')
  if (ret_index) return(dupes)
  else return(cubes)
}

compare_cubes <- function(cube, compare, dupes, index, ret_index=F) {
  orients <- examine_cube(cube) # look at all the orientations of the first cube
  for (j in 1:length(orients)) { # examine each orientation of the cube and compare to all other cubes
    ind <- which(dupes == F)
    unique_cubes <- compare[!dupes]
    if (length(unique_cubes) > 0) {
      comp <- orients[[j]]
      sub_duplicates <- sapply(unique_cubes, function(x) identical(x, comp))
      dupes[ind[sub_duplicates]] <- T # mark those that match
    }
    else {
      break
    }

  }
  if (!missing(index)) dupes[index] <- F
  if(ret_index) {
    return(which(dupes == T))
  }
  else {
    return(dupes)

  }
}

compare_cubes.jim <- function(cube, compare, dupes, index, ixCorners) {
  # orients <- examine_cube(cube) # look at all the orientations of the first cube
  cube <- as.vector(cube)
  for (j in 1:ncol(ixCorners)) { # examine each orientation of the cube and compare to all other cubes
    ind <- which(dupes == F)
    unique_cubes <- compare[!dupes]
    if (length(unique_cubes) > 0) {
      comp <- cube[ixCorners[,j]]
      sub_duplicates <- sapply(unique_cubes, function(x) identical(x, comp))
      dupes[ind[sub_duplicates]] <- T # mark those that match
    }
    else {
      break
    }

  }
  if (!missing(index)) dupes[index] <- F

  return(dupes)
}

# Piece Interactions ------------------------------------------------------
place_piece <- function(current_cube, area, piece) {
  current_cube[area$rows, area$cols, area$height] <- piece$shape
  return(current_cube)
}

test_piece <- function(current_cube, piece) {
  # test if current piece can be placed
  # if true, return position bounds
  # if false, return false
  matches <- list()
  for (i in piece) {
    matches <- append(matches, check_piece(i, current_cube))
  }

  # Now to evaluate which is best?  Need to prioritze how
  # pieces are placed.
  if (length(matches) == 0) return(NULL) else return(matches)


}

check_piece <- function(piece, test_space) {
  # Given a piece and a 3 dimensional space, how many
  # ways can that piece fit onto the space without
  # rotating the piece?

  piece_dims <- dim(piece$shape)

  cube_dims <- dim(test_space)
  fit <- list()
  for (r in 1:cube_dims[1]) {
    for (col in 1:cube_dims[2]) {
      for (h in 1:cube_dims[3]) {
        r_end <- r + piece_dims[1] - 1
        c_end <- col + piece_dims[2] - 1
        h_end <- h + piece_dims[3] - 1
        rows <- r:r_end
        cols <- col:c_end
        height <- h:h_end

        if (r_end <= cube_dims[1] & c_end <= cube_dims[2] & h_end <= cube_dims[3]) {
          if (all(test_space[rows, cols, height] == 0)) {
            fit <- append(fit, list(list('rows'= rows,'cols' = cols,'height' = height, 'piece' = piece)))

          }
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
  new_piece <- NULL
  if (piece$pref != 3) { # no point in moving the cube
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


# Cube interactions -------------------------------------------------------
examine_cube <- function(current_cube, plot=F) {
  # goal of this is to "flatten" the cube down to two dimensions
  # as though we were looking straight down at it to see where open space
  # exists
  t.cc <- current_cube
  p <- permn(1:3) # this should match dimensions
  views <- list()

  for (i in p) {
    x <- aperm(t.cc, perm = c(i[1], i[2],i[3]), resize=F, keep.class=T)
    views <- append(views, list(x))
    if (plot) {
      plot_shape(x, na_blocks = T)
      readline("Press enter...")
    }
  }

  return(views)
}

# Setup -------------------------------------------------------------------
make_pieces <- function() {
  # make a list with each type of piece
  pieces <- list('stump' = list(n = 6, l = 3, w = 2, h = 2, pref = 1, shape = array(1, c(3, 2, 2))),
                 'long' = list(n = 6, l = 4, w = 2, h = 1, pref = 2, shape = array(2, c(4, 2, 1))),
                 'cube' = list(n = 5, l = 1, w = 1, h = 1, pref = 3, shape = array(3, c(1, 1, 1))))

  rotations <- rep(c('flip', 'spin'), 4)

  for (p in 1:length(pieces)) {
    piece <- pieces[[p]]
    if (piece$pref != 3) {
      orientations <- list()
      for (r in rotations) {
        piece <- rotate_piece(piece, r)
        orientations <- append(orientations, list(piece))
      }
      keep <- !duplicated(apply(sapply(orientations, function(x) dim(x$shape)), 2, function(j) paste0(j, collapse='')))
      orientations <- orientations[keep]
      pieces[[p]] <- list('n' = pieces[[p]]$n, 'orientations' = orientations)
    }
    else {
      pieces[[p]] <- list('n' = piece$n, 'orientations' = list(piece))
    }

  }

  puzzle_pieces <- list()
  for (i in pieces) {
    # make enough pieces to match
    puzzle_pieces <- append(puzzle_pieces, rep(list(i$orientations), i$n))
  }
  return(puzzle_pieces)
}



# Plotting -----------------------------------------------
plot_shapes <- function(shape_list, na_blocks=F, spin=F, spin_axis=c(0,0,1), spin_rpm=5, spin_time=15) {
  for (i in shape_list) {
    plot_shape(i, na_blocks=na_blocks, spin=spin, spin_axis=spin_axis, spin_rpm=spin_rpm, spin_time=spin_time)
    readline("Press enter to contine...")
  }
}

plot_shape <- function(shape, na_blocks=F, spin=F, spin_axis=c(0,0,1), spin_rpm=5, spin_time=5) {
  clear3d()

  for (h in 1:dim(shape)[3]) {
    for (col in 1:dim(shape)[2]) {
      for (row in 1:dim(shape)[1]) {
        val <- shape[row, col, h]
        if (val != "0") {
          make_cube(row, col, h, filled = T, bordered=F, fillcol = c('#0000FF', '#FF0000','#00FF00')[as.numeric(val)], alpha = .5)
        }
        else if (na_blocks) {
          make_cube(row, col, h, filled = T, bordered=F, fillcol=grey(.001), alpha=.015)
        }

      }
    }
  }
  if (spin) {
    try(movie3d(spin3d(axis=spin_axis, rpm=spin_rpm), duration=spin_time), silent = T)

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

calcSplits <- function(df, splitBy) {
  # Used by postDHIS2_Values(), this will split a data frame into
  # equal parts for upload to the system.  splitBy determines how
  # many rows to attempt to include in each chunk. The function
  # also divides the remainder of nrow(df)/splitBy across each
  # segment.

  # Ex.-
  # > nrow(df)
  # [1] 1404
  # > calcSplits(df, 700)
  #   start end
  # 1 1     702
  # 2 703   1404

  # returns a dataframe with start and end points for each segment
  if (!is.data.frame(df)) {df <- as.data.frame(1:length(df))}
  # calculate how many splits we think we need
  nsplit <- ceiling(nrow(df)/splitBy)
  split <- rep(splitBy, nsplit)
  remainder <- nrow(df) %% splitBy
  divide_across_all <- floor(remainder/nsplit)
  split <- split + divide_across_all
  divide_across_some <- remainder %% nsplit
  split[1:divide_across_some] <- split[1:divide_across_some]

  cumulative_split <- cumsum(split)

  start <- cumulative_split - (split) + 1
  end <- cumulative_split
  end[length(end)] <- nrow(df)

  splits <- cbind('start' = start, 'end' = end)


  return(splits)
}

