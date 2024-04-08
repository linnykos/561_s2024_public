generate_random_graph <- function(n,
                                  clique_fraction = 0.2,
                                  density_low = 0.1){
  # check all the arguments are correct
  stopifnot(n %% 1 == 0, n >= 0, 
            clique_fraction >= 0, clique_fraction <= 1,
            density_low >= 0, density_low <= 1)
  
  # generate an unsymmetric matrix
  adj_mat <- matrix(sample(x = c(0,1),
                           size = n^2,
                           prob = c(1-density_low, density_low),
                           replace = T), 
                    nrow = n, ncol = n)
  
  # symmetrize the matrix
  adj_mat <- adj_mat + t(adj_mat)
  adj_mat[adj_mat > 0] <- 1
  diag(adj_mat) <- 1
  
  # form the clique
  clique_size <- ceiling(n * clique_fraction)
  adj_mat[1:clique_size, 1:clique_size] <- 1
  
  # randomize the order of the nodes
  sample_idx <- sample(1:n)
  adj_mat <- adj_mat[sample_idx, sample_idx]
  
  # compute the appropriate reverse order
  rev_order <- sapply(1:n, function(i){
    which(sample_idx == i)
  })
  # to see what happens, try: adj_mat[rev_order, rev_order]
  
  list(adj_mat = adj_mat,
       rev_order = rev_order)
}