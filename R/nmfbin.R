#' Non-negative Matrix Factorization for Binary Data
#'
#' @param V A binary matrix (with entries 0 or 1) to factorize.
#' @param k Number of components.
#' @param divergence_type Method for divergence calculation. Choices are "kl" for Kullback-Leibler divergence and "crossentropy" for binary cross-entropy.
#' @param init_method Method for matrix factor initialization. Choices are "svd" for Singular Value Decomposition and "random" for random values.
#' @param max.iter Maximum number of iterations for the factor update algorithm.
#' @param tol Tolerance for convergence in the factor update algorithm.
#' @param learning_rate Learning rate for gradient descent (only used with divergence_type = "crossentropy").
#' @param compute_marginal_divergences Logical indicating whether to compute the marginal divergences.
#' @return A list containing the factor matrices W and H, the final divergence, and if compute_marginal_divergences=TRUE, a vector of marginal divergences.
#' @export
#' @examples
#' # Create a small binary matrix
#' V <- matrix(c(1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1), nrow = 3)
#' 
#' # Perform binary NMF with 2 components using KL divergence and random initialization
#' result <- nmfbin(V, k = 2, divergence_type = "kl", init_method = "random")
#'
#' # Print factor matrices
#' print(result$W)
#' print(result$H)
#' 
#' # Check the final divergence
#' print(result$final_divergence)

nmfbin <- function(V, k, divergence_type = "kl", init_method = "random",
                   max.iter = 100, tol = 1e-5, learning_rate = 0.01, 
                   compute_marginal_divergences = FALSE) {
  
  # Check if V contains only 0s and 1s
  if (!all(V %in% c(0, 1))) {
    stop("The input matrix V must only contain 0s and 1s.")
  }
  
  # Check if V contains at least some 1s
  if (all(V %in% c(0))) {
    stop("The input matrix V must contain at least some 1s.")
  }
  
  # Input matrix dimensions
  m <- nrow(V)
  n <- ncol(V)
  
  # Check if k < ncol(V)
  if (k >= n) {
    stop("Chosen k must be (significantly) smaller than ncol(V).")
  }
  
  # Choice of init_method
  if (init_method == "svd") {
    
    # Use SVD for initialization
    svd_decomp <- svd(V)
    W <- svd_decomp$u[, 1:k] * sqrt(svd_decomp$d[1:k])
    H <- t(t(svd_decomp$v[, 1:k]) * sqrt(svd_decomp$d[1:k]))
    
  } else if (init_method == "random") {
    
    # Random initialization
    W <- matrix(stats::runif(m * k), m, k)
    H <- matrix(stats::runif(k * n), k, n)
    
  } else {
    stop("Invalid initialization method specified")
  }
  
  # Ensure non-negativity (hacky)
  W[W < 0] <- 0
  H[H < 0] <- 0
  
  # Initialize results list
  result <- list()
  
  # Define appropriate divergence function
  compute_divergence <- switch(divergence_type,
                               kl = binary_KL_divergence,
                               crossentropy = binary_crossentropy,
                               stop("Invalid divergence_type specified")
  )
  
  # Select appropriate update rule based on divergence choice
  if (divergence_type == "kl") {
    
    update_factors <- update_factors_KL
    
  } else if (divergence_type == "crossentropy") {
    
    update_factors <- update_factors_crossentropy
    
  }
  
  # Compute updates 
  updates <- update_factors(V, W, H, max.iter, tol, learning_rate)
  
  W = updates$W
  H = updates$H
  
  # record results
  result$W <- W
  result$H <- H
  result$final_divergence <- updates$divergence
  
  # Compute marginal divergences if requested
  if (compute_marginal_divergences) {
    
    divergence_reductions <- numeric(k)
    
    for (dim in 1:k) {
      
      prev_divergence = compute_divergence(V, W %*% H)
      
      updates <- update_factors(V, W, H, max.iter, tol, learning_rate)
      
      divergence_reductions[dim] <- prev_divergence - updates$divergence
      
    }
    
    result$divergence_reductions <- divergence_reductions
    
  }
  
  
  # return
  return(result)
  
}

#' Sigmoid function
#' 
#' @noRd
#' 

sigmoid <- function(X) {
  
  1 / (1 + exp(-X))
  
}

#' KL divergence for binary matrices
#' 
#' @noRd
#' 

binary_KL_divergence <- function(V, V_approx) {
  
  # to avoid division by zero
  epsilon <- 1e-10
  
  # apply sigmoid
  flat_V = as.numeric(V)
  flat_V_approx = as.numeric(sigmoid(V_approx + epsilon))
  
  # clips inputs - probably terrible
  #flat_V = pmin(1 - epsilon, pmax(V, epsilon))
  #flat_V_approx = pmin(1 - epsilon, pmax(V_approx, epsilon))
  
  # KL divergence
  divergence <- sum((flat_V + epsilon) * log(flat_V / flat_V_approx + epsilon) + 
                      (1 - flat_V + epsilon) * log((1 - flat_V + epsilon) / (1 - flat_V_approx))
                    )
  
  return(divergence)
  
}

#' Binary cross-entropy loss
#' 
#' @noRd
#' 

binary_crossentropy <- function(V, V_approx) {
  
  # to avoid division by zero
  epsilon <- 1e-10
  
  # apply sigmoid
  flat_V = as.numeric(V)
  flat_V_approx = as.numeric(sigmoid(V_approx + epsilon))
  
  # result
  out <- -sum((flat_V + epsilon) * log(flat_V_approx) + (1 - flat_V + epsilon) * log(1 - flat_V_approx))
  
  return(out)
  
}

#' Gradient for binary cross-entropy
#' 
#' @noRd
#' 

binary_crossentropy_gradient <- function(V, W, H) {
  
  # apply sigmoid
  V_approx = sigmoid(W %*% H)
  
  dV = V_approx - V
  dW = dV %*% t(H)
  dH = t(W) %*% dV
  
  return(list(dW = dW, dH = dH))
  
}

#' Update factors using KL divergence
#' 
#' @noRd
#' 

update_factors_KL <- function(V, W, H, max.iter, tol, ...) {
  
  # constant to avoid division by zero
  epsilon <- 1e-10
  
  # populate divergence
  prev_divergence <- binary_KL_divergence(V, W %*% H)
  
  # matrix of 1s with V dimensions
  one_matrix <- matrix(rep(1, nrow(V)), nrow=nrow(V), ncol=ncol(V))
  
  for(iter in 1:max.iter) {
    
    # Compute the current approximation of V
    V_approx <- W %*% H + epsilon
    
    # update W
    W <- W * (((V/V_approx) %*% t(H)) / (one_matrix %*% t(H)))
    
    # update H
    H <- H * ((t(W) %*% (V/V_approx)) / (t(W) %*% one_matrix))
    
    # update divergence
    divergence <- binary_KL_divergence(V, W %*% H)
    
    # tolerance check
    if(abs(divergence - prev_divergence) < tol) {
      
      break
      
    }
    
    prev_divergence <- divergence
    
  }
  
  return(list(W = W, H = H, divergence = divergence))
  
}

#' Update factors using binary cross-entropy with gradient descent
#' 
#' @noRd
#' 

update_factors_crossentropy <- function(V, W, H, max.iter, tol, learning_rate) {
  
  prev_loss <- binary_crossentropy(V, W %*% H)
  
  for(iter in 1:max.iter) {
    
    gradients = binary_crossentropy_gradient(V, W, H)
    dW = gradients$dW
    dH = gradients$dH
    
    W = W - learning_rate * dW
    H = H - learning_rate * dH
    
    # Ensure non-negativity
    W[W < 0] <- 0
    H[H < 0] <- 0
    
    loss = binary_crossentropy(V, W %*% H)
    
    if(abs(loss - prev_loss) < tol) {
      break
    }
    prev_loss <- loss
  }
  
  return(list(W = W, H = H, divergence = prev_loss))
  
}
