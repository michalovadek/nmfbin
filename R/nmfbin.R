#' Logistic Non-negative Matrix Factorization
#'
#' This function performs Logistic Non-negative Matrix Factorization (NMF) on a binary matrix.
#'
#' @param X A binary matrix (m x n) to be factorized.
#' @param k The number of factors or components.
#' @param optimizer Type of updating algorithm. `update` for NMF multiplicative update rules or `gradient` for gradient descent.
#' @param init Method for initializing the factorization.
#' @param max_iter Maximum number of iterations for the gradient descent optimization.
#' @param tol Convergence tolerance. The optimization stops when the change in loss is less than this value.
#' @param learning_rate Learning rate (step size) for the gradient descent optimization.
#' @param verbose Print convergence if `TRUE`.
#' @param loss_fun Choice of loss function.
#' @param loss_normalize Normalize loss if `TRUE`.
#' @param epsilon Constant to avoid log(0).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{W}: The basis matrix (m x k).
#'   \item \code{H}: The coefficient matrix (k x n).
#'   \item \code{c}: The global threshold.
#' }
#'
#' @examples
#' \dontrun{
#' # Generate a binary matrix of size 100 x 50
#' set.seed(123)
#' m <- 100
#' n <- 50
#' X <- matrix(sample(c(0, 1), m * n, replace = TRUE), m, n)
#'
#' # Set the number of factors
#' k <- 4
#'
#' # Apply the function
#' result <- nmfbin(X, k)
#' }
#'
#' @export

nmfbin <- function(X, k, optimizer = "mur", init = "nndsvd", max_iter = 1000, tol = 1e-6, learning_rate = 0.001,
                   verbose = FALSE, loss_fun = "logloss", loss_normalize = TRUE, epsilon = 1e-10) {
  
  # Check if X contains only 0s and 1s
  if (!all(X %in% c(0, 1))) {
    stop("The input matrix X must only contain 0s and 1s.")
  }
  
  # Check if X contains at least some 1s
  if (all(X %in% c(0))) {
    stop("The input matrix X must contain at least some 1s.")
  }
  
  # matrix dimensions
  m <- nrow(X)
  n <- ncol(X)
  
  # Check if k < ncol(X)
  if (k >= n) {
    stop("Chosen k must be smaller than ncol(X).")
  }
  
  # Initialize W, H, and threshold c
  init_vals <- initial_values(X, k, m, n, method = init)
  
  W = init_vals$W
  H = init_vals$H
  c = init_vals$c
  
  # SGD does not require initial X_hat
  if (optimizer == "sgd"){
    
    # placeholder value
    X_hat <- 0
    
  } else {
    
    # Compose into initial X_hat
    X_hat <- recombine_matrix(W, H, c) 
    
  }
  
  # previous loss
  prev_loss <- Inf
  
  # record convergence
  convergence <- NA
  
  # iterate
  for (iter in 1:max_iter) {
    
    # update W, H, c
    updated <- update_step(X, W, H, c, X_hat, m, n, type = optimizer, max_iter = max_iter, tol = tol, learning_rate = learning_rate, epsilon = epsilon)
    
    W = updated$W
    H = updated$H
    c = updated$c
    
    # Compose into new X_hat
    X_hat <- recombine_matrix(W, H, c)
    
    # compute loss
    loss <- loss_function(X_true = X, X_hat = X_hat, type = loss_fun, normalize = loss_normalize, epsilon = epsilon)
    
    # record loss
    convergence[iter] <- loss
    
    # Print loss and change
    if (verbose == TRUE){
      
      cat(sprintf("Iteration %d | Divergence from source matrix (log loss) = %f | Change from previous iteration = %f \n", iter, loss, loss - prev_loss)) 
      
    }
    
    # Check for convergence
    if (abs(prev_loss - loss) < tol) {
      break
    }
    
    prev_loss <- loss
    
  }
  
  # return
  return(list(W = updated$W, H = updated$H, c = updated$c, convergence = convergence, final_loss = loss))
  
}

#' Recombine factors into sigmoidal matrix
#' 
#' @noRd
#' 

recombine_matrix <- function(W, H, c){
  
  # matrix multiplication with constant added
  WHc <- W %*% H + c
  
  # sigmoid transformation
  X_hat <- 1 / (1 + exp(-WHc))
  
  # return
  return(X_hat)
  
}

#' Loss functions
#' 
#' @noRd
#' 

loss_function <- function(X_true, X_hat, type = "logloss", normalize = TRUE, epsilon = 1e-10){
  
  # dimensions for normalization
  m <- nrow(X_true)
  n <- ncol(X_true)
  
  # log likelihood loss function, also known as binary cross-entropy loss
  if (type == "logloss"){
    
    # epsilon is added to avoid log(0)
    loss <- -sum(X_true * log(X_hat + epsilon) + (1 - X_true) * log(1 - X_hat + epsilon))
    
  }
  
  # mean squared error
  else if (type == "mse"){
    
    loss <- sum((X_true - X_hat)^2)
    
  }
  
  # normalization (to give per element loss)
  if (normalize == TRUE){
    
    loss <- loss / (m * n)
    
  }
  
  # return
  return(loss)
  
}

#' Optimization
#' 
#' @noRd
#' 

update_step <- function(X_true, W, H, c, X_hat, m, n, type = "mur", max_iter = 1000, tol = 1e-5, learning_rate = 0.001, epsilon = 1e-10){
  
  # optimize with NMF multiplicative update rules
  if (type == "mur"){
    
    # Update rules for W, H, and c
    W <- W * (X_true %*% t(H)) / (X_hat %*% t(H) + epsilon)
    H <- H * (t(W) %*% X_true) / (t(W) %*% X_hat + epsilon)
    c <- c * sum(X_true - X_hat) / (m * n)
    
  }
  
  # optimize with gradient descent
  else if (type == "gradient"){
    
    # Compute the gradients
    dW <- -(X_true - X_hat) %*% t(H)
    dH <- -t(W) %*% (X_true - X_hat)
    dc <- -sum(X_true - X_hat)
    
    # Update W, H, and c using gradient descent
    W <- W - learning_rate * dW
    H <- H - learning_rate * dH
    c <- c - learning_rate * dc
    
    # Ensure non-negativity by setting negative values to a small positive value
    W[W < 0] <- epsilon
    H[H < 0] <- epsilon
    
  }
  
  # optimize with stochastic gradient descent
  else if (type == "sgd"){
    
    # Randomly select a data point from X
    i <- sample(1:m, 1)
    j <- sample(1:n, 1)
    xij_true <- X_true[i, j]
    
    # Compute the current estimate for the selected data point
    WHc_ij <- sum(W[i,] * H[,j]) + c
    xij_hat <- 1 / (1 + exp(-WHc_ij))
    
    # Compute the gradient for the selected data point
    error <- xij_true - xij_hat
    dW <- -error * H[,j]
    dH <- -error * W[i,]
    dc <- -error
    
    # Update W, H, and c
    W[i,] <- W[i,] - learning_rate * dW
    H[,j] <- H[,j] - learning_rate * dH
    c <- c - learning_rate * dc
    
    # Ensure non-negativity
    W[W < 0] <- epsilon
    H[H < 0] <- epsilon
    
  }
  
  # return
  return(list(W = W, H = H, c = c))
  
}

#' Initialization
#' NNDSVD credit to Renaud Gaujoux for R code
#' and C. Boutsidis and E. Gallopoulos for paper and original MATLAB code
#' 
#' @noRd
#' 

initial_values <- function(X, k, m, n, method = "nndsvd", densify = c("base", "average", "random")){
  
  # constant always random
  c <- stats::runif(1) # Global constant approach
  
  # check densify argument, average by default
  if (missing(densify)){
    densify = "average"
  }
  
  # random non-negative values
  if (method == "random"){
    
    W <- matrix(stats::runif(m * k), m, k)
    H <- matrix(stats::runif(k * n), k, n)
    
  }
  
  # SVD initialization
  else if (method == "svd"){
    
    # Perform SVD on X
    s <- svd(X)
    
    # Use the first k singular values and vectors
    U_k <- s$u[, 1:k]
    S_k <- diag(sqrt(s$d[1:k]))
    V_k <- s$v[, 1:k]
    
    # Initialize matrices W and H using absolute values
    W <- abs(U_k %*% S_k)
    H <- abs(t(V_k))
    
  }
  
  # Nonnegative Double Singular Value Decomposition
  else if (method == "nndsvd"){
    
    # Perform SVD on X
    s <- svd(X)
    U <- s$u
    S <- s$d
    V <- s$v
    
    # Initialize matrices W and H
    W <- matrix(0, m, k)
    H <- matrix(0, k, n)
    
    # Handle the first singular triplet, must be nonnegative
    W[,1] = sqrt(S[1]) * abs(U[,1])    
    H[1,] = sqrt(S[1]) * abs(t(V[,1])) 
    
    # second SVD for the other k
    for (i in seq(2, k)){
      
      uu = U[,i]
      vv = V[,i]
      
      uup = pmax(uu, 0)
      uun = pmax(-uu, 0)
      
      vvp = pmax(vv, 0)
      vvn = pmax(-vv, 0)
  
      n_uup = sqrt(drop(crossprod(uup)))
      n_vvp = sqrt(drop(crossprod(vvp)))
      n_uun = sqrt(drop(crossprod(uun)))
      n_vvn = sqrt(drop(crossprod(vvn)))
      
      termp = n_uup * n_vvp
      termn = n_uun * n_vvn
      
      if (termp >= termn){
        
        W[,i] = sqrt(S[i] * termp) * uup / n_uup 
        H[i,] = sqrt(S[i] * termp) * vvp / n_vvp
        
      } else{		
        
        W[,i] = sqrt(S[i] * termn) * uun / n_uun
        
        H[i,] = sqrt(S[i] * termn) * vvn / n_vvn
        
      }
    }
    
    # average variant of NNDSVD
    if (densify == "average"){
      
      average <- mean(X)
      
      W[W == 0] <- average
      H[H == 0] <- average
      
    }
    
    # random fill variant of NNDVSD
    else if (densify == "random"){
      
      n1 <- sum(W == 0)
      n2 <- sum(H == 0)
      
      average <- mean(X)
      
      W[W == 0] <- average * stats::runif(n1, min = 0, max = 1) / 100
      H[H == 0] <- average * stats::runif(n2, min = 0, max = 1) / 100
      
    }
    
  }
  
  # return
  return(list(W = W, H = H, c = c))
  
}
