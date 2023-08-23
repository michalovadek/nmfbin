#' Sigmoid function
#'
#' @param z A numeric value or vector.
#' @return The sigmoid of z.
#' @noRd
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

#' Binary cross-entropy loss function
#'
#' @param p Predicted probabilities.
#' @param y Actual labels (0 or 1).
#' @return The binary cross-entropy loss.
#' @noRd
binary_crossentropy <- function(p, y) {
  -sum(y * log(p) + (1 - y) * log(1 - p))
}

#' Gradient of binary cross-entropy with respect to weight w
#'
#' @param x Input features.
#' @param y Actual labels (0 or 1).
#' @param w Weight.
#' @return The gradient of the loss with respect to w.
#' @noRd
gradient <- function(x, y, w) {
  p <- sigmoid(w * x)
  sum(x * (p - y))
}

#' Gradient Descent for minimizing binary cross-entropy
#'
#' @param x Input features.
#' @param y Actual labels (0 or 1).
#' @param starting_point Initial weight value.
#' @param learning_rate Learning rate for gradient descent.
#' @param n_iterations Number of iterations for the gradient descent.
#' @return Estimated weight after gradient descent.
#' @noRd
gradient_descent <- function(x, y, starting_point, learning_rate, n_iterations) {
  w <- starting_point
  for (i in 1:n_iterations) {
    grad <- gradient(x, y, w)
    w <- w - learning_rate * grad
    
    # Print progress
    p <- sigmoid(w * x)
    loss <- binary_crossentropy(p, y)
    cat(sprintf("Iteration %d: w = %f, Loss = %f\n", i, w, loss))
  }
  return(w)
}
