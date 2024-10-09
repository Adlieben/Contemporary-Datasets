# Set language
Sys.setenv(LANG = "en")

# Function to estimate item parameters using Newton-Raphson method
est_rasch_newton_raphson <- function(data, tol = 1e-4, max_iter = 100) {
  
  # Step 2: Initialize item difficulty parameters (starting at 0 for simplicity)
  n_items <- ncol(data)
  beta <- rep(0, n_items)
  
  # Step 3: Define the conditional log-likelihood function using sufficient statistics
  conditional_log_likelihood <- function(beta) {
    n_persons <- nrow(data)
    log_likelihood <- 0
    
    for (j in 1:n_persons) {
      # Total score for person j
      r_j <- sum(data[j, ])
      
      # If total score is 0 or max (all correct or all incorrect), skip
      if (r_j == 0 || r_j == n_items) next
      
      # Calculate the conditional probability for each person's response pattern
      log_prob_sum <- sum(data[j, ] * beta - log(1 + exp(beta)))
      
      log_likelihood <- log_likelihood + log_prob_sum
    }
    
    return(-log_likelihood)  # Return negative log-likelihood for minimization
  }
  
  # Step 4: Use nlm to minimize the conditional log-likelihood
  result <- nlm(f = conditional_log_likelihood, p = beta)
  
  # Return the estimated beta parameters
  return(result$estimate)
  
  # # Step 3: Newton-Raphson loop
  # for (iter in 1:max_iter) {
  #   
  #   # Initialize gradient and Hessian
  #   gradient <- rep(0, n_items)
  #   hessian <- matrix(0, n_items, n_items)
  #   
  #   # Step 3a: Compute gradient and Hessian
  #   for (j in 1:nrow(data)) {
  #     r_j <- sum(data[j, ])
  #     
  #     if (r_j == 0 || r_j == n_items) next  # Skip people with total score 0 or full marks
  #     
  #     # Probability of correct response given current beta
  #     p <- exp(beta) / sum(exp(beta))
  #     
  #     # Update gradient for each item
  #     for (i in 1:n_items) {
  #       gradient[i] <- gradient[i] + data[j, i] - p[i]
  #     }
  #     
  #     # Update Hessian for each item pair (i, j)
  #     for (i in 1:n_items) {
  #       for (k in 1:n_items) {
  #         hessian[i, k] <- hessian[i, k] - p[i] * p[k]
  #       }
  #     }
  #   }
  #   
  #   # Add the contribution of the sufficient statistics to the gradient
  #   gradient <- gradient + item_sum
  #   
  #   # Step 3b: Update beta using the Newton-Raphson update formula
  #   beta_update <- solve(hessian) %*% gradient
  #   beta <- beta - beta_update
  #   
  #   # Step 3c: Check for convergence (change in beta is small)
  #   if (max(abs(beta_update)) < tol) {
  #     cat("Converged after", iter, "iterations.\n")
  #     break
  #   }
  # }
  # 
  # # Step 4: Return the estimated beta parameters
  # return(beta)
}

# Example usage
# Simulate some binary response data (persons x items)
set.seed(123)
data <- matrix(rbinom(100, 1, 0.5), nrow = 10, ncol = 10)

# Estimate item parameters using Newton-Raphson method
beta_estimates <- est_rasch_newton_raphson(data)
print(beta_estimates)
