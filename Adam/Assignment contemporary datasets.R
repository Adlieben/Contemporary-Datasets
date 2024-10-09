# Function to estimate item parameters in Rasch model using CMLE and nlm()
est_rasch_cml_nlm <- function(data) {
  
  # Step 1: Compute sufficient statistics (sum of correct answers for each item)
  item_sum <- colSums(data)
  
  # Step 2: Initialize item difficulty parameters (starting at 0 for simplicity)
  n_items <- ncol(data)
  beta <- rep(0, n_items)
  
  # Step 3: Define the conditional log-likelihood function
  conditional_log_likelihood <- function(beta) {
    n_persons <- nrow(data)
    log_likelihood <- 0
    
    for (j in 1:n_persons) {
      # Total score for person j
      r_j <- sum(data[j, ])
      
      # If total score is 0 or max (all correct or all incorrect), skip
      if (r_j == 0 || r_j == n_items) next
      
      # Calculate the sum over all items for person j
      log_prob_sum <- sum(data[j, ] * beta - log(1 + exp(beta)))
      
      log_likelihood <- log_likelihood + log_prob_sum
    }
    
    return(-log_likelihood)  # Return negative log-likelihood for minimization
  }
  
  # Step 4: Use nlm to minimize the conditional log-likelihood
  result <- nlm(f = conditional_log_likelihood, p = beta)
  
  # Return the estimated beta parameters
  return(result$estimate)
}

# Example usage
# Simulate some binary response data (persons x items)
set.seed(123)
data <- matrix(rbinom(100, 1, 0.5), nrow = 10, ncol = 10)

# Estimate item parameters using CMLE and nlm
beta_estimates <- est_rasch_cml_nlm(data)
print(beta_estimates)
