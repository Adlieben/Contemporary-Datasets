# The aim is to reproduce the combinations that give you one score 
# Initialise a data frame
Sys.setenv(LANG = "en")
j <- 10 # number of items
results <- data.frame(matrix(nrow = j + 1, ncol = j + 1))
results[1,] <- rep(1,j+1) # Initialise rows, only 1s for row 0
results[2:(j+1),1] <- rep(0,j) # Initialise first column
set.seed(3000)
Beta <- rnorm(j, 0, 1)
bi <- exp(-Beta) 
# bi <- rep(1, j) # To check this works, normally it should line up with the choose function below

# Computing the results matrix
for (i in (1:j)){ # Columns
  for (k in (1:j)){ # Rows
  results[k+1, i+1] <- bi[i] * results[k, i] + results[k+1, i] # Exactly as in table
  }
}

results[,i+1] # Final results

# CHECK: For the verification, we compare to choose
chosen <- rep(NA, j+1)
for (i in 0:j){
  chosen[i+1] <- choose(j, i)
}
chosen # All is good
