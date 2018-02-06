foo <- function(arg1, arg2) {
  
  return(final_stuff)
}

b_ols1 <- function(data, y, X) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Create the y matrix
  y_data <- data %>%
    # Select y variable data from 'data'
    select_(.dots = y) %>%
    # Convert y_data to matrices
    as.matrix()
  
  # Create the X matrix
  X_data <- data %>%
    # Select X variable data from 'data'
    select_(.dots = X) %>%
    # Add a column of ones to X_data
    mutate_("ones" = 1) %>%
    # Move the intercept column to the front
    select_("ones", .dots = X) %>%
    # X_data <- cbind(1, X_data)
    # colnames(X_data)[1] <- "ones"
    # Convert X_data to matrices
    as.matrix()
  
  # Calculate beta hat
  beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
  # Change the name of 'ones' to 'intercept'
  rownames(beta_hat) <- c("intercept", X)
  # Return beta_hat
  return(beta_hat)
}

b_ols <- function(data, y, X) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Create the y matrix
  y_data <- data %>%
    # Select y variable data from 'data'
    select_(.dots = y) %>%
    # Convert y_data to matrices
    as.matrix()
  
  # Create the X matrix
  X_data <- data %>%
    # Select X variable data from 'data'
    select_(.dots = X) %>%
    # Add a column of ones to X_data
    mutate_("ones" = 1) %>%
    # Move the intercept column to the front
    select_("ones", .dots = X) %>%
    # Convert X_data to matrices
    as.matrix()
  
  # Calculate beta hat
  beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
  # Change the name of 'ones' to 'intercept'
  rownames(beta_hat) <- c("intercept", X)
  # Return beta_hat
  return(beta_hat)
}


b_ols(cars, y="price", X=c("mpg", "weight"))


# library(pacman); p_load(lfe)

#solve inverts matrices, t() transposes vector




b_ols_noint <- function(data, y, X) {
  # Require the 'dplyr' package
  require(dplyr)
  
  # Create the y matrix
  y_data <- data %>%
    # Select y variable data from 'data'
    select_(.dots = y) %>%
    # Convert y_data to matrices
    as.matrix()
  
  # Create the X matrix
  X_data <- data %>%
    # Select X variable data from 'data'
    select_(.dots = X) %>%
    # Convert X_data to matrices
    as.matrix()
  
  # Calculate beta hat
  beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
  # Change the name of 'ones' to 'intercept'
  rownames(beta_hat) <- c(X)
  # Return beta_hat
  return(beta_hat)
}

b_ols_noint(data= cars, y="price", X=c("mpg", "weight"))





# 
# 
# b_ols2 <- function(data, y, X) {
#   # Require the 'dplyr' package
#   require(dplyr)
#   
#   # Select y variable data from 'data'
#   y_data <- select_(data, .dots = y)
#   # Convert y_data to matrices
#   y_data <- as.matrix(y_data)
#   
#   # Select X variable data from 'data'
#   X_data <- select_(data, .dots = X)
#   # Add a column of ones to X_data
#   # X_data <- mutate_(X_data, "ones" = 1)
#   # Move the intercept column to the front
#   # X_data <- select_(X_data, "ones", .dots = X)
#   # Convert X_data to matrices
#   X_data <- as.matrix(X_data)
#   # Add a column of ones to front
#   X_data <- cbind(1, X_data)
#   colnames(X_data)[1] <- "ones"
#   
#   # Calculate beta hat
#   beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
#   # Change the name of 'ones' to 'intercept'
#   rownames(beta_hat) <- c("intercept", X)
#   # Return beta_hat
#   return(beta_hat)
# }
# 
# 
# b_ols1 <- function(data, y, X) {
#   # Require the 'dplyr' package
#   require(dplyr)
#   
#   # Create the y matrix
#   y_data <- data %>%
#     # Select y variable data from 'data'
#     select_(.dots = y) %>%
#     # Convert y_data to matrices
#     as.matrix()
#   
#   # Create the X matrix
#   X_data <- data %>%
#     # Select X variable data from 'data'
#     select_(.dots = X) %>%
#     # Add a column of ones to X_data
#     mutate_("ones" = 1) %>%
#     # Move the intercept column to the front
#     select_("ones", .dots = X) %>%
#     # Convert X_data to matrices
#     as.matrix()
#   
#   # Calculate beta hat
#   beta_hat <- solve(t(X_data) %*% X_data) %*% t(X_data) %*% y_data
#   # Change the name of 'ones' to 'intercept'
#   rownames(beta_hat) <- c(X, "intercept")
#   # Return beta_hat
#   return(beta_hat)
# }
# 
# b_ols1(cars, y="price", X=c("mpg", "weight"))
# 
# b_ols2(cars, y="price", X=c("mpg", "weight"))

