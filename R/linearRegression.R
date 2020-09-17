linreg <- function(formula, data){
  
  X <- model.matrix(formula , data)
  
  y_name <- all.vars(formula)[1]
  y <- data[y_name]
  #print(class(y))
  
  # get the count of independent parametes
  params <- attr(terms(formula),"term.labels")
  params_len <- length(params)
  #print(params_len)
  
  X_transpose <- t(X)
  X_transpose_y <- X_transpose %*% as.matrix(y)
  #print(X_transpose_y)
  X_transpose_X <- X_transpose %*% X
  X_invertion <- solve(X_transpose_X)
  #print(X_invertion)
  beta_hat <- X_invertion %*% X_transpose_y
  #print(beta_hat)
  y_hat <- X %*% beta_hat
  #print(y_hat)
  
  e_hat <- as.matrix(y) - y_hat
  
  degree_freedom <- dim(y)[1] - params_len
  #print(degree_freedom)
  
  residual_variance <- (t(e_hat) %*% e_hat) / degree_freedom
  #print(residual_variance)
  #print(class(X_invertion))
  #print(residual_variance)
  variance_beta_hat <-  drop(residual_variance) * X_invertion 

  t_value_beta <- beta_hat / sqrt(variance_beta_hat)
   
}
