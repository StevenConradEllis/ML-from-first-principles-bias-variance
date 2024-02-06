rm(list=ls())

# Set up a target function/pattern:
f = function(x){sin(pi*x)}

# Write a function that generates a data set of size N 
# with pattern f. Return a list of predictors (x) and responses (y).
dgp = function(N,f,sig2)
{
   x = runif(N,-1,1)
   e = sqrt(sig2)*rnorm(N,0,1) # this represents 'noise'
   y = f(x)+e
   return(list(x = x , y = y))
}

# Plot a single realization of the data and overlay the target function:
res_data = dgp(2,f,0.1)
plot(res_data, pch = 16, cex = 2, col = 'blue',xlim = c(-1,1), ylim = c(-1,1), xlab = "x", ylab = "y")
xx = seq(-1,1,1/100)
lines(f(xx)~xx, col = 'black')


# Calculate the (squared) bias for an appropriate H. 
# Use M = 600 draws of the data and a step-size of dx = 1/100 for 
# the left-(end)point approximation to the relevant integral.

bias_var= function(N = 2, order = 1, sig2 = 0, M  = 600, dx = 1/100)
{
 x_lat = seq(-1,1,dx) # vector if values from -1 to 1, in 0.01 increments
 N_dx  = length(x_lat) # 200 
 g_bar = rep(0, N_dx) # vector of 200 0 values
 G_D   = matrix(0,M,N_dx) # matrix of 600 rows and 200 cols
 errors = rep(0,M) # vector of 200 zeros
 
 plot(res_data, pch = 16, type = 'n', cex = 2, 
      col = 'blue',xlim = c(-1,1), ylim = c(-2,2), xlab = "x", ylab = "y")
 xx = seq(-1,1,1/100)
 lines(f(xx)~xx, col = 'black', lwd = 4) # overlay the target function/pattern on the plot 
 
 for(i in 1:M) # 1 to 5000
 {
  res_data  = dgp(N,f,sig2) # return dataset of size N with pattern F
  if(order == 0) {
    res_model = lm(y ~ y, data = res_data) # constant hypothesis  
  } else {
    res_model = lm(y ~ poly(x, order), data = res_data) # linear model  
  }
  
  # generate the candidate hypothesis using the 'predict' function
  g_D       = predict(res_model, data.frame(x = x_lat)) 
  g_bar     = g_bar + g_D
  
  G_D[i,]   = g_D
  dat_oos   = dgp(N,f,sig2) # generate an out-of-sample data set with pattern f
  
  # calculate the out-of-sample error of the model
  yhat_oos  = predict(res_model, data.frame(x = dat_oos$x))
  errors[i] = mean((yhat_oos-dat_oos$y)^2)
  
  # plot the calculated x-y coordinates of the candidate hypothesis
  lines(g_D~x_lat, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.1), lwd = 1)

 }
 
 # this is the value of the 'average function'
 g_bar = g_bar/M
 
 # this is the average function's error
 test_error = mean(errors)
 
 # plot the average function in red
 lines(g_bar ~x_lat, col = "red", lwd = 3)
 
 phi_X = 0.5
 print(g_bar-f(x_lat))
 # Calculate the bias of the model
 bias2 = sum((g_bar - f(x_lat))[-N_dx]^2*phi_X*dx)
 
# Calculate the variance of the model
# This integration involves two steps.
 ones = matrix(1,M,1)
 var_at_x = colSums((G_D-ones%*%g_bar)^2)/M # Step 1
 var  = sum(var_at_x[-N_dx]*phi_X*dx)
 
 
  # Calculate the expected Out-of-Sample error (over all D).
  # Use sample estimates for M simulations. 
  # Call it Test_Error
  
  # Now, wrap all of this in a function, bias_var, that returns  
  # bias2, var, bias2 + var, and Test_Error, for particular N

  ret = list(bias2= bias2, var = var, both = bias2+var,test_error=test_error)
  return(ret)
}

bias_var(N = 2,order = 0,sig2 = 0.05)
bias_var(N = 2,order = 1,sig2 = 0.05)

bias_var(N = 5,order = 0,sig2 = 0.05)
bias_var(N = 5,order = 1,sig2 = 0.05)
bias_var(N = 5,order = 3,sig2 = 0.05)

bias_var(N = 10,order = 0,sig2 = 0.05)
bias_var(N = 10,order = 1,sig2 = 0.05)
bias_var(N = 10,order = 3,sig2 = 0.05)

bias_var(N = 20,order = 0,sig2 = 0.05)
bias_var(N = 20,order = 1,sig2 = 0.05)
bias_var(N = 20,order = 3,sig2 = 0.05)
 