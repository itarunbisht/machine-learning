#normalize x and y before passing it to the function

gd <- function(x,y,lr){
  
  theta0 = c(0)
  theta1 = c(0)
  
  m = 1/length(x)
  cost = c(0.5*m*sum(y^2))
  
  for(i in 2:10){
    
    temp = theta0[i-1]+theta1[i-1]*x-y
    
    theta0[i] = theta0[i-1]-lr*m*sum(temp)
    theta1[i] = theta1[i-1] - lr*m*sum(temp*x)
    
    cost[i] = 0.5*m*sum(((theta0[i]+theta1[i]*x)-y)^2)
    
    print(paste("Iteration: ", i, " theta0: ", theta0[i], " theta1: ", theta1[i], " cost: ", cost[i]))
  }
}
