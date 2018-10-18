# for multi variable
# normalize variables before passing as an argument

gd <- function(x1,x2,x3,y,lr){

  m = length(y)
  
  theta0 = c(0)
  theta1 = c(0)
  theta2 = c(0)
  theta3 = c(0)
  cost = c(0.5*m*sum(-y^2))
  
  for(i in 2:10){
    
    x = theta0[i-1] + theta1[i-1]*x1 + theta2[i-1]*x2 + theta3[i-1]*x3 - y
    
    theta0[i] = theta0[i-1] - lr*(1/m)*sum(x)
    theta1[i] = theta1[i-1] - lr*(1/m)*sum(x*x1)
    theta2[i] = theta2[i-1] - lr*(1/m)*sum(x*x2)
    theta3[i] = theta3[i-1] - lr*(1/m)*sum(x*x3)
    
    htheta_new = theta0[i] + theta1[i]*x1 + theta2[i]*x2 + theta3[i]*x3
    cost[i] = 0.5*m*sum((htheta_new - y)^2)

    print(paste("Iteration: ", i, " theta0: ", theta0[i], " theta1: ", theta1[i], " theta2: ", theta2[i], " theta3: ", theta3[i]))
    
  }
  
}

