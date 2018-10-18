y = c(12,19,29,37,45)
x = c(2005, 2006,2007,2008,2009)

fun1 = function(x_new, y_new){
  p = (x_new - mean(x_new))*(y_new - mean(y_new))
  q = (x_new - mean(x_new))^2
  p = sum(p)
  q = sum(q)
  
  theta1 = p/q
  theta0 = mean(y_new) - theta1*mean(x_new)
  return(c(theta0, theta1))
}

repeat{
  print("Choose a normalization method or exit menu: ")
  
  print("1: (x-xmin)/(xmax-xmin)")

  print("2: (x-xmean)/(xmax-xmin)")
  
  print("3: (x-xmean)/(Standard Deviation)")
  
  print("4: exit")
  
  choice = as.numeric(readline("Enter choice: "))
 
  if(choice == 1){
    estimate = as.numeric(readline("Enter year to obtain estimated sales: "))
    
    x_new = (x-min(x))/(max(x)-min(x))
    y_new = (y-min(y))/(max(y)-min(y))
    
    theta = fun1(x_new,y_new)
    
    estimate = (estimate-min(x))/(max(x)-min(x))
    estimate_y = theta[1] + theta[2]*estimate
    
    estimated_sales = (max(y)-min(y))*estimate_y + min(y)
    print(estimated_sales)
  }
  else if(choice == 2){
    estimate = as.numeric(readline("Enter year to obtain estimated sales: "))
    
    x_new = (x-mean(x))/(max(x)-min(x))
    y_new = (y-mean(y))/(max(y)-min(y))
    
    theta = fun1(x_new, y_new)
    
    estimate = (estimate-mean(x))/(max(x)-min(x))
    estimate_y = theta[1] + theta[2]*estimate
    
    estimated_sales = (max(y)-min(y))*estimate_y + mean(y)
    print(estimated_sales)
    
  }
  else if(choice == 3){
    estimate = as.numeric(readline("Enter year to obtain estimated sales: "))
    
    x_new = (x-mean(x))/sd(x)
    y_new = (y-mean(y))/sd(y)
    
    theta = fun1(x_new, y_new)
    
    estimate = (estimate-mean(x))/sd(x)
    estimate_y = theta[1] + theta[2]*estimate
    
    estimated_sales = sd(y)*estimate_y + mean(y)
    print(estimated_sales)
    
  }
  else if(choice== 4){
    break;
  }
  else{
    print("Invalid choice, try again with a valid one")
  }
}