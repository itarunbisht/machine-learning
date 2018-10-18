#normalize x and y before passing it to the function

library(ISLR)

gd <- function(x,y,lr){
  
  theta0 = c(0) 
  theta1 = c(0)
  
  m = 1/length(x)
  
  
  for(i in 2:20){
    
    temp = (exp(theta0[i-1]+theta1[i-1]*x))/(1+exp(theta0[i-1]+theta1[i-1]*x))
    temp[temp>=0.5]=1
    temp[temp<0.5]=0
    #temp=temp-y
    
    theta0[i] = theta0[i-1]-lr*m*sum(temp-y)
    theta1[i] = theta1[i-1] - lr*m*sum((temp-y)*x)
    
    
    
    print(paste("Iteration: ", i, " theta0: ", theta0[i], " theta1: ", theta1[i]))
 
     }
}

y=Default$default
b=c(0)
for(i in 1:length(y))
{
  if(y[i]=="Yes")
    b[i]=1
else
  b[i]=0
}
x=Default$balance
x=(x-min(x))/(max(x)-min(x))
gd(x,b,0.5)

