# perceptron training ANN

# function to determine sign
sgn <- function(x){
    temp = ifelse(x>0 ,1 , -1)
    return(temp)
}


# main script for expression a & b
# change expressions as required on line no 27 

a = c(0,0,1,1)
b = c(0,1,0,1)
w = c(0,0,0)
x = c()

repeat{

  change = FALSE
  
  for(i in 1:4){
    x[1] = 1
    x[2] = a[i]
    x[3] = b[i]
    
    t = sgn(x[2] & x[3])
    o = sgn(w[1]*x[1] + w[2]*x[2] + w[3]*x[3])
    
    # if signs are equal, just repeat in the loop
    # else update the weights and start loop from
    # the beginning again
    
    if(t != o){
      for(i in 1:3){
        w[i] <- w[i] + 0.5*(t - o)*x[i]
      }
      change = TRUE
    }
  }
  
  if(!change) break
}

print(w)

