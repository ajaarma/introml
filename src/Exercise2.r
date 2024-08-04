library(MASS)

# Function to compute the coefficient matrix and error.
polynomial_func <- function(input,data)
  {
     ones_train = as.matrix(rep(1,30))
         
     x_train = as.matrix(data$x[1:30])
     y_train = as.matrix(data$y[1:30])
     
              
     y_valid_pred = A = W = c()
         
     A = poly_order(input,x_train,ones_train)
     A.inv = ginv(A)
     W = t(y_train) %*% t(A.inv)
     y_valid_pred = A %*% t(W)
     E = mean((y_train - y_valid_pred)^2)
     tmp = list(E,W)
     return(tmp)
       
   }
 
# Function to compute the sum of squared error of K cross validation and produce the corresponding plot.
poly_func_new <- function(data)
   {
    
    Err = c()
    for(k in 0:10)
    { 
      input = k
      ones_train = as.matrix(rep(1,27))
      ones_valid = as.matrix(rep(1,3))
      error = c()
      for(j in 1:10)
      {
        x_train = as.matrix(data[data_new[,1]!=j,2])
        x_valid = as.matrix(data[data_new[,1]==j,2])
     
        y_train = as.matrix(data[data_new[,1]!=j,3])
        y_valid = as.matrix(data[data_new[,1]==j,3])
     
        x_valid_mat = y_valid_pred = A=W=c()
     
        A = poly_order(input,x_train,ones_train)
        #print(dim(A))
        A.inv = ginv(A)
        #print(dim(A.inv))
        W = t(y_train) %*% t(A.inv)
        x_valid_mat = poly_order(input,x_valid,ones_valid)
        y_valid_pred = x_valid_mat %*% t(W)
        E = sum((y_valid - y_valid_pred)^2)
        error = c(error,E)
        #print(E)
        #tmp = list(E,W)
        
       }
       err_sum = sum(error)
       #print(err_sum)
       Err = c(Err,err_sum)
     }   
     #close.screen(all=TRUE)
     print(Err)
     plot(0:10,Err,col="red")
     smoothingSpline = smooth.spline(0:10, Err,spar=0.2,cv=TRUE)
     lines(smoothingSpline,col="blue")
   }
   
 # Function to generate the expression for polynomial of given order K.
poly_order = function(input,x_data,ones_data)
  {
     if(input==0)
     {
       A = cbind(ones_data)
       return(A)
       break
     }
     if(input==1)
     {
       
       A = cbind(ones_data,x_data)
       return(A)
       break
     }
     if(input == 2)
     {
       A = cbind(ones_data,x_data,x_data^2)
       return(A)
       break
     }
     if(input == 3)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3)
       return(A)
       break
     }
     if(input == 4)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4)
       return(A)
       break
     }
     if(input == 5)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4,x_data^5)
       return(A)
       break
     }
     if(input == 6)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4,x_data^5,x_data^6)
       return(A)
       break
     }
     if(input == 7)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4,x_data^5,x_data^6,x_data^7)
       return(A)
       break
     }
     if(input == 8)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4,x_data^5,x_data^6,x_data^7,x_data^8)
       return(A)
       break
     }
     if(input == 9)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4,x_data^5,x_data^6,x_data^7,x_data^8,x_data^9)
       return(A)
       break
     }
     if(input == 10)
     {
       A = cbind(ones_data,x_data,x_data^2,x_data^3,x_data^4,x_data^5,x_data^6,x_data^7,x_data^8,x_data^9,x_data^10)
       return(A)
       break
     }
  }
 #Function used to plot the fitted polynomial of given order k
 poly_plot <- function(k,data)
 {
   colour = c(1:11)
   R = polynomial_func(k,data)
   error = R[[1]]
   #print(error)    
   w = R[[2]]
   A = poly_order(k,x,rep(1,30))
   y_pred = as.matrix(A %*% t(w))
   smoothingSpline = smooth.spline(x, y_pred,spar=0.2,cv=TRUE)   
   plot(data$x,data$y,col="red")
   par(new=TRUE)
   lines(smoothingSpline,col=colour[k+1])
   #close.screen(all=TRUE)
   #tmp = sum((y_pred - mean(y_pred))^2)/sum((y-mean(y))^2)
   
 }
 
 #Function to calculate the coefficient of determination R^2
 poly_coeff <- function(data)
 {
   coeff = c()
   for(k in 0:10)
   {
     R = polynomial_func(k,data)
     error = R[[1]]          
     w = R[[2]]
     A = poly_order(k,x,rep(1,30))
     y_pred = as.matrix(A %*% t(w))
     tmp = sum((y_pred - mean(y_pred))^2)/sum((y-mean(y))^2)
     coeff = c(coeff,tmp)
     
   }
   return(coeff)
 }

x = runif(30,-3,3)
f = 2+x-.5*x^2
n = rnorm(30,0,0.4)
y = f+n
data = data.frame(x,y)

#plot(x,y,col="red")
#par(new=TRUE)

b = c()

for(i in 1:10)
{
  tmp = c(rep(i,3))
  b = c(b,tmp)

}

data_new = data.frame(b,data)
data_list = split(data_new,data_new$b)
close.screen(all=TRUE)





