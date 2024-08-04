source("loadmnist.R")
library(scatterplot3d)
pca_analysis = function(mat)
{
  cov_mat = cov(mat)
  eigen_mat = eigen(cov_mat)
  eigen_vec = eigen_mat$vectors[,1:2]
  mat.new = t(eigen_vec) %*% t(mat)
  return(mat.new)

}

perceptron <- function(x_inp,y_act,w_vec,data.train.new)
{
   #print(x_inp)
   N = dim(x_inp)[2]
   #print(N)
   
   for(i in 1:10000)
   {
     cat("iteration","\t",i,"\n")
     update = 0
     for(j in 1:N)
     {
       #cat("value of j","\t",j,"\n")
       sig = t(w_vec) %*% x_inp[,j]
       #print(sig)
       y_pred = sign(sig)
       #print(y_pred)
       #print("\n")
       #print(as.vector(y_pred))
       if (y_act[j] != as.vector(y_pred))
       {
         #cat("inside the if loop\n")
         w_vec = w_vec + as.matrix(x_inp[,j] * y_act[j])
         update = 1
         
       } 
     }
     y.new = sign(t(w_vec) %*% t(data.train.new))
     b = y_act == y.new
     err.rate = 1-length(which(b==TRUE))/N
     cat("error rate:",err.rate,"\n")
     if(update == 0)
     {
       cat("inside the break \n")
       break
     }
     #cat("\n") 
   }  
    return(w_vec)
 }

ex1.1 <- function()
{
  N = 5000
  mnistdata = loadmnist(N)

  digit_mat = mnistdata$X
  digit_mat = digit_mat - rowMeans(digit_mat)
  nrow = dim(digit_mat)[1]
  digit.mat = mat.or.vec(5000,784)
  for(i in 1:nrow)
  {
    val = sqrt(var(digit_mat[i,]))  
    if(val !=0)
    {
      digit.mat[i,] = digit_mat[i,]/val
  
    } 
 
  }
  master.zero_ind = which(mnistdata$y==0)
  master.one_ind = which(mnistdata$y==1)
  digit.new = pca_analysis(digit.mat)

  zero_ind = sample(master.zero_ind,2)
  #zero_ind = c(1513,2161)
  one_ind = sample(master.one_ind,2)
  #one_ind = c(3042,471)
  comb_ind = sort(c(zero_ind,one_ind))
  #digit.mat = digit.mat[comb_ind,]

  digit.zero = digit.new[,zero_ind]
  digit.one = digit.new[,one_ind]
  const = rep(1,4)
  x_inp = cbind(digit.zero,digit.one)
  x_inp = rbind(x_inp,const)  
  x_inp = t(x_inp)
  y_act = c(-1,-1,1,1)
  w_vec = as.matrix(c(rep(0,2),1))
  w_new = perceptron(t(x_inp),y_act,w_vec,x_inp)
  #print(w_new)
  x_inp = t(x_inp)
  y_new = t(w_new)%*%x_inp
  y_1 = w_new[1,1]*x_inp[1,]
  y_2 = w_new[2,1]*x_inp[2,]
  #print(y_new)
  #print(y_1)
  #print(y_2)
  
  op = par(mfrow= c(1,2))
  plot(-40:40,-40:40,type="n")
  points(digit.zero[1,],digit.zero[2,],col="red")
  points(digit.one[1,],digit.one[2,],col="blue")
  plot(-40:40,-40:40,type="n")
  points(digit.zero[1,],digit.zero[2,],col="red")
  points(digit.one[1,],digit.one[2,],col="blue")
  points(x_inp[1,],y_1,type="l")
  #points(y_2,x_inp[2,],type="l")
  #points(digit.zero[1,2],y_new[2])
  #scatterplot3d(as.vector(x_inp[1,]),as.vector(x_inp[2,]),as.vector(y_new),main="scatter 3D Plot")
  #tmp = list(x_inp,y_new)
  #return(tmp)
  par(op)

}
preprocess <- function(mnistdata)
{
  digit_mat = mnistdata$X
  digit_mat = digit_mat - rowMeans(digit_mat)
  nrow = dim(digit_mat)[1]
  digit.mat = mat.or.vec(5000,784)
  for(i in 1:nrow)
  {
    val = sqrt(var(digit_mat[i,]))
    if(val !=0)
    {
      digit.mat[i,] = digit_mat[i,]/val

    }

  } 
  return(digit.mat)

}
ex1.2 <- function()
{
  source("loadmnist.R")
  source("visual.R")
  N = 5000
  mnistdata = loadmnist(N)
  digit.prep = preprocess(mnistdata)
  #digit.prep = mnistdata$X
  
  zero_ind = which(mnistdata$y==0)
  one_ind = which(mnistdata$y==1)    
  
  zero_train = zero_ind[1:234]
  one_train = one_ind[1:277]
  comb_train = c(zero_train,one_train)
  
  zero_ind_test = zero_ind[235:479]
  one_ind_test = one_ind[278:563]
  comb_test = c(zero_ind_test,one_ind_test)
  
  data.train = digit.prep[comb_train,]
  data.test = digit.prep[comb_test,]
  const.train = rep(1,dim(data.train)[1])
  const.test = rep(1,dim(data.test)[1])
  data.train.new = cbind(data.train,const.train)
  data.test.new = cbind(data.test,const.test)
  y_act = c(rep(-1,length(zero_train)),rep(1,length(one_train)))
  w_vec = as.matrix(c(rep(0,784),3000))
  
  w_new = perceptron(t(data.train.new),y_act,w_vec,data.train.new)
  w = w_new[1:784,]
  visual(t(w))
  y_new = sign(t(w_new) %*% t(data.test.new))
  y_act_test = c(rep(-1,length(zero_ind_test)),rep(1,length(one_ind_test)))
  b = y_act_test==y_new
  N = dim(data.test.new)[1]
  error.rate = 1-length(which(b==TRUE))/N
  cat("Final Misclassification error",error.rate,"\n")
  
  #return(y_new)
}


