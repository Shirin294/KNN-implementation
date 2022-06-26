#### KNN algorithm implementation without using library 
## generate data sample -----------
# underlying stochastic process 
f<- function(x) tan(0.2*pi*x)
x<-seq(0,2,length.out = 1000)
y<- f(x)+rnorm(1000,0,0.5)
df<-data.frame(x,y)
head(df)

# plot y vs. x
plot(df$x,df$y)
lines(df$x,f(df$x),col="red",lwd=2);grid()
# Histogram of x
hist(df$x,breaks = 20)

#Histogram of y
hist(df$y, breaks = 20)
# splitting the Data into train and test
train_data_index<-sample(1:nrow(df),0.8*nrow(df))
head(train_data_index)
train_data<-df[train_data_index,]
#ordering for drawing plot
train_data<-train_data[order(train_data$x,decreasing = F),]
head(train_data)
dim(train_data)
plot(train_data$x,train_data$y)
#test
test_data<-df[-train_data_index,]
#ordering for drawing plot
test_data<-test_data[order(test_data$x,decreasing = F),]
dim(test_data)
head(test_data)
plot(test_data$x,test_data$y)
## implementation of KNN algorithm 
KNN_f<-function(x,y,k)
{
  if(!is.matrix(x))
  {
    x<-as.matrix(x)
  }
  if(!is.matrix(y))
  {
    y<-as.matrix(y)
  }
  KNN<-list()
  KNN[["features"]]<-x
  KNN[["response"]]<-y
  KNN[["k"]]<- k
  return(KNN)
}
###calculating pairwise Distance matrix

pdist<-function(A,B)
{
  
  nA<-nrow(A)
  nB<-nrow(B)
  
  a<-apply(A, 1, function(m) t(m)%*% m)
  b<-apply(B, 1, function(m) t(m)%*% m)
  sqrt(
        matrix(a, nA, 1)%*% matrix(rep(1,nB),1,nB) +
        matrix(rep(1,nA),nA,1) %*% matrix(b, 1, nB) -
        2 * A %*% t(B)  
      )
}
### KNN predictor
 KNN_f_predict <- function(KNN, new_data_x)
 {
   if(!is.matrix(new_data_x))
   {
     
     new_data_x<- as.matrix(new_data_x)
   }
   #calculating distances
    
   dist_mat <- pdist(KNN[["features"]],new_data_x)
   t( apply(dist_mat, 2, rank)<=KNN[["k"]]) %*% KNN[["response"]] / KNN[["k"]]
   
 }

 
### prediction on training  data set------------
 # k=3
 train_data$predict_k3<-as.vector(KNN_f_predict(KNN_f(train_data$x, train_data$y, 3), train_data$x))
head(train_data)
plot(train_data$x,train_data$y)

lines(train_data$x, train_data$predict_k3,col="red",lwd=2)

#k=10
 
train_data$predict_k3<-as.vector(KNN_f_predict(KNN_f(train_data$x, train_data$y, 10), train_data$x))
head(train_data)
plot(train_data$x,train_data$y)

lines(train_data$x, train_data$predict_k3,col="red",lwd=2)
#k=25

train_data$predict_k3<-as.vector(KNN_f_predict(KNN_f(train_data$x, train_data$y, 25), train_data$x))
head(train_data)
plot(train_data$x,train_data$y)

lines(train_data$x, train_data$predict_k3,col="red",lwd=2)
# k=50
train_data$predict_k3<-as.vector(KNN_f_predict(KNN_f(train_data$x, train_data$y, 50), train_data$x))
head(train_data)
plot(train_data$x,train_data$y)

lines(train_data$x, train_data$predict_k3,col="red",lwd=2)
# k=100
train_data$predict_k3<-as.vector(KNN_f_predict(KNN_f(train_data$x, train_data$y, 100), train_data$x))
head(train_data)
plot(train_data$x,train_data$y)

lines(train_data$x, train_data$predict_k3,col="red",lwd=2)
## tuning k via K-fold validation

grids <-c(3,10,25,50,100)
grids

#10-fold validation
K_fold <- 10
train_data$fold <- sample(1:K_fold, nrow(train_data), replace = T)
head(train_data)
train_data

#RMSE matrix

rmse_mat <- matrix(NA, K_fold, length(grids), dimnames = list(NULL,paste(grids)))
rmse_mat

for (i in 1:length(grids)) {
  for (j in 1:K_fold) {
    calculated <- as.vector(
      KNN_f_predict(
        KNN_f(train_data$x[train_data$fold !=j] , train_data$y[train_data$fold !=j], grids[i]
              )
        , train_data$x[train_data$fold ==j]
      )
    )
  }
  rmse_mat[i ,j]<-round(mean((train_data$y[train_data$fold == j] - calculated) ^ 2), 3)
}










