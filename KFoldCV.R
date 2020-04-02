## install package if it is not already.
if(!require("data.table")){
  install.packages("data.table")
}

## attach all functions provided by these packages.
library(data.table)
library(ggplot2)

## download spam data set to local directory, if it is not present.
if(!file.exists("spam.data")){
  download.file("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/spam.data", "spam.data")
}

## Read spam data set and conver to X matrix and y vector we need for
## gradient descent.
spam.dt <- data.table::fread("spam.data")
N.obs <- nrow(spam.dt)
X.raw <- as.matrix(spam.dt[, -ncol(spam.dt), with=FALSE]) 
y.vec <- spam.dt[[ncol(spam.dt)]]
X.sc <- scale(X.raw) #scaled X/feature/input matrix.

n.folds <- 5
max.neighbors <- 20
X.new.test <- matrix(0, nrow(X.sc), ncol(X.sc))
X.new.test <- apply(X.new.test, c(1, 2), function(x) sample(c(0, 1), 1))

test.list <- list()
test.list <- NearestNeighborsCV(X.sc , y.vec , X.new.test ,n.folds , max.neighbors )


KFoldCV <- function(X_mat, y_vec, ComputePredictions, fold_vec){
  k <- max(fold_vec)  
  error_vec <- rep(0, k)
    
    for (folds in 1:k){
    is.test <- fold_vec == folds
    is.train <- !is.test
    X.new <- X_mat[is.test, ]
    y.new <- y_vec[is.test]
    X.train <- X_mat[is.train, ]
    y.train <- y_vec[is.train]
    pred.new <- ComputePredictions(X.train,X.new,y.train)
    zero.loss <- pred.new != y.new
    mean.err <- mean(zero.loss)    
    error_vec[folds] = mean.err
  }

  return(error_vec)
}


NearestNeighborsCV <- function(X_mat, y_vec, X_new, num_folds, max_neighbors){
    set.seed(10)
    validation_fold_vec <- sample(rep(1:num_folds, l = nrow(X_mat)))
    error_mat = matrix(0, num_folds, max_neighbors)
    mean_error_vec <- c(1:max_neighbors)
    for (num_neighbors in 1:max_neighbors){
	     #figure class::knn()
         error_mat[, num_neighbors] = KFoldCV(X_mat, 
                                              y_vec, 
                                              function(X_mat, X_new, y_vec){class::knn(X_mat, X_new, y_vec, k=num_neighbors )}, 
                                              validation_fold_vec)
    }
    
    mean_error_vec <- colMeans(error_mat)
    best_neighbors <- which.min(mean_error_vec)
    
    metric.list <- list()
    metric.list$min.pred.vec <- class::knn(X_mat , X_new ,y_vec , k= best_neighbors)
    metric.list$mean_err_vec_val <- mean_error_vec
    metric.list$err_mat <- error_mat
    
    return(metric.list)
}




 ## for loop to place a dt of perc err of neighbor by fold 
error.dt.list <- list()
for( fold in 1 : nrow(test.list$err_mat)){
  error.dt.list[[fold]] <- data.table( error = test.list$err_mat[fold,] ,
                                      neighbors = 1:ncol(test.list$err_mat),
                                      folds = fold)
}

err.dt <- do.call( rbind , error.dt.list)

## data frame for mean error of folds by num of neighbors
mean.err.dt <- data.table(  mean_error = as.numeric(test.list$mean_err_vec_val),
                            neighbors = 1:length(test.list$mean_err_vec_val)) 

min.dt <- mean.err.dt[ which.min(mean_error)]

ggplot()+
  geom_line( aes( x = neighbors ,
                  y = error ,
                  color = folds, 
                  group = folds),
             data = err.dt) +
  geom_line( aes( x = neighbors ,
                  y = mean_error ),
                  color = "red"  , 
                  data = mean.err.dt ,
                  size=  2) +
  geom_point( aes( x= neighbors ,
                   y= mean_error) ,
              color = "black",
              size = 1.5 ,
              data = min.dt)


## test_fold_vec  - one element for each observations and elemnts are integers 1 -4
n.folds <- 4
fold.vec <- sample(rep(1:n.folds, l=length(y.vec)))

test.fold <- 1 
is.test <- fold.vec == test.fold
test_fold_vec <- X.sc[is.test ,]








