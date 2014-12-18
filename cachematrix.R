## These Functions Cache a Matrix and its Inverse, 
## Also checkInv calculates the error


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## This functions displays the error of the inverse , it is not needed
## Serves as debugging aid.

checkInv<-function(A,Ainv){
oneway<-A%*%Ainv;
twoway<-Ainv%*%A;
  iden<-diag(nrow(A));
cat('The A*B error is:',sum(iden-oneway),
    '\nThe B*A error is:',sum(iden-twoway))
  
}
