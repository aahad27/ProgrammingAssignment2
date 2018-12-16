## The first function creates a special "matrix" object that can cache its inverse.
## The second function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

## The first function returns a list of 4 functions.
## set is used to set the matrix, get to return the matrix, setinv to set the
## inverse of the matrix and getinv to return the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<- function(inverse){ 
    inv <<- inverse
  }
  getinv <- function(){
    inv
  }
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## This function first checks if the inverse has already been computated and returns that value 
## with a message. otherwise it solves for the inverse, stores it in the makeCacheMatrix
## function and also returns that value.

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("Displaying cached Data")
    return(inv)
  }
  matrix<- x$get()
  inv<- solve(matrix)
  message("No cached data.")
  x$setinv(inv)
  inv
}
