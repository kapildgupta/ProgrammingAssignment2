## The objective is to write a pair of functions to determine the inverse of a 
## matrix, and store the inverse in cache and fetch it to save computational
## time & effort

## The function  below creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invrs) inv <<- invrs
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## In the below function, it is first checked if the inverse of the matrix has
## been computed already. If so, it fetches the inverse without fresh computation.
## If not, it computes the inverse, and then assigns the value in the cache via
## setinv function.


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("fetching Inverse from Cache")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
}
