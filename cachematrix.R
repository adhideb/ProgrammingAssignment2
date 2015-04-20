## Matrix inversion is usually a costly computation 
## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse

## This function creates a special "matrix" object that can cache its inverse
## It is a list containing a function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
## 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the above function
## If the inverse has already been calculated and the matrix has not changed, then
## return the inverse from the cache
## Function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix, ...)
  
  x$setinverse(inv)
  inv
}
