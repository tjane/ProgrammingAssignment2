## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing
## repeatedly (there are also alternatives to matrix inversion that not 
## discuss here).This program aims to write a pair of functions that cache
## cache the inverse of a matrix.

## There are two functions in this program.
## function one : makeCacheMatrix
## function two : cacheSolve

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above.
## If the inverse has already been calculated(and the matrix has not changed), then "cacheSolve" should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  I = matrix(rep(1,nrow(x)))
  inv <- solve(data,I,...)
  x$setinverse(inv)
  inv
}
