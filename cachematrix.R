## Programming Assignment 2. Juan Manuel Ramírez (JuaneleTamal)
## makecacheMatrix and cacheSolve are two functions that allow to save time in calculations,
## in this case, for the calculation of the inverse of a matrix

## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x <<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function()m
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve will first check if the inverse matrix has been already calculated,
## i.e. it's in the cache, to skip the calculation. If it's not, it will calculate it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}

