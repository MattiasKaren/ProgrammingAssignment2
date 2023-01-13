## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Creates a special matrix object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## Setting the matrix: 
  set <- function(matrix)
    x <<- matrix
    inv <<- NULL
  
    
  
  ## Get the matrix: 
  get <- function() x
  
  ## Setting the inverse of the matrix: 
  setInverse <- function(inverse) inv <<- inverse
  
  ## Getting the inverse of the matrix: 
  getInverse <- function()
    inv
  
  ##Return a list of the methods: 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describ ng this function

## This function computes the inverse of the specialmatrix returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
