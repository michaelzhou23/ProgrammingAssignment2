## makeCacheMatrix function creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL ##This is to make sure that if the matrix is changed, the cached inverse also needs to be reset
      }
      get <- function() x
      
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve function computes the inverse of a source matrix
## and caches the result for the future use. 

## If the inverse has already been calculated,
## then the cache value is retrieved.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("Getting cached inverse")
            return(inv)
      }
         
      data <- x$get()
      inv <- solve(data)
      x$setInverse(inv)
      
      inv
      
}
