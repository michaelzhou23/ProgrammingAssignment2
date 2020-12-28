## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
