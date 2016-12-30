## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function takes in a regular square invertible matrix
# It is able to set the matrix and retrieve it using get()
# It is also able to set the inverse and retrieve it
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      #set matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      #get matrix
      get <- function() x
      #set matrix inverse
      setinv <- function(inv_) inv <<- inv_
      #get matrix inverse
      getinv <- function() inv
      #set type of output for this function
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Write a short comment describing this function
#Calculate/Retrieve from cache the inverse of the special matrix object
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      #if it has been calculated, retrieve it
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      #otherwise, calculate the inverse of the special matrix
      data <- x$get()
      inv <- solve(data, ...)
      #set and cache the inverse matrix inside the special matrix
      x$setinv(inv)
      #return a matrix that is the inverse of 'x'
      inv
}
