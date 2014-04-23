# The following two functions are used to create a matrix object with the capability to cache its inverse. 


## This function creates a matrix object that is associated with a list of four functions: get(), set(), 
## getinverse(), and setinverse(). The get() function is used return the contents of the matrix while the set() 
## function changes the content of the matrix. The getinverse() function returns the inverse of the matrix if 
## the inverse is already computed using the cacheSolve() function, otherwise it returns NULL. The setinverse()
## function is used by the cacheSolve() funtion to set the computed inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inv <<- inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This function returns the inverse of a matrix created using the makeCacheMatrix() funtion. 
## If the inverse is not computed before for the given matrix then the inverse is computed and  
## stored in the cache. Repeated invocation of this method for the same matrix will return the 
## computed inverse stored in the cache. When the the content of the matrix is changed, the inverse 
## is computed again and stored in the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv  
}

