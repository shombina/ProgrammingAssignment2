# The result of matrix inversion is being cached during the first attempt
# and then is stored and returned if needed withot being computed again.

# Function makeCacheMatrix returns a functions list:
# set(y)/get() for setting/getting the matrix itself,
# setinv(inverse) for caching matrix's inverse and
# getinv() for getting it if already cached.

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get = function() x
  
  setinv = function(inverse) inv <<- inverse
  
  getinv = function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

# Function cacheSolve returns the inverse matrix, but if getinv()
# returns null, inverse matrix is calculated and cached first,
# otherwise the cached value is returned 

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  if (!is.null(inv)) {
    
    message("getting cached data")
    return(inv)
    
  }
  
  mat.data = x$get()
  
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  inv
  
}
