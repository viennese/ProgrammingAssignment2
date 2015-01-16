## The two functions should create the inverse of a matrix x.
## If the inverse is available in cache, it is taken from cache. otherwise calculated.

## makeCacheMatrix creates a special R object that first initializes a variable 'm'
## then provides a get() function to obtain a matrix. Also provides function
## setinvmatrix() to assign the computed inverse matrix (of x) to m;
## Provides function getinvmatrix() to obtain the cached inverse matrix.
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  get <- function() x
  setinvmatrix <- function(Imatrix) m <<- invmatrix
  getinvmatrix <- function() m
  
  # return a list of functions as an R object
  list(get=get, setinvmatrix=setinvmatrix, getinvmatrix=getinvmatrix)
}

## cacheSolve inveses the matrix x.  If the inverse matrix is found,
## it returns the result and terminates. If it is not found, the inverse
## is calculated, then saved to cache and returned.
cacheSolve <- function(x) {
  m <- x$getinvmatrix()
  if(!is.null(m)){
    message("Found in cache. Getting data.")
    return(m)
  }
  else {
    message("Calculating inverse matrix")
    data <- x$get() # obtains matrix from object x
    m <- solve(data) # finds inverse matrix
    x$setinvmatrix(m) # assigns resulting inverse matrix to object x
    return(m)
  }
}
