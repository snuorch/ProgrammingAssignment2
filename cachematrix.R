
## This function store inverse of matrix 'x'
## it contains 4 function (2 setters methods, 2 getter methods)
## Two for get/set matrix itself, two for set/get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculate or retrieve from cache inverse of matrix 'x'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Try to get inverse from cached matrix
  inv <- x$getinv()
  if(!is.null(inv)) {
    #if not null (cached matrix exists) retrieve inverse from cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
