makeVector <- function(x = matrix()) {
  f <- NULL
  set <- function(y) {
    x <<- y
    f <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) f <<- solve
  getsolve <- function() f
  list(set = set, get = get,
       setsove = setsolve,
       getsolve = getsolve)
}
cacheSolve <- function(x, ...) {
  f <- x$getsolve()
  if(!is.null(f)) {
    message("getting cached data")
    return(f)
  }
  data <- x$get()
  f <- solve(data, ...)
  x$setsolve(f)
  f
}