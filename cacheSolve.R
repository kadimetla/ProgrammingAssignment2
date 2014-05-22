 cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  if ( det(data) == 1) {
  m <- solve(data, ...)
  x$setinv(m)
 }
  m
}