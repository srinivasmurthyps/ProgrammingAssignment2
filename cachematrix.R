# makeCacheMatric gives the inversed matrix through get_inverse. If it is not inversed in the prior execution it gives Null when get_inverse is invoked 
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_Inverse <- function(inverse) inv_matrix <<- inverse
  get_Inverse <- function() inv_matrix
  list(set = set, get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}


## Cache function returns the Cached inverse matrix upon repeated execution of cacheSolve function, with a message getting cached data
## If the matrix is not inversed in previous execution, then it will do it as it is doing it for first time
cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_Inverse()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  my_mat <- x$get()
  inv_matrix <- solve(my_mat, ...)
  x$set_Inverse(inv_matrix)
  inv_matrix
        ## Return a matrix that is the inverse of 'x'
}
