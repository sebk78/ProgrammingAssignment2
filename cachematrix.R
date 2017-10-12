## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## please see my comments inside the functions

makeCacheMatrix <- function(x = matrix()) {
  # this is basically a code similar to the one in the vector mean example, but it takes a matrix as argument instead of numeric
  my_inverted_matrix <- NULL
  
  # here we create the list of functions to be used by the cacheSolve function
  set <- function(y) {
    # assigning <<- to 'switch' the environment
    x <<- y
    my_inverted_matrix <<- NULL
  }
  get = function() x
  set_inv <- function(inv) my_inverted_matrix <<- inv
  get_inv <- function() my_inverted_matrix
  # and finally the output list that is used in the cacheSolve function
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # here x will be the list output from makeCacheMatrix()
  my_inverted_matrix <- x$get_inv()
  
  # unless the inverted matrix is calculated then get it from cache
  if (!is.null(my_inverted_matrix)) {
    message("gettin' the inv matrix from cache")
    return(my_inverted_matrix)
  }
  
  # if it's not in the cache then calculate the inverse
  my_matrix <- x$get()
  my_inverted_matrix <- solve(my_matrix, ...)
  
  # push the inverse into the cache
  x$set_inv(my_inverted_matrix)
  
  # return the inverse
  return(my_inverted_matrix)
}
