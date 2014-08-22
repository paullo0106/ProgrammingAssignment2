## A pair of functions that calculate and cache the inverse of a matrix
##
## Example:
##   > mx <- matrix(c(2,2,3,2),nrow=2)
##   > my_func <- makeCacheMatrix(mx)
##   > my_func$get()
##   ... print out the matrix we just put into function object
##   > cacheSolve(my_func)
##   ... print the inverse of matrix, this time requires calculation
##   > cacheSolve(my_func)
##   ... print the inverse of matrix from cache data directly
##   > mx2 <- matrix(c(0,1,1,0),nrow=2)
##   > my_func$set(mx2)
##   ... update a new matrix
##   > my_func$get()
##   > cacheSolve(my_func)  
##   ... need another calculation since the value is changed
##   > cacheSolve(my_func)
##   ... print the inverse of matrix from cache

## This function takes a matrix as input, and 
## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(my_matrix = matrix()) {
  cache_inverse <- NULL  # cache data
  set <- function(new_matrix){
      my_matrix <<- new_matrix  ## update the value of matrix
      cache_inverse <<- NULL
  }
  # get the matrix
  get <- function() my_matrix  
  # save the inverse matrix to 'm' as our cache
  setsolve <- function(solve) cache_inverse <<- solve
  # get the cache of inverse matrix 
  getsolve <- function() cache_inverse
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache_inverse <- x$getsolve()
  
  if(!is.null(cache_inverse)){ # if we have already calculated the inverse previously
    message("getting cache data")
    return(cache_inverse)
  }
  else{ # we didn't calculate inverse before
    message("calculating inverse matrix")
    my_matrix <- x$get()  ## get original matrix
    inverse_matrix <- solve(my_matrix, ...)  ## do inverse calculation
    x$setsolve(inverse_matrix)  # save the data to the cache variable
    return(inverse_matrix)
  }
}

