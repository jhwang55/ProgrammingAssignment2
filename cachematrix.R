## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #cache inverse matrix
  
  inv_matrix <- NULL
  
  #matrix set and get functions
  
  get <- function() x
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  #inverse matrix set and get functions 
 
  get_inverse <- function() inv_matrix
  set_inverse <- function(inverse) inv_matrix <<- inverse
  
  #return list of functions for the matrix
  
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Write a short comment describing this function

# This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$get_inverse()
    
    #if cached matrix exists, return it
    
    if(!is.null(inv_matrix)) {
      message("retreiving cached matrix")
      return(inv_matrix)
    }
    
    #compute inverse matrix
    
    matrix <- x$get()
    inv_matrix <- solve(matrix,...)
    
    #cache inverse matrix
    
    x$set_inverse(inv_matrix)
    
    #return inverse matrix
    
    return(inv_matrix)
}


##Testing

#set up matrix 
a <- matrix(c(1,2,3,4),2,2)
#cache the inverse matrix
a1 <- makeCacheMatrix(a)
#inverse after computation
cacheSolve(a1)
#inverse from cache
cacheSolve(a1)