##This function creates a special "matrix" object that can cache its inverse
##Then able to return the inverse of aforementioned special "matrix" from cache

makeCacheMatrix <- function(x = matrix()) {
      
      #set value of the matrix, get value of matrix
      #set value of the inverse, get the value of the inverse
      
      m <- NULL
      set <- function(y) {
            # <<- operator assigns values from non-current environment
            x <<- y
            m <<- NULL
      }
      #set to null
      
      get <- function() x
      set.inverse <- function(matrix.inversed) m <<- matrix.inversed
      get.inverse <- function() m
      list(set = set, get = get,
           set.inverse = set.inverse, get.inverse = get.inverse)
      #define list of functions to be accessed later

}


#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse 
#from the cache.

cacheSolve <- function(x, ...) {    ## Return a matrix that is the inverse of 'x'
      m <- x$get.inverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
            #if inverse calculated, returns it.
      data <- x$get()
      m <- solve(data)
      x$set.inverse(m)
      m
            #if inverse not calculated, calculates, returns value
}
