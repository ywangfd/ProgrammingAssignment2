## My task is to write a pair of functions that cache the inverse of a matrix

##makeCacheMatrix() creates a special "matrix" object 
##that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL

set <- function(y){
  x <<- y
  m <<- NULL
}

get <- function(){x}

set_inverse <- function(inverse){ m <<- inverse}

get_inverse <- function() {m}

list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


##cacheSolve() computes the inverse of the special "matrix" 
##returned by makeCacheMatrix() above. If the inverse has already been calculated(the matrix has not changed)
##then the cacheSolve() should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data,...)
  x$set_inverse(m)
  m
}
