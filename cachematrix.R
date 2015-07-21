## Put comments here that give an overall description of what your
## functions do.



# An invertible matrix (source:http://www.mathwords.com/i/inverse_of_a_matrix.htm )
x<- matrix(c(4,3,3,2),2,2) # The matrix
x_inverse<-matrix (c(-2,3,3,-4),2,2)
identity_2x2<-matrix(c(1,0,0,1),2,2) # identity matrix

x %*% x_inverse # "true" matrix multiplication shows that x_inverse is the inverse
# of x, since the answer is identity

solve(x,identity_2x2) # documentation: "solves the equation a %*% x = b for x,
#where b can be either a vector or a matrix". In this case I am solving:
#x %*% x_inverse = identity_2x2 for x_inverse


## Write a short comment describing this function.
## "makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse."
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##cacheSolve: "This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse from 
#the cache."
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

cacheSolve(makeCacheMatrix(x)) #Works as intended
