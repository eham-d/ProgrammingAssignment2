##### Overall comments:
# makeCacheMatrix accepts a matrix, solves it to get its inverse, and returns a list of
# 4 objects, one of which is the inverse matrix

# cacheSolve requires the result of makeCacheMatrix, from which it cheks to see if an 
# inverse matrix has been calculated. If so it is printed, otherwise the inverse is
# calculated.
#####


## In the comments the "inputted matrix" is the matrix we wish to find the inverse of;
## the "inverse matrix" is the result - i.e the inverse matrix of the "inputted matrix". 


## "makeCacheMatrix: This function creates a special "matrix" object that can cache its
## inverse." (from the assignment description)

## Saving makeCacheMatrix to an object, i.e: a<-makeCacheMatrix, and then str(a) reveals
## that "a" is a list of 4, with each element of the list containing a function. Each 
## element may be accessed like this e.g. a$get() returns matrix x.

makeCacheMatrix <- function(x = matrix()) { # x, taken by the function is a  matrix
 
  inverse <- NULL
  set <- function(y) { #"Set" changes the value of x, in the "outside" environment
    x <<- y # Change happens here - as the "<<-" operator assigns the value of y to x,
            # in an environment different to that of function set, such as the main
            # function environment
    inverse <<- NULL #If the matrix has been changed, the inverse must be recalculated
  }
  get <- function() x # Stores the value of the inputted matrix, access by a$get()
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse # Stores the result (the inverse matrix)
  list(set = set, get = get, #This list, with its 4 objects is the returned output
       setinverse = setinverse,
       getinverse = getinverse)
  
}
a<-makeCacheMatrix(x)


## cacheSolve: "This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
#  the cache." (from the assignment description)

cacheSolve <- function(x, ...) { # Takes the output of makeCacheMatrix as an object 
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse() # The stored inverse matrix is assigned to "inverse"
  if(!is.null(inverse)) { # If getinverse has a value, this is returned below
    message("getting cached data")
    return(inverse)
  }
  data <- x$get() #The following lines only appear to proceed if "inverse" was null
    # implying that there wasn't a stored inverse matrix. In such a case:
    ## the inputted matrix is stored as "data" and then solved and saved as "inverse",
    ## and x$setinverse then takes the function solve as an argument
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse #Finally inverse is returned
}



# using the functions -----------------------------------------------------

# The functions may be run seperately i.e. a1<-makeCacheMatrix(a_matrix),
# b2<-cacheSolve(a1)

# Or in one step: inversematrix<-cacheSolve(makeCacheMatrix(a_matrix))



# An example showing use --------------------------------------------------

# An invertible matrix (source:http://www.mathwords.com/i/inverse_of_a_matrix.htm )
x<- matrix(c(4,3,3,2),2,2) # The matrix
x_inverse<-matrix (c(-2,3,3,-4),2,2)
identity_2x2<-matrix(c(1,0,0,1),2,2) # identity matrix

x %*% x_inverse # "true" matrix multiplication shows that x_inverse is the inverse
# of x, since the answer is identity

solve(x,identity_2x2) # documentation: "solves the equation a %*% x = b for x,
#where b can be either a vector or a matrix". In this case I am solving:
#x %*% x_inverse = identity_2x2 for x_inverse


cacheSolve(makeCacheMatrix(x)) #Works as intended, yielding the inverse matrix of x.
