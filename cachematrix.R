## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function


#makeCacheMatrix function gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object. 


makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
#set the value of the Matrix
    setMatrix <- function(y) {
      x <<- y
      inverseMatrix <<- NULL
      }
}

  getMatrix <- function() x                                    #get the value of the Matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse  #set the value of the invertible matrix
  getInverse <- function() inverseMatrix                     #get the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
               setInverse = setInverse, getInverse = getInverse)

}

## Write a short comment describing this function

#The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.

cacheSolve <- function(x, ...) {
    
#get the value of the invertible matrix from the makeCacheMatrix function
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {                       #if inverse matrix is not NULL
          message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
          return(inverseMatrix)                             #return the invertible matrix
      }
          
#if value of the invertible matrix is NULL then  
         MatrixData <- x$getMatrix()                     #get the original Matrix Data 
         inverseMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(inverseMatrix)                         #set the invertible matrix 
        return(inverseMatrix)                               #return the invertible matrix
                                                        ## Return a matrix that is the inverse of 'x'
}
