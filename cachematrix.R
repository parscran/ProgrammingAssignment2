##
## Coursera - Data Science Specilization : 2. R Programming
##      Week 3 : Programming Assignment 2
##      
## Author :
##      parscran (parscran@gmail.com)
##
## This programming assignment is about to write an R function is able to
## cache potentially time-consuming compuatations like a Matrix inversion.
## Their may be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.
##

## Function 
##  - Name : 
##      makeCacheMatrix
##  - Desciprtion :
##      This creates a special "matrix" object that can cache its inverse,
##      which is really a list containing a function to :
##        1. set the value of the matrix
##        2. get the value of the matrix
##        3. set the value of the inverse for the matrix
##        4. get the value of the inverse for the matrix
##
makeCacheMatrix <- function(x = matrix()) {
        # Declare a local object, 'inv', for an inverse of this "matrix"
        inv <- NULL
        
        # Declare a function for setting the value of the matrix 
        # and initialize the inverse fot this matrix
        setMtx <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Declare a function for getting the value of the matrix
        getMtx <- function() x
        # Declare a function for setting the value of the inverse
        setInv <- function(inverse) inv <<- inverse
        # Declare a function for getting the value of the inverse
        getInv <- function() inv
        
        # Return a list containing functions for the matrix and inverse
        list(setMtx = setMtx,
             getMtx = getMtx,
             setInv = setInv,
             getInv = getInv)        
}


## Function 
##  - Name : 
##      cacheSolve
##  - Description :
##      This computes the inverse of the special "matrix" returned by 
##      'makeCacheMatrix' above. If the inverse has already been calculated,
##      then this function should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
        # Get the value of the inverse from the "makeCacheMatrix"
        inv <- x$getInv()
        # Check whether the inverse has already been calculated
        if (!is.null(inv)) {
                # Print a message for cached data
                message("Getting cached data")
                # Return a matrix from the cached data
                return(inv)
        }     
        
        # Get the value of the matrix
        data <- x$getMtx()
        # Calculate the value of the matrix using 'solve'
        inv <- solve(data, ...)
        # Set the value of the inverse for the matrix
        x$setInv(inv)
        # Return a matrix that is the inverse of 'x'
        inv
}
