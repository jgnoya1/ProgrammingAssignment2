## Put comments here that give an overall description of what your
## functions do
## In the function makeCacheMatrix stores in memory the inverse of the matrix 
## passed by parameter
## The function cacheSolve calculates the inverse of the matrix passed by parameter
## To do this operation first check if it was calculated previously, so in that case,
## is in memory using the function makeCacheMatrix, and returns the value stored in memory. 
## In the case the inverse of the matrix is not in memory calculates the inverse and stores it
## in memory using the function makeCacheMatrix
## So to execute this function it´s necessary follow this steps:
## 1.- Create a square matrix, name it matrix1, for example
## 2.- Create the special matrix with the function makeCacheMatrix passing matrix1 as a parameter. 
##     Example: matrix2 <- makeCacheMatix(matrix1)
## 3.- Execute the function cacheMatrix passing the matrix created in the 2 step as a parameter.
##     Example: matrix3 <- cacheMatrix(matrix2)

## Write a short comment describing this function
## This function takes as argument a matrix X and creates a new matrix with the following components:
## inv: variable to store the inverse of a matrix
## set: function that sets the value NULL to the variable inv, and sets the value of the original matrix
## get: function that returns the original matrix
## setinv: function that sets the value of the variable inv with the value passed by parameter (the inverse
##         of the original matrix)
## getinv: gets the value of the variable inv (stores the inverse of the original matrix)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Write a short comment describing this function
## This funcition requires as a parameter a matrix created with the funcition makeCacheMatrix
## so, first with x$getinv() checks if the inverse of the matrix was calculated previously, in that
## case is in memory, so writes a message and returns the inverse matrix stored in memory
## If the inverse matrix is not stored in memory, with x$get() obtains the original matrix, and calculates
## his inverse with the function solve. Finaly stores in memory the inverse matrix calculated with
## x$setinv(inv) and returns the inverse function calculated


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
