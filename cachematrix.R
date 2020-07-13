## Put comments here that give an overall description of what your
## functions do
## R Programming - Programming Assignment 2
## Write a short comment describing this function
## A pair of functions that cache the inverse of a matrix.
## This function creates a special matrix that can cache its inverse., 
## which is really a list containing a function to:
## 1. set the value of the matrix - makeCacheMatrix$set()
## 2. get the value of the matrix - makeCacheMatrix$get()
## 3. set the value of the inverse - makeCacheMatrix$setInv()
## 4. get the value of the inverse - makeCacheMatrix$setInv()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(solveMatrix) inv <<- solveMatrix
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by above 
##function.
## The matrix is assumed to be square and nonsingular.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv      
}
