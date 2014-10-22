## Put comments here that give an overall description of what your 
## functions do 
  

## Write a short comment describing this function
##
## The makeCacheMatrix creates a cache wrapper
## around a matrix object.  The function takes 
## 1 argument that is defaulted to a new empty 
## matrix if one is not provided during function
## invocation.
## 
## The makeCacheMatrix function returns a list object
## with get, set, getInverse and setInverse functions
## that interact with the matrix within it's environment.
## 
## set() sets the matrix in the instance of the makeCacheMatrix
## get() gets the matrix in the instance of the makeCacheMatrix
## setInverse() sets the inverse of the matrix in the instance of makeCacheMatrix
## getInverse() gets the inverse of the matrix in the instance of makeCacheMatrix
## 

makeCacheMatrix <- function(regMatrix = matrix()) { 
  inverseMatrix <- NULL
  set <- function(nRegMatrix){
    regMatrix <<- nRegMatrix
    inverseMatrix <<- NULL
  }
  get <- function()regMatrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
 
} 
 


## Write a short comment describing this function 
##
## cacheSolve is a function that has 1 explict argument
## and inherited arguments for the solve function.
## 
## cacheSolve takes a makeCacheMatrix instance and uses
## the getInverse() to check if the inverse has already
## been calculated for the matrix.  If getInverse returns 
## null, the cacheSolve function gets the matrix from 
## the makeCacheMatrix function and calculates the inverse
## by using the solve() function.  
## The cacheSolve function then sets the inverse matrix in the 
## makeCacheMatrix object and returns the inverse matrix.
##
## If getInverse() returns an inverse matrix, the message
## "getting inverse matrix from cache" is returned, then the inverse
## matrix is returned.
##



cacheSolve <- function(cacheMatrix, ...) { 
   ## Return a matrix that is the inverse of 'cacheMatrix' 
  inverseMatrix <- cacheMatrix$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting inverse matrix from cache")
    return(inverseMatrix)
  }
  data <- cacheMatrix$get()
  inverseMatrix <- solve(data, ...)
  cacheMatrix$setInverse(inverseMatrix)
  inverseMatrix
  
} 

