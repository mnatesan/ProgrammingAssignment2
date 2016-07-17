## The pair of functions makeCacheMatrix and cacheSolve 
## implemented in this file work together to cache the inverse of ## a matrix.
## 1. makeCacheMatrix : Creates a special "matrix" object that 
## can cache the inverse of the specified matrix.
## 2. cacheSolve : Returns the inverse of the special 
## "matrix" returned by makeCacheMatrix by retrieving the inverse ## from the cache if it already resides there, or by computing it 
## and storing it in the cache for future retrieval before 
## returning it to the caller.



## makeCacheMatrix: Creates a "Matrix" Object unique to input 
## matrix X that contains the following:
## 1. The object X (a square invertible matrix)
## 2. The object INV, which represents the inverse of X
## 3. A list of 4 methods (functions)pertaining to X and/or INV
## get - A method to retrieve and return the input matrix X
## set - A method to store the input matrix in X, 
##       and a NULL value in INV
## getInv - A method to retrieve and return INV
## setInv - A method to store the input matrix in INV
##


makeCacheMatrix <- function(X = matrix()) {

INV <- NULL

   set <- function(MAT_INPUT) {
      X <<- MAT_INPUT
      INV <<- NULL
   }

   get <- function() X

   setInv <- function(INV_INPUT) INV <<- INV_INPUT

   getInv <- function() INV

   list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## cacheSolve(): Returns the inverse or a matrix, given 
## its "Matrix" Object as input, using the following logic
## 
## (1) Retrieves the inverse of the desired matrix
## from the cache if it already resides there, or 
##
## (2) Computes the inverse, store it
## in the cache for subsequent retrieval, and return it to the
## caller.



cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'

   INV <- X$getInv()

   if(!is.null(INV)) {
      message("Getting inverse of input matrix from the cache")
      return(INV)
   }

   
   DATA <- X$get()

   INV <- solve(DATA, ...)

   X$setInv(INV)

   INV

}
