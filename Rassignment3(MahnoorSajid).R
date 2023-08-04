 ## Put comments here thaat give an overall description of what your function do 
 ## Function to create a special "matrix" object. 
 ## This object can cache its inverse

makeCacheMatrix <- function(x =matrix()) {
   inv <- NULL
   set <- function(y) {
        x <<-y
        inv <<- NULL 
   }
   get <- function()x
   setInverse <-function (inverse) inv <<- inverse
   getInverse <-function()
     list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
   
}


 ##In oorder to compute the inverse of 'matrix' created by makeCacheMatrix
 ##If the inverse has already been created (and the matrix has not changed) then it should retrive the inverse form cache.

 cacheSolve <- function(x, ...) {
   ##Return a matrix that is inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv)) {
       message("getting cached data")
       return(inv)
        
     }
   
   mat <-x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
 }
