## Put comments here that give an overall description of what your
## functions do
## These two functions help compute the inverse of a given matrix.
## If the inverse was already computed for the matrix, then
## the inverse is cached and returned when accessed. 


## 'makeCacheMatrix' returns an object containing functions
## to set/get inverses on a data matrix which acts as a cache
## for the inverse once set. '
makeCacheMatrix <- function(x = matrix()) {

   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
 
   get <- function()x
   setinv <- function(inverse) inv <<- inverse
   getinv <- function()inv
   list(set =  set, get = get, 
        setinv = setinv, 
        getinv = getinv)
}



## 'cacheSolve' returns the inverse of a matrix that is stored
## as a field in x accessed via get() function on x. If the
## inverse was already computed and stored in x, then the 
## cached inverse is returned and is not computed once again

cacheSolve <- function(x, ...) {
        
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinv()

   if(!is.null(i)) {

      message("getting cached data")
      return(i)
   }

   data <- x$get()
   i <- solve(data)
   x$setinv(i)
   return(i)   
   
}
