## This function creats the required matrix and the varibale of its inverse.
## This matrix can cache its inverse if it exists

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
	     get = get,
	     setInverse = setInverse,
	     getInverse = getInverse)	
}


## This function calculate the inverse of the matrix that's created by the
## makeCacheMatrix function if it wasn't created before or retrive the inverse
## if it's already exisits

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getInverse()
	  if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	   }
	   m <- x$get()
	   inv <- solve(m, ...)
	   x$setInverse(inv)
         inv
}

