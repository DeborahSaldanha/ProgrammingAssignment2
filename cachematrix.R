#make CacheMatrix creates a list containing a function to set the value of the m#atrix, get the value of a matrix, set the value of the inverse of the matrix and# get the value of the inverseof the matrix.
makeCacheMatrix <- function(x = matrix()) {
       inv<- NULL
       set<- function(y) {
            x<<- y
	     inv<<- NULL
	}
	get <- function()x
	setinverse<- function(inverse) inv<<- inverse
	getinverse<- function() inv
	list(set = set, get=get,
	     setinverse=setinverse,
	     getinverse=getinverse)
}


#The following function returns the inverse of a matrix. It first checks if the #inverse has been computed, if so, it gets the value and skips the computation.
# if not, it computes the inverse and sets the value in the cache via the setinv#erse function.
cacheSolve<- function(x, ...) {
    inv<- x$getinverse()
     if(!is.null(inv) {
        message(" getting cached data")
	return(inv)
     }
     data<- x$get()
     inv<- solve(data, ...)
     x$setinverse(inv)
     inv
}
