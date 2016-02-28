## The functions below can be used to create a matrix and cache its inverse

## makeCacheMatrix is a function that creates a matrix and 
## provides four methods to manipulate it:
## 	set - to set the value of the matrix  
##	get - to get the value of the matrix
##	setinverse - to set the inverse of the matrix
##	getinverse - to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve calculates the inverse of a matrix that has been augmented with the 
## makeCacheMatrix function to have set, get, setinverse and getinverse "methods". 
## At first, a check is done to see if the inverse has already been calculated.  
## If so, the inverse is obtained from the cache and the computation is skipped. 
## Otherwise, the inverse is calculated and the setinverse function of the matrix used 
## to set the inverse of the matrix.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
	# There is no need to pass further parameters to solve().
    # For the matrix to be invertible, the columns must be linearly 
    # independent (so there is no need to set the tol parameter). 
	# To get the inverse with solve(), parameter b should be omitted.
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
