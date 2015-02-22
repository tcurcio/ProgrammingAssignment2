## https://github.com/tcurcio/ProgrammingAssignment2.git


## Write a short comment describing this function

##function will create a special matrix object that will cache its inverse.
##It is a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


##function completes inverse of special matrix. If inverse has been calculated
##in cache, will return inverse from cache.
##checks to see if inverse has been stored in memory first
##if so, returns value in memory.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
