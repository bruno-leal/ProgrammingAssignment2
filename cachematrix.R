## The following functions calculate the inverse matrix of a given matrix
## using cache data.
## The first one (makeCacheMatrix) allows to create a «special vector» that
## can then be used as an argument for the second function (cacheSolve).

## The makeCacheMatrix function creates an object (a list) that corresponds 
## to a «special matrix», with properties to (i) set the value of the matrix,
## (ii) get the value of the matrix, (iii) set the value of the matrix's
## inverse and (iv) get the value of the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function calculates the inverse of a given «special matrix»
## previously created with the makeCacheMatrix function.
## It returns the cached value if it has already been calculated, and if not
## it calculates the inverse matrix and cache's it for future reference.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    xdata <- x$get()
    inv <- solve(xdata)
    x$setinverse(inv)
    inv
}
