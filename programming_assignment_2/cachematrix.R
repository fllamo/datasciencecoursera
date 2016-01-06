##########################################################################
##  We are taking advantage of R's lexical scoping rules which sets up
##  a unique environment for each unique execution of a function.
##  if we take the makeCacheMatrix below and run it as
##      a <- makeCacheMatrix(matrix(8:11,2,2))
##  and then run cacheSolve(a) the value returned for that run of cacheSolve runs
##  in the environment or scope of 'a' and as such has access to the matrix that was
##  stored in the environment (scope) the original execution of makeCacheMatrix.
##  if we again run makeCacheMatrix but this time like this
##      b <- makeCacheMatrix(matrix(1:4,2,2))
##  and now run cacheSolve(b) the value returned will be for the new matrix but
##  if we again run cacheSolve(a) we will get the origin value from the first fun
##  of cacheSolve(a) only this time it return the value from cache (memory) it is not recomputed
##  at this point we can contiune to run both cacheSolve(a) and cacheSolve(b) as much as we like
##  and the values returned are always returned from cache and not computed,
##  saving computational cycles.
##
##
##########################################################################

##########################################################################
##
## makeCacheMatrix - creates a list object which is used to cache
##                   (save to memory) and retrieve the values of the
##                   matrix passed into it and its' inverse which is calculated
##                   via the cacheSolve function
## parameters - x square matrix
## returns - list object containing funtion pointers
##           to get and set the matrix value
##           as well and get and set the inverse matrix value
##
##########################################################################
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

##########################################################################
##
## cacheSolve - Returns the inverse of a matric.
##              if the value exists it returns the value from cache (memory)
##              other wise it calculates the inverse of a matrix stores it
##              to cache and returns the value.
## parameters -
##      x - object containing the matrix to solve for and the inverse
## returns - inverse matrix
##
##########################################################################
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix)
    x$setInverse(i)
    i
}
