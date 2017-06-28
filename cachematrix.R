## Overall, the functions create a place in memory where funcitons and the
## computer inverse matrix are stored. This allows them to be called in future
## and does not take up memory re-calculting them if the same matrix inversion
## is required again. Otherwise, if it is a new matrix, the new matrix inversion
## is stored in the environment of makeCacheMatrix for use in the future.

## mackeCacheMatrix creates the environment for cacheSolve to call
## functions/objects from, and to retrieve the inverse matrix from,
## if the matix specified is the same as when the function was last run.
## If a new matrix is specified, then the cacheSolve function solves the
## matrix inverse and stores it in the environment of makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmi <- function(solve) m <<- solve
        getmi <- function() m
        list(set = set, get = get, setmi = setmi, getmi = getmi)

}


## cacheSolve first assigns m to the value of x in getmi in the makeCacheMatrix
## environment. Then, if x is the same as the matrix specified previously, m
## will have not been specified as NULL in makeCacheMatrix, so the if function
## first tells us what its going to do, then retrieves the inverse matrix from memory.
## If a new matrix has been specified, its inverse matrix is solved and stored to m in 
## the makeCacheMean environment.

cacheSolve <- function(x, ...) {
        m <- x$getmi()
        if(!is.null(m)) {
                message("retrieving data from cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmi(m)
        m
}
