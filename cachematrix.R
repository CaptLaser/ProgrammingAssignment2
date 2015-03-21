## Here is a demonstration of variable assignment and retrieval
## from a parent environment.  This is useful to speed up processing
## by preventing repetitive calculation of the same data.

makeCacheMatrix <- function(dataMatrix = matrix()) {
        cachedMatrix <- NULL
        set <- function(y) {
                dataMatrix <<- y
                ## initialize the cachedMatrix to NULL in the
                ## makeCacheMatrix environment
                cachedMatrix <<- NULL
        }
        ## Create function "get" to be used by cacheSolve below
        get <- function() dataMatrix
        ## cachedMatrix in makeCacheMatrix env receives value solve
        setSolve <- function(solve) cachedMatrix <<- solve
        ## the following returns cachedMatrix from makeCacheMatrix
        getSolve <- function() cachedMatrix
        ## Here is a list of names and associated values assigned
        ## within the makeCacheMatrix environment.  This enables
        ## access from outside this env using "$" as shown below.
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## Function cacheSolve receives input from the makeCacheMatrix env
## and solves the inverse matrix only if it's not solved already.

cacheSolve <- function(inputMatrix, ...) {
        ## Go to the inputMatrix environment and assign
        ## the cachedMatrix value locally
        localMatrix <- inputMatrix$getSolve()
        ## If localMatrix is not null, return it as is.
        if (!is.null(localMatrix)) {
                ## return stored value; this is a function exit
                return(localMatrix)
        }
        
        ## getting this far means localMatrix == NULL
        ## therefore, calc the inverse matrix and store
        data <- inputMatrix$get()
        localMatrix <- solve(data, ...)
        ## calc'd value gets stored in makeCacheMatrix environment
        inputMatrix$setSolve(localMatrix)
        ## return the calculated value; function exit
        localMatrix
}