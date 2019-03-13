##  These two functions together calculate the inverse matrix given input argument X and store result in cache to avoid recalculating.

## makeCacheMatrix returns list of setter and getter functions required for caching the original matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        #
        setMatrix <-function(thematrix){
                x <<- thematrix
                m <<- NULL
        }
        #
        getMatrix <- function() x
        #
        setInverseMatrix <- function(calcInvMatrix) m <<- calcInvMatrix
        #
        getInverseMatrix <- function() m
        
        #
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
        
}


## cacheSolve returns the InverseMatrix in memory if it exists, otherwise calculates inverse matrix for X and then stores in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrixData <- x$getMatrix()
        m <-solve(matrixData)
        x$setInverseMatrix(m)
        m
        
}
