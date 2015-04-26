## 
## Functions makeCacheMatrix and cacheSolve are helper functions 
## for speeding up the proces of inverse matrice computation.

## Function makeCacheMatrix returns a vector of following functions  
##          set : setting original matrix
##          get : return a matrix previously set by set function
##   setInverse : computes the inverse metrix to the original matrix
##   getInverse : return the inverse metrix to the original matrix

makeCacheMatrix <- function(x = matrix()) {
		im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse_matrix) im <<- inverse_matrix
        getInverse <- function() im
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Returns inverse matrix from cache if it was already solved, 
## if not, computes the inverse matrix and returns it right away.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getInverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInverse(im)
        im
}