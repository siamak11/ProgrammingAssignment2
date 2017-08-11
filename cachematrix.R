## The following two functions, i.e makeCacheMatrix and cacheSolve, will cache the inverse 
## of a matrix.

## My first function creates a certain "matrix" object that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
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


## Assume that we have created a certain "matrix" by makeCacheMatrix function above. 
## The following function computes the inverse of this certain "matrix" created by makeCacheMatrix
## as above. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## Here's an example showing how this code works

# my_matrix$set(matrix(c(3,3, 2, -10), 2, 2))
# my_matrix$get()
#       [,1] [,2]
# [1,]    3 2
# [2,]    3 -10
# my_matrix$getInverse()
# NULL
# cacheSolve(my_matrix)
#       [,1] [,2]
# [1,] 0.27777778 0.05555556
# [2,] 0.08333333 -0.08333333
# cacheSolve(my_matrix)
# getting cached data
#       [,1] [,2]
# [1,] 0.27777778 0.05555556
# [2,] 0.08333333 -0.08333333
