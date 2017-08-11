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

## Here's two examples showing how this code works
## Example 1:


        > my_matrix$set(matrix(c(3,3, 2, -10), 2, 2))
        > my_matrix$get()
             [,1] [,2]
        [1,]    3    2
        [2,]    3  -10
        > my_matrix$getInverse()
        NULL
        > cacheSolve(my_matrix)
                   [,1]        [,2]
        [1,] 0.27777778  0.05555556
        [2,] 0.08333333 -0.08333333

## Example 2:


        > my_matrix <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
        > my_matrix$get()
                   [,1]       [,2]       [,3]
        [1,]  0.1914453 -0.2003549 -0.7914014
        [2,] -0.4488061 -0.3078116 -0.9110498
        [3,]  0.1862152  1.5331059 -0.0952676
        > my_matrix$getInverse()
        NULL
        > cacheSolve(my_matrix)
                   [,1]       [,2]        [,3]
        [1,]  1.7503163 -1.5126087 -0.07495506
        [2,] -0.2607051  0.1584944  0.65002218
        [3,] -0.7741668 -0.4060354 -0.18269485

