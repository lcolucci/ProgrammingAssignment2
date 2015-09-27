# R Programming: Programming Assignment 2
# 
# GOAL This code inverts a matrix and caches the results. The goal of cache-ing is to 
# potentially save time on future matrix inversions by pulling up the results stored in 
# the cache instead of performing the inversion again. There are 2 functions in 
# this script: makeCacheMatrix() and cacheSolve (). They are described below. 
#
# Created by Lina Colucci, 26 Sept 2015


# makeCacheMatrix()
#    This function creates four functions (get, set, setinverse, getinverse) embedded into it.
#    The first functions 'get' displays a matrix. The function 'set' changes the stored matrix. 
#    The function 'setinverse' stores the inverse of the matrix (but doesn't compute it). And 
#    finally the functino 'getinverse' displays the matrix inverse when we ask for it. All these 
#    outputs are stored in list format (a cache) that we can call later if need be. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get=get,      #Cache the results
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve()
#    This function calculates the inverse of a matrix. The function solve() is a base 
#    package function that does the heavy lifting, i.e. actual inversion calculation. 
#    The function first checks the cache to see if the matrix inverse has already 
#    been calculated and stored there. If yes, the function produces a message "getting
#    cached data" and returns the stored result. If not, the matrix inverse is calculated
#    and this new result is now stored in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #Invert the matrix
        x$setinverse(m)
        m
}
