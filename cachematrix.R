
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a 
## matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { ## 1 ##
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
   
 
## Obtaining the value of x and assign it to "get". 
## Return the value x in its lexical scope
## of when the function (makeCacheMatrix) was defined.
  get <- function() x ## 2 ##
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
## A list is a generic vector containing other objects
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

### EXAMPLE ###
## 1 ## x = rbind(c(1, -1/4), c(-1/4, 1)) - create a matrix and assign to x
## 1 ## m = makeCacheMatrix(x) - assign the function to m with the matrix in it
## 2 ## m$get() return the matrix 

## No cache in the first run

## 2 ## will return the below
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) { ## 3 ##
  inv <- x$getinverse() ## 4 ##
  if(!is.null(inv)) { ## 5 ##
    message("getting cached data.")
    return(inv)
  }
  data <- x$get() ## 6 ##
  
##?solve This generic function solves the equation a %*% x = b for x, 
##where b can be either a vector or a matrix.
  inv <- solve(data)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

### EXAMPLE ###
## 3 ## > cacheSolve(m) - recieve m and will run thru ## 4,5,6 ##
## since ## 4,5 ## does not have cached data at the 1st run
## it will run ## 6 ## to get the inverse by using solve.
## inverse answer is assigned to inv and recieved by setinverse
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## 3 ## cacheSolve(m) - recieve m (same one again) and will run thru ## 4,5 ##
## it was able to get the previous cache data and hence 
## producing the message "getting cached data."
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 