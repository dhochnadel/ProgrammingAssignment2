## The first function takes a matrix input and converts
## it to a special list, while the second uses that list
## to either compute the inverse of the matrix, or to pull
## said inverse from a stored cache in order to save computation
## time.

## Both of these were written using the 'makeVector' and 'cachemean'
## functions found at https://github.com/rdpeng/ProgrammingAssignment2
## as guides for the second programming assignment of the Coursera
## R programming course.

## This first function, 'makeCacheMatrix', converts the matrix input
## into a list containing four functions to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## This second function 'cacheSolve' calculates the inverse
## of the special "matrix" created with the above function.
## However, if the inverse has already been computed, it
## will pull the inverse from the cache and skip computing it
## a second time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}