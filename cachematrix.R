## Put comments here that give an overall description of what your
## functions do
## Function makeCacheMatrix returns a list of 4 different functions  
## Function cacheSolve calls different functions in the makeCacheMatrix function
## to calculate the inverse of a matrix



## Function makeCacheMatrix creates 4 different functions  
## set --> Creates a new matrix based on the input passed to the main function 
##         and also sets the inverse matrix to a blank matrix as a reset
## get --> Outputs the matrix provided as an input to the main function
## setsolve --> Sets the value of the inverse matrix(i) based on the input passed
## getsolve --> Outputs the value of the inverse matrix (i)
## The main function returns a list with these 2 functions

makeCacheMatrix <- function(x = matrix()) {
  i <- matrix()
  set <- function(y) {
    x <<- y
    i <<- matrix()
  }
  get <- function() {x}
  setsolve <- function(solve) {i <<- solve}
  getsolve <- function() {i}
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function takes the output of makeCacheMatrix function as an input
## after makeCacheMatrix's set function has been invoked.
## It then invokes different functions of makeCacheMatrix to calculate the 
## inverse of the matrix.
## Once the output of makeCacheMatrix has been passed to cacheSolve, it first
## checks if the inverse matrix has already been computed by invoking the getsolve
## function. If it has been computed, it prints the cached data message and returns
## the inverse matrix. If it hasn't been computed, it computes the inverse using
## solve function and returns the value of the inverse matrix. It also invokes
## the setsolve function from makeCacheMatrix so that the inverse matrix object i
## is set in the cache.
## Process to invoke these two functions is as shown in the bottom comments
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getsolve()
  if(!is.na(n[1,1])) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setsolve(n)
  n
}
## Get the list of 4 functions from makeCacheMatrix into object v
##      v <- makeCacheMatrix()
## Input the value of the matrix using the set function in makeCacheMatrix
##      v$set(matrix(rnorm(9),3,3))
## Print the value of the matrix
##      v$get()
## Run the cacheSolve function to calculate the inverse matrix
##      cacheSolve(v)
## Run the cacheSolve again to make sure the cached value is returned
## Set the value of a matrix to a new level and confirm that the cache is reset


