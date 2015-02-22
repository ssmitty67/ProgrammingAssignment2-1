## Put comments here that give an overall description of what your
## functions do

## this thread helped me understand the sample example and apply it to a
## matrix inversion as the assignment requires
## https://class.coursera.org/rprog-011/forum/thread?thread_id=817
## this thread helped explain that this stuff is analygous to objects/emthods
## and it started to click for me
## and the x$getmean(), x$setmean() syntax made alot more sense when described in this manner

## The first function, makeCacheMatrix, takes an arguement that is a matrix and creates 
## a list of functions (or methods if you prefer)
##  The second function, cacheSolve, then calls these "methods" to do the work.  Either pulling 
## pulling the cached inverted matrix or doing the calculations of inverting the matrix
## I am still confused about why the use of the '=' operator in the list function as
## shown in the example.
##  I kind of understand the '<<-' operator.  Without it these types of advanced data storage
## are not possible, I believe this is the heart of lexical scoping.
##  Although I am still trying to wrap my head around this :)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  ## this is kind of analygous to a method, to set new values to some object
  ## in this "method" we are setting a different matrix than the originally created matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ##  this "method" returns the matrix data
  get <- function() {
    x
  }
  
  ##  this "method" does the actual matrix inversion calculation and stores
  ##  or caches it for later use
  setsolve <- function(solve) {
    m <<- solve
  }
  
  ##  this "method" returns the cached inverted matrix (if there was one set)
  getsolve <- function() {
    m
  }
  
  ##  this function creates a list of the "methods" and returns it
  ##  I am still stuggling to understand why the '=' operator is used
  ##  I think it has to do with scoping within the function
  ##  meaning that these variable are defined only in the enviroment of the function
  ##  Alhtough this concept is a little murky to me
  ##if I have time I will come back and add comments about this 
  ##  if I get a better understanding
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
  

## The second function, cacheSolve, then calls these "methods" to do the work of 
## calculating and inverting the matrix
## it takes an arguement, which is the list of "methods" created in the first function
##  makeCacheMatrix
##  it first checks if there is a values for the inverted matrix with x$getsolve
## if it is not null, then is gives a message indicating it is using the cached data
##  then it returns the cached inverted matrix
##  if the cached inverted matrix is null, then it grabs the original matrix using x$get()
## "method" and calculates the inverted matrix using the "solve" function

cacheSolve <- function(x, ...) {
  
  ## calling the 'getsolve() 'method' from the 1st function on the
  ## x 'object', which would be the inverted matrix
  m <- x$getsolve()
  
  ## checks to see if the inverted matrix has been calculated
  ## if it has, return the cached value
  ## if it hasnt it will be null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## grab the original matrix information using the get 'method'
  data <- x$get()
  
  #  calculates the inverted matrix using the R solve function
  m <- solve(data, ...)
  
  ## returns the calculated inverted matrix to the setsolve "method" to be cached
  x$setsolve(m)
  
  ## returns calculated inverted matrix
  m
}
