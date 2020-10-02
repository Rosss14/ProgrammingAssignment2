## The functions below are used to calculate the inverse of a matrix,
## and store this value in cache to be later accessed provided the 
## original matrix has not changed

## The first function creates a list containing functions to
## 1) Set the value of the matrix 
## 2) Get the value of the matrix
## 3) Set the value of the inverse matrix
## 4) Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set<-function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s<<-solve
  getSolve <- function() s
  list(set=set , get=get , setSolve=setSolve , getSolve=getSolve)
}


## The following function looks for the value of inverse of x in 
## cache. If the value has been previously cached, the function
## returns this value. Otherwise, the value of inverse is calculated
## and stored in cache, and returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)){
    message("Getting cached data")
    return(s)
  }
  data<-x$get()
  s <- solve(data,...)
  x$setSolve(s)
  s
}
