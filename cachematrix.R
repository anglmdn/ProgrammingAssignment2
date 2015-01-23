#my functions take first a matrix as an argument, 
##when setting it´s assigned to a global variable so it can work on both functions
##They calculate the inverse of a matrix when it hasn´t been computed and stores it
##or look for it in the list of functions in the first function


## makeCacheMatrix sets the value of the matrix and resets the inverse every time it changes
##takes the inverse computed by the cacheSolve function and stores it
##returns a list so you can choose the function you want to run

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL               
    set <- function(y){ 
      x <<- y       
      i <<- NULL
    }
    get<- function()  x
    setsolve <- function(inverse) i<<-inverse
    getsolve <- function() i
    list(get = get, set = set, 
         setsolve = setsolve, 
         getsolve = getsolve)
  }


## cacheSolve Takes a list as an argument,and looks for the inverse of the current vector
##if it has been already computed it should be in the getmean() part of the list
##if it is not it computes it and stores it in the list of makeCacheMatrix

cacheSolve <- function(l, ...) {
  check <- l$getsolve()
  if(!is.null(check)){
    message("getting cache data")
    return(check)
  }
  mat<-l$get()
  i<-solve(mat)
  l$setsolve(i)
  i
}
