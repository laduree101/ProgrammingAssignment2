# MakeCacheMatrix creates the matrix
MakeCacheMatrix <- function(x=matrix()) {
  new_matrix  <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    new_matrix <<- NULL
  }
  
  getmatrix <- function() x
  
  setinverse <- function(inv) new_matrix <<- inv
  
  getinverse <- function() new_matrix
  
  #creates the list for the functions created
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

#cacheSolve will give the inverse of a matrix
cacheSolve <- function (x,...){
  
  #if a matrix inverse already has been calculated, get it
  new_matrix <- x$getinverse()
  
  #check if cachesolve has been run  before
  if (!is.null(new_matrix)) { 
    message("CacheSolve already run, display the cached matrix")
    return(new_matrix)
  }
  
  y <- x$getmatrix() # if there is a new matrix, run the getmatrix function to get the value of the new matrix
  
  x$setmatrix(y) # set the input matrix to cache it
  
  new_matrix <- solve(y, ...)  #take the inverse of the matrix
  
  x$setinverse(new_matrix)  #set the inverse matrix to cache the inversed matrix
  
  return(new_matrix) # return the inversed matrix
} 

