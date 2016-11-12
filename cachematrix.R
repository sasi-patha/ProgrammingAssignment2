## Put comments here that give an overall description of what your functions do
## Function makeCacheMatrix creates variables and functions(getters, setters) for Matrix, its Inverse. 
## Function cacheSolve is called for setting a new Matrix as well, getting inverse from cache 
## if already present or Calculate the inverse of a new matrix and return it.

## Write a short comment describing this function - makeCacheMatrix
## Function makeCacheMatrix should first be supplied by a square Matrix for which inverse exists.
## this function returns the Function objects of - getters, setters as a list. The GLobal 
## environment would contain the variables(matrix, inverse matrix, ) and functions.

makeCacheMatrix <- function(M = matrix()) 
{
  
  I <- NULL         #inverse of the input Matrix
  prevM <- NULL     # previous matrix for which inverse was calculated.
  
  # set, get Functions on input Matrix
  setmatrix         <- function(m) {
    M <<- m                                                 
  }
  getmatrix         <- function() M
  
  # set, get Functions on previous Matrix for which Inverse was calculated.
  cachematrix       <- function(prevm) {
    prevM <<- prevm
  }
  getcachematrix    <- function() prevM
  
  # set, get Functions on Inverse of the input Matrix
  cacheinverse      <- function(inv) I <<- inv
  getinverse        <- function() I
  
  # Return the list of Functions   
  list( setmat     = setmatrix,    getmat     = getmatrix,
        setprevmat = cachematrix,  getprevmat = getcachematrix,
        setinv     = cacheinverse, getinv     = getinverse )
}


## Write a short comment describing this function
## Function cacheSolve is called to calculate the Inverse of a Matrix if it is not cached already.

cacheSolve <- function(mCM = makeCacheMatrix(), ...) 
  
{
  
  #Assumption : For this assignment, the matrix supplied is always invertible( Square matrix and determinant <> 0)
  # Return the inverse of x, or calculate & return if cache is empty
  
  i     <- mCM$getinv()
  prevm <- mCM$getprevmat()
  m     <- mCM$getmat()
  
  if(identical(m,prevm) & !is.null(i) ) {
    
    message("Getting cached date for inverse of Matrix:")
    return(i)
  } 
  else { 
    i <- solve(m) 
    mCM$setinv(i)
    mCM$setprevmat(m)
    print("Calculated inverse of the new Matrix:")
    return(i)
  }
  
}








