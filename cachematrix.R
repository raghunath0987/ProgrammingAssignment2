## The makeCacheMatrix contains list of functions to cache and read the cached matrix and its
## inverse. 
##The cacheSolve will take the matrix argument and calls the functions in the 
## makeCacheMatrix function to get the cached inverse value if the value is already computed. 
## If the matrix argument is different from the cached one, it will call functions under 
## makeCacheMatrix to compute the inverse, cache and read it.

## makeCacheMatrix is a function used to Cache and retrieve a Matrix 
## and its inverse. This function will return a list of following four
## functions defined in it. setMatrx() function caches the value of
## its matrix argument into the Matrx variable of the makeCacheMatrix function.
## getmatrx() function returns the value of the makeCachematrix function variable Matrx.
## setInverseMatrx() calculates the inverse of the Matrx variable of makeCacheMatrix 
## and caches it into the InverseMatrx variable of the makeCacheMatrix function
## getInverseMatrx() returns the value of the makeCacheMatrix function variable InverseMatrx

makeCacheMatrix <- function(x = matrix()) {
  
  ##Store the matrix into the makeCacheMatrix function variable Matrx
  setMatrx<-function(m=matrix()){
    Matrx<<-m
  }
  
  ## Retrieve the makeCacheMatrix function variable Matrx value if it exists, else NULL
  getMatrx<-function(){
    if(exists("Matrx")){
      Matrx
    }
    else{
      NULL
    }
  }
  
  ## Compute the inverse of the matrix and cache into the makeCacheMatrix function variable
  ## InverseMatrx
  setInverseMatrx<-function(){
    InverseMatrx<<-solve(Matrx)
  }
  
  ## Retrieve the cached value of the matrix inverse from makeCacheMatrix function variable
  ## InverseMatrx
  getInverseMatrx<-function(){
    InverseMatrx
  }
  
list(setMatrx=setMatrx,getMatrx=getMatrx,setInverseMatrx=setInverseMatrx,getInverseMatrx=getInverseMatrx)
  
}


## This function takes the matrix object as an argument and checks if the inverse is already
## calculated for this matrix. If it is, then it will get the value from the cache. If the 
## matrix argument is a new one, then it will call the setMatrx function to cache the matrix,
## then it will call setInverseMatrx to compute the inverse matrix and cache it. It will then
## call the getInverseMatrx to get the value from the cache.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Call the makeCacheMatrix and store the returned list of its functions in the Cache variable
  Cache<-makeCacheMatrix(x)
  
  ## If matrix argument and the cached matrix are identical, retrieve the cached inverse matrix
  if(identical(Cache$getMatrx(),x)){
    
    Cache$getInverseMatrx()
        
  }
  
  ## If the matrix argument is a new one, cache the new matrix, compute & cache the matrix inverse
  ## and retieve the matrix inverse.
  else
  {
    Cache$setMatrx(x)
    Cache$setInverseMatrx()
    Cache$getInverseMatrx()
  
  }
  
}
