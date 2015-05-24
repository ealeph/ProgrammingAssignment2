## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(m=matrix()) {
  inv<-NULL
  set<-function(x) {
    m <<-x
    inv <<-null
  }
  get<-function()m
  setinv <-function(solve) inv<<-solve
  getinv <-function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve<-function(m,...){
  inv<-m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-m$get()
  inv<-solve(data,...)
  m$setinv(inv)
  return(inv) ## Return a matrix that is the inverse of 'm'
}

checking:
  > m<-matrix(1:4,2,2)
> print(m)
[,1] [,2]
[1,]    1    3
[2,]    2    4
> trx=makeCacheMatrix(m)
> trx$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(trx)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(trx)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

