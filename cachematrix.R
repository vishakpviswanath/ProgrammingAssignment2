## Put comments here that give an overall description of what your
## functions do

## the function makeCacheMatrix would create and return the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    
    get<-function() x
    setinv<-function(inverse) inv<<-inverse 
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## this function is used to check if the the inverse matrix is present in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting the cached data")
        return(inv)
    }
    mat.data<-x$get()
    inv=solve(mat.data,...)
    x$setinv(inv)
    return(inv)
    
}
