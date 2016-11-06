## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#creates matric object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

    i = NULL            #setting i as NULL
    set <- function(y) {
        x <<- y         #using "<<" as the variable is defined outside set
        i <<- NULL
    }
    get = function() x      
    seti = function(inverse) i <<- inverse 
    geti = function() i
    list(set=set, get=get, seti=seti, geti=geti)    
}


## Write a short comment describing this function
#Computes the inverse using function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i = x$geti()
    
    # if the inverse has already been calculated
    if (!is.null(i)){
        # get it from the cache and skips the computation. 
        message("getting cached data")
        return(i)
    }
        mat.data = x$get()
        i = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$seti(i)
        
        return(i)
}

mat<-matrix(c(1,0,1,0,2,2,1,2,2),3,3)
head(mat)
t<-makeCacheMatrix(mat)

start.time = Sys.time()
cacheSolve(t)
dur = Sys.time() - start.time
print(dur)



