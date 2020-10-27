## As per course example, The goal is to set a matrix using 'set' and extract 
##it using 'get, setinvMatrix would calculate inverse based on x and then getinvMatrix
## would release this.

## Finally, list at the end to allow accessing via $. 
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL  
        set <- function(y) {
                y <<- x
                invMatrix <<- NULL
        }
        get <- function () x 
        setinvMatrix <- function(inv) invMatrix <<- inv
        getinvMatrix <- function () invMatrix
        list(set = set, get = get, setinvMatrix = setinvMatrix,
             getinvMatrix = getinvMatrix)

}

## In this function, inv objective is used to get the matrix and if cached,
## the function is designed to return cached data and if not ,it will calculate
## as well as set and then return a calculated inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinvMatrix()
        if (!is.null(inv)){
                message ("getting cached data")
                return(inv)
        }
        else {
                data <- x$get()
                inv <- solve(data)
                x$setinvMatrix(inv)
                inv  
        }
        
}

## example matrixes attached. 
b<- c(1/2, -1/4,-1,3/4)
a<- matrix(b,2,2)
