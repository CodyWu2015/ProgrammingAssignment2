# below functions implement caching & retrieving of matrix inverse calculation, which potentially can be very time consuming
# so if the inverse of the same matrix is requested multiple times, it will be only calculated first time with the result cached
# all later request will be satisfied immediately by returning directly the result cached earlier.
#
# Example to demonstrate the usage:
# 
# a) first we show what solve() would give us if we directly apply it to sample matrix c
#
#> c <- rbind(c(1,3), c(5,6))
#> c
#     [,1] [,2]
#[1,]    1    3
#[2,]    5    6
#> solve(c)
#           [,1]       [,2]
#[1,] -0.6666667  0.3333333
#[2,]  0.5555556 -0.1111111
#
#> solve(c) %*% c
#              [,1] [,2]
#[1,]  1.000000e+00    0
#[2,] -1.110223e-16    1
#
# b) next we create the object m that represents the cache
#
#> m <- makeCacheMatrix(c)
#
# c) we can apply cacheSolve() unto m to show the identical result to solve(c)
#
#> cacheSolve(m)
#           [,1]       [,2]
#[1,] -0.6666667  0.3333333
#[2,]  0.5555556 -0.1111111
#
# d) if we later call cacheSolve() multiple times it will display message "getting cached data" with the same result returned
#
#> cacheSolve(m)
#getting cached data
#           [,1]       [,2]
#[1,] -0.6666667  0.3333333
#[2,]  0.5555556 -0.1111111
#> cacheSolve(m)
#getting cached data
#           [,1]       [,2]
#[1,] -0.6666667  0.3333333
#[2,]  0.5555556 -0.1111111
#
# N.B. the function does least argument sanity check assuming all input matrix are invertible


# makeCacheMatrix() takes a raw matrix input and returns a list representing a cache object
# 
# example:
#> l <- makeCacheMatrix(matrix(4:7, 2, 2))
#> attributes(l)
#$names
#[1] "set"    "get"    "setinv" "getinv"
#
#> class(l)
#[1] "list"
# 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# cacheSolve() takes cache objected returned from makeCacheMatrix() and return the inversion of matrix embedded in it
# any additional arguments will be passed to solve() which is ultimately called to find the inverse
# 
# example:
#> cacheSolve(makeCacheMatrix(matrix(4:7, 2, 2)))
#     [,1] [,2]
#[1,] -3.5    3
#[2,]  2.5   -2
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
