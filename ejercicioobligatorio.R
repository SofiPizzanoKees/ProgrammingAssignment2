makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#makeCacheMatrix()
#Crea un objeto especial que:
#Guarda una matriz
#Guarda su inversa en caché (cuando se calcula)
#cacheSolve()
#Calcula la inversa de la matriz:

#Primero: Verifica si ya está en caché
#Si existe: La retorna directamente (rápido)
#Si NO existe: La calcula con solve(), la almacena y la retorna

#Objetivo:
 #Optimizar: Evitar recalcular la inversa múltiples veces (operación costosa)
#Concepto clave: Usa closures (scoping de R) para mantener datos en memoria entre llamadas.



#The assignment asks you to create two R functions that cache (store) the inverse of a matrix to avoid recalculating it repeatedly:
#makeCacheMatrix()
#Creates a special object that:

#Stores a matrix
#Stores its inverse in cache (when calculated)

#cacheSolve()
#Computes the inverse of the matrix:

#First: Checks if its already in cache
#If exists: Returns it directly (fast)
#If NOT exists: Calculates it with solve(), stores it, and returns it


#Objective:
#Optimize: Avoid recalculating the inverse multiple times (expensive operation)
#Key concept: Uses closures (Rs scoping rules) to keep data in memory between function calls. 