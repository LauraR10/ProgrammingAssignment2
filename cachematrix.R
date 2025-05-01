## makeCacheMatrix crea un objeto especial que almacena una matriz
## y permite almacenar en caché su inversa. Las funciones dentro de 
## este objeto permiten obtener y establecer la matriz y su inversa.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable para almacenar la inversa de la matriz
  
  # Función para establecer el valor de la matriz
  set <- function(y) {
    x <<- y      # Asigna la nueva matriz
    inv <<- NULL # Resetea la inversa cacheada
  }
  
  # Función para obtener el valor de la matriz
  get <- function() x
  
  # Función para establecer el valor de la inversa
  setinverse <- function(inverse) inv <<- inverse
  
  # Función para obtener el valor de la inversa
  getinverse <- function() inv
  
  # Devuelve una lista con las funciones anteriores para operar sobre la matriz
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve calcula la inversa de una matriz. Si la inversa ya está 
## cacheada (es decir, ya se ha calculado anteriormente y la matriz no ha cambiado), 
## la función simplemente devuelve la inversa almacenada. Si no, calcula la inversa 
## y la almacena en caché.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Obtiene la inversa cacheada, si existe
  
  # Si la inversa ya está en la caché, se devuelve
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Si la inversa no está en caché, se calcula
  data <- x$get()         # Obtiene la matriz
  inv <- solve(data, ...) # Calcula la inversa de la matriz
  x$setinverse(inv)       # Almacena la inversa en caché
  inv                    # Devuelve la inversa
}
