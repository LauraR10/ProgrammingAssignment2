## Estas funciones permiten almacenar en caché la inversa de una matriz para evitar
## cálculos redundantes y mejorar el rendimiento al calcular la inversa de matrices
## grandes o que se usan varias veces.

## La función makeCacheMatrix crea un objeto especial que puede almacenar una matriz 
## y su inversa en caché. Proporciona métodos para establecer y obtener la matriz,
## así como para establecer y obtener la inversa de la matriz.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  # Variable para almacenar la inversa de la matriz en caché
    
    ## Establece el valor de la matriz
    set <- function(y) {
        x <<- y
        m <<- NULL  # Resetea la inversa cuando se cambia la matriz
    }
    
    ## Obtiene el valor de la matriz
    get <- function() x
    
    ## Establece la inversa de la matriz
    setInverse <- function(inverse) m <<- inverse
    
    ## Obtiene la inversa de la matriz
    getInverse <- function() m
    
    ## Devuelve una lista con las funciones para manipular la matriz y su inversa
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## La función cacheSolve calcula la inversa de la matriz almacenada en el objeto
## creado por makeCacheMatrix. Si la inversa ya ha sido calculada previamente, la
## función la obtiene de la caché. Si no, la calcula y la guarda en la caché para
## futuros usos.

cacheSolve <- function(x, ...) {
    ## Obtiene la inversa de la matriz almacenada en caché
    m <- x$getInverse()
    
    ## Si la inversa ya está en caché, devuelve la caché
    if (!is.null(m)) {
        message("Obteniendo inversa desde la caché")
        return(m)
    }
    
    ## Si no está en caché, calcula la inversa
    data <- x$get()  # Obtiene la matriz
    m <- solve(data, ...)  # Calcula la inversa de la matriz usando la función solve
    
    ## Almacena la inversa en la caché
    x$setInverse(m)
    
    ## Devuelve la inversa calculada
    m
}

