.__C__SparseMatrix <-
new("classRepresentation", slots = list(.Data = structure("matrix", package = "methods")), 
    contains = list(matrix = new("SClassExtension", subClass = structure("SparseMatrix", package = ".GlobalEnv"), 
        superClass = structure("matrix", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            .dm <- dim(from)
            .dn <- dimnames(from)
            attributes(from) <- NULL
            dim(from) <- .dm
            dimnames(from) <- .dn
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            from@.Data <- as(value, "matrix", strict = FALSE)
            from
        }, simple = TRUE, by = character(0), dataPart = TRUE, 
        distance = 1), array = new("SClassExtension", subClass = structure("SparseMatrix", package = ".GlobalEnv"), 
        superClass = structure("array", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            if (strict) 
                from <- from@.Data
            {
                class(from) <- "array"
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "matrix", TRUE)
            as(.value, "array") <- value
            value <- .value
            {
                from@.Data <- as(value, "matrix", strict = FALSE)
                from
            }
        }, simple = TRUE, by = structure("matrix", package = "methods"), 
        dataPart = FALSE, distance = 2), structure = new("SClassExtension", 
        subClass = structure("SparseMatrix", package = ".GlobalEnv"), 
        superClass = structure("structure", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            if (strict) 
                from <- from@.Data
            {
                from <- {
                  class(from) <- "array"
                  from
                }
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "matrix", TRUE)
            as(.value, "structure") <- value
            value <- .value
            {
                from@.Data <- as(value, "matrix", strict = FALSE)
                from
            }
        }, simple = TRUE, by = structure("matrix", package = "methods"), 
        dataPart = FALSE, distance = 3), vector = new("SClassExtension", 
        subClass = structure("SparseMatrix", package = ".GlobalEnv"), 
        superClass = structure("vector", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- as(from, "matrix", strict = strict)
            {
                from <- as(from, "array", strict = strict)
                as.vector(from)
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "matrix", TRUE)
            as(.value, "vector") <- value
            value <- .value
            {
                from@.Data <- as(value, "matrix", strict = FALSE)
                from
            }
        }, simple = FALSE, by = structure("matrix", package = "methods"), 
        dataPart = FALSE, distance = 4)), virtual = FALSE, prototype = <S4 object of class NULL>, 
    validity = NULL, access = list(), className = structure("SparseMatrix", package = ".GlobalEnv"), 
    package = ".GlobalEnv", subclasses = list(), versionKey = <pointer: 0x0>, 
    sealed = FALSE)
.requireCachedGenerics <-
structure(list("Ops", "[", "Math"), package = c("base", "base", 
"base"))
