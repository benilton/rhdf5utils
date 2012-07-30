setClass('hdf5Container',
	representation(filename='character'))

setClass('hdf5Content',
         representation(name='character', filename='character', dim='integer'))

## METHODS

setGeneric('filename', function(object) standardGeneric('filename'))
setMethod('filename', 'hdf5Container',
	function(object) object@filename)
setMethod('filename', 'hdf5Content',
          function(object) object@filename)


setMethod('show', 'hdf5Container',
	function(object){
		message('hdf5Container')
		message('Filename: ', filename(object))
		print(h5ls(filename(object)))
	})

setMethod('names', 'hdf5Container',
          function(x) h5ls(filename(x))$name)
setMethod('names', 'hdf5Content',
          function(x) x@name)

setMethod('dim', 'hdf5Content',
          function(x) x@dim)

setMethod('show', 'hdf5Content',
          function(object){
              message('hdf5Content')
              message('Filename: ', filename(object))
              message('Content.: ', names(object))
              message('Dim.....: ', paste(dim(object), collapse=' x '))
          })

setMethod("$", "hdf5Container",
	function(x, name){
            info <- h5ls(filename(x))
            i <- which(info$name == name)
            info <- info[i,]
            dims <- as.integer(unlist(strsplit(info$dim, ' x ')))
            new('hdf5Content', name=name,
                filename=filename(x), dim=dims)
	})



## HELPERS
## File loader
hdf5LoadContainer <- function(fname){
    new('hdf5Container', filename=fname)
}

## File creator
hdf5Container <- function(fname){
	if (h5createFile(fname)){
		return(new('hdf5Container', filename=fname))
	}else{
		stop("File could not be created.")
	}
}

## Add empty matrix
hdf5AddArray <- function(object, name, dim, storage.mode){
	h5createDataset(filename(object),
                        name, dim,
                        storage.mode=storage.mode)
}


setMethod('[', 'hdf5Content',
          function(x, ..., drop=FALSE){
              dims <- dim(x)
              nDim <- length(dims)
              nArgs <- nargs()-as.integer(!missing(drop))-1
              if (nArgs != nDim)
                  stop('incorrect number of dimensions')
              if (missing(drop)) drop <- FALSE
              if (nDim == 1L){
                  if (missing(i)) i <- NULL
                  args <- list(i=i)
              }else if (nDim == 2L){
                  if (missing(i)) i <- NULL
                  if (missing(j)) j <- NULL
                  args <- list(i=i, j=j)
              }else if (nDim > 2L){
                  if (missing(i)) i <- NULL
                  if (missing(j)) j <- NULL
                  dots <- match.call(expand.dots=FALSE)[['...']]
                  for (ii in seq_along(dots))
                      if (dots[[ii]] == '')
                          dots[ii] <- list(NULL)
                  args <- c(list(i=i, j=j), dots)
              }
              obj <- h5read(filename(x), names(x), index=args)
              if (drop) return(drop(obj))
              return(obj)
          })

setReplaceMethod('[', signature(x='hdf5Content'),
                 function(x, ..., value){
                     dims <- dim(x)
                     nDim <- length(dims)
                     nArgs <- nargs()-2
                     if (nArgs != nDim)
                         stop('incorrect number of dimensions')
                     if (nDim == 1L){
                         if (missing(i)) i <- NULL
                         args <- list(i=i)
                     }else if (nDim == 2L){
                         if (missing(i)) i <- NULL
                         if (missing(j)) j <- NULL
                         args <- list(i=i, j=j)
                     }else if (nDim > 2L){
                         if (missing(i)) i <- NULL
                         if (missing(j)) j <- NULL
                         dots <- match.call(expand.dots=FALSE)[['...']]
                         for (ii in seq_along(dots))
                             if (dots[[ii]] == '')
                                 dots[ii] <- list(NULL)
                         args <- c(list(i=i, j=j), dots)
                     }
                     h5write(value, filename(x), names(x), index=args)
                 })

setReplaceMethod('$', signature(x='hdf5Container'),
                  function(x, name, value){
                      hdf5LoadContainer(filename(x))
                  })

setMethod('dimnames', signature(x='hdf5Content'),
          function(x)
          NULL)

