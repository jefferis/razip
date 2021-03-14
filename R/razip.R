#' @import R6
RAZip <- R6Class(
  "RAZip",
  list(
    zipfile = NA_character_,
    initialize = function(zipfile, unserialize=NULL) {
      self$zipfile <- zipfile
      private$zl = zip::zip_list(zipfile)
      stopifnot("offset" %in% names(private$zl))
      stopifnot(is.null(unserialize) || is.function(unserialize))
      private$unserialize = unserialize
    },
    finalize = function() {
      if(private$isopen())
        close(private$con)
    },
    getbytes=function(f) {
      private$open()
      zl=private$zl
      i=match(f, zl$filename)
      stopifnot(is.finite(i))
      stopifnot(length(i)==1)
      doffset=zl$offset[i]
      stopifnot(is.finite(doffset))
      seek(private$con, where=doffset)
      h=read_local_header(private$con)
      bytes=readBin(private$con, what=raw(), n=as.integer(zl$compressed_size[i]))
      bytes
    },
    get=function(f) {
      bytes=self$getbytes(f)
      if(is.function(private$unserialize)) {
        private$unserialize(bytes)
      } else {
        ext=tools::file_ext(f)
        # we could remove the qs from the default implementation
        switch(ext,
               'rds'=unserialize(memDecompress(bytes, 'gz')),
               'qs'=qs::qdeserialize(bytes),
               stop("Unrecognised data format in zip!")
        )
      }
    },
    ziplist=function() {
      private$zl
    }
  ),
  private = list(
    zl = NULL,
    con = NULL,
    unserialize=NULL,
    isopen = function() isTRUE(try(isOpen(private$con, rw = 'r'), silent = T)),
    open = function() {
      if(is.null(private$con) || !private$isopen())
        private$con=file(self$zipfile, open='rb')
    }
  )
)
