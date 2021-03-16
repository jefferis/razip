#' R6 class implementing fast random access to zip files.
#'
#' @import R6
#' @export
#' @examples
#' \donttest{
#' smp=RAZip$new(system.file("sampledata/Cell07PNs-rds.zip", package = 'razip'))
#' smp

#' }
#' \dontrun{
#' raz=RAZip$new("~/Desktop/flywire_neurons_flow_FlyWireqs.zip")
#' raz
#' zl=raz$ziplist()
#' bench::mark(s1=raz$get(sample(zl$filename, 1)), check = F)
#' }
RAZip <- R6Class(
  "RAZip",
  list(
    #' @field zipfile the path to the zip file
    zipfile = NA_character_,

    #' @description Initialise an RAZip object
    #' @param zipfile path to the zip file
    #' @param unserialize Optional function to be applied to raw bytes from the
    #'   zip file. When missing R's \code{unserialize} or \code{qs::deserialize}
    #'   is used.
    initialize = function(zipfile, unserialize=NULL) {
      self$zipfile <- zipfile
      check_zip_package()
      private$zl = zip::zip_list(zipfile)
      stopifnot(is.null(unserialize) || is.function(unserialize))
      private$unserialize = unserialize
    },
    #' @description read raw bytes for a zip entry
    #' @param f A file inside the zip to read.
    getbytes=function(f) {
      private$open()
      bytes=read_zip_entry(private$con, f, zl=private$zl, unserialize = F)
      bytes
    },
    #' @description read an R object from a zip entry
    #' @param f A file inside the zip to read.
    get=function(f) {
      if(is.function(private$unserialize)) {
        bytes=self$getbytes(f)
        private$unserialize(bytes)
      } else {
        private$open()
        read_zip_entry(private$con, f, zl=private$zl, unserialize = T)
      }
    },
    #' @description read multiple R objects from a zip file
    #' @param f One or more files inside the zip to read
    #' @param ... additional arguments passed to \code{pbapply::\link{pbsapply}}
    #' @return A named \code{list}
    mget=function(f, ...) {
      pbapply::pbsapply(f, self$get, simplify = FALSE, ...)
    },
    #' @description Return the zip info from \code{zip::zip_list}
    #' @return A named \code{list}
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
    },
    finalize = function() {
      if(private$isopen())
        close(private$con)
    }
  )
)

#' @export
print.RAZip <- function(x, ...) {
  cat("RAZip object wrapping:", x$zipfile, "which contains", nrow(x$ziplist()), "files.")
}
