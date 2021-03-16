
#' Read data from one zip file entry
#'
#' @details When \code{unserialize=TRUE} the data must a) have been written to
#'   disk with \code{\link{saveRDS}} using gzip, bzip2 or xz compression or
#'   \code{qs::\link[qs]{qsave}} AND b) zip must have been instructed \emph{not}
#'   to further compress the data (which would be pointless). This is the
#'   behaviour of
#'   \href{https://natverse.org/nat/reference/write.neurons.html}{\code{write.neurons}}
#'    in the \href{https://natverse.org/nat/}{nat package}.
#'
#' @param zip The path to the zip file or an open connection
#' @param f The file to read from inside the zip
#' @param zl A zip list as produced by
#' @param unserialize Whether to unserialize the data (from rds or qs format).
#'   Otherwise raw bytes are returned.
#' @return A \code{raw} array, which may have been unserialized.
#' @export
#' @importFrom zip zip_list
read_zip_entry <- function(zip, f, zl=NULL, unserialize=FALSE) {
  if(is.character(zip)) {
    con=file(zip, open='rb')
    on.exit(close(con))
    if(is.null(zl)) {
      check_zip_package()
      zl=zip_list(zip)
    }
  } else {
    con=zip
    if(is.null(zl))
      stop("You must provide a ziplist when zip is a connection")
  }
  i=match(f, zl$filename)
  if(!isTRUE(is.finite(i)))
    stop("Unable to find ",f, " in zip file: ", zip)
  if(!isTRUE(length(i)==1))
    stop("Unable to find exactly one match for ",f, " in zip file: ", zip)
  if(!isTRUE(is.finite(zl$offset[i])))
    stop("Unable to find valid data offset for ",f, " in zip file: ", zip)

  seek(con, where=zl$offset[i])
  h=read_local_zip_header(con)
  bytes=readBin(con, what=raw(), n=as.integer(zl$compressed_size[i]))
  ext=tools::file_ext(f)

  if(unserialize) {
    switch(ext,
           'qs'=qs::qdeserialize(bytes),
           unserialize(memDecompress(bytes, 'unknown')))
  } else bytes
}

# returns byte offset of start of data
read_local_zip_header <- function(con) {
  if(is.character(con)) {
    con=file(con, open='rb')
    on.exit(close(con))
  }
  fixedbytes=readBin(con, what=raw(), n = 30L)
  stopifnot(isTRUE(all(fixedbytes[1:2]==charToRaw("PK"))))
  if(all(fixedbytes[19:22]==as.raw(c(0xFF,0xFF, 0xFF, 0xFF))))
     stop("zip64 not yet supported!")
  filenamelen=readBin(fixedbytes[27:28], what='int', size=2, endian='little' )
  extralen=readBin(fixedbytes[29:30], what='int', size=2, endian='little' )
  filename=readChar(con, nchars=filenamelen, useBytes=T)
  if(extralen>0)
    readBin(con, what = raw(), n=extralen)
  seek(con)
}


# Private function that calculates offsets to actual data
# (as opposed to start of the zip local header)
# For testing - we will normally just read the local header on the fly
zip_list2 <- function(x) {
  check_zip_package()
  zl=zip::zip_list(x)
  con=file(x, open='rb')
  on.exit(close(con))
  zl$doffset=numeric(nrow(zl))
  for(i in seq_along(zl$offset)) {
    seek(con, where = zl$offset[i])
    zl$doffset[i]=read_local_zip_header(con)
  }
  zl
}

# make sure we have the zip package
check_zip_package <- memoise::memoise(function() {
  zip=system.file("sampledata/Cell07PNs-rds.zip", package = 'razip')
  zl=zip::zip_list(zip)
  ok = "offset" %in% colnames(zl)
  if(isTRUE(ok)) return(TRUE)
  stop(call. = FALSE, "razip: You must update to a new version of the zip package:\n",
       'remotes::install_github("r-lib/zip")')
})
