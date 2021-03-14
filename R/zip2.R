
#' @export
# for testing, not optimised
read_direct <- function(zip, f, zl=NULL) {
  if(is.character(zip)) {
    con=file(zip, open='rb')
    on.exit(close(con))
    if(is.null(zl)) zl=zip_list(zip)
  } else {
    if(is.null(zl))
      stop("You must provide a ziplist when zip is a connection")
  }
  i=match(f, zl$filename)
  stopifnot(is.finite(i))
  stopifnot(length(i)==1)
  doffset=as.double(zl$offset[i])
  stopifnot(is.finite(doffset))
  seek(con, where=doffset)
  h=read_local_header(con)
  bytes=readBin(con, what=raw(), n=as.integer(zl$compressed_size[i]))
  ext=tools::file_ext(f)
  switch(ext,
         'rds'=unserialize(memDecompress(bytes, 'gz')),
         'qs'=qs::qdeserialize(bytes),
         bytes)
}

# returns byte offset of start of data
read_zip_header <- function(con) {
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



zip_list2 <- function(x) {
  check_zip_package()
  zl=zip::zip_list(x, extra = TRUE)
  con=file(x, open='rb')
  on.exit(close(con))
  zl$doffset=numeric(nrow(zl))
  for(i in seq_along(zl$offset)) {
    seek(con, where = zl$offset[i])
    zl$doffset[i]=read_zip_header(con)
  }
  zl
}


check_zip_package <- memoise::memoise(function() {
  argnames=names(formals(zip::zip_list))
  if("extra" %in% argnames) return(TRUE)
  stop("You must update to a new version of the zip package by doing:\n",
       'remotes::install_github("jefferis/zip@feature/ziplist-crc32-offset")')
})
