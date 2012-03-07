# from package reshape
# Rename
# Rename an object
# 
# The rename function provide an easy way to rename the columns of a
# data.frame or the items in a list.
# 
# @arguments object to be renamed
# @arguments named vector specifying new names
# @keyword manip
#X rename(mtcars, c(wt = "weight", cyl = "cylinders"))
#X a <- list(a = 1, b = 2, c = 3)
#X rename(a, c(b = "a", c = "b", a="c")) 
#X 
#X # Example supplied by Timothy Bates
#X names <- c("john", "tim", "andy")
#X ages <- c(50, 46, 25)
#X mydata <- data.frame(names,ages)
#X names(mydata) #-> "name",  "ages"
#X 
#X # lets change "ages" to singular.
#X # nb: The operation is not done in place, so you need to set your 
#X # data to that returned from rename
#X 
#X mydata <- rename(mydata, c(ages="age"))
#X names(mydata) #-> "name",  "age"
rename <- function(x, replace) {
  replacement <-  replace[names(x)]
  names(x)[!is.na(replacement)] <- replacement[!is.na(replacement)]
  x
}

signString <- function(str, is.filename=T, initials='LP') {
  id <- function() paste("WM", gsub("-", "", Sys.Date()), sep="")

  format(Sys.time(), "%y%m%d")

  if(is.filename) {
    pattern <- '(^.*?)(\\.[^\\.]*)?$'
    replacement <- paste('\\1_',initials,format(Sys.time(), "%y%m%d"),'\\2',sep='')
    gsub(pattern,replacement,str)
  } else {
    paste(str,'_',initials,format(Sys.time(), "%y%m%d"),sep='')
  }
}

writeFunction <- function(FUN, fname=NULL) {
  funname <- as.character(match.call()[['FUN']])
  if(is.null(fname))
    fname <- paste(funname,'.R',sep='')
  dump(file = fname,list = funname)
}

resetDisplay <- function(dis) {
## reset display in a screen session
  Sys.setenv(DISPLAY=paste('localhost:',dis,'.0', sep=''))
}

wideScreen <- function(ncols) {
  if (missing(ncols))
    ncols <- as.integer(Sys.getenv("COLUMNS"))
  options(width=ncols);
}

