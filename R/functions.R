################################################################################
############################### Common Functions ###############################
################################################################################

# Name-based Cropping
nm <- function(x, y){
  n.m <- match(rownames(x), rownames(y))
  n.b <- cbind(n.m, x)
  n.c <- n.b[complete.cases(n.b[,1]), ]
  n.c <- n.c[,2:ncol(n.c)]
  return(n.c)
}

##### Order by rownames
order_Rnames <- function(x,y){
  x.o <- match(rownames(x), rownames(y))
  x.s <- x[order(x.o), ]
  return(x.s)
}

##### Order by colnames
order_Cnames <- function(x,y){
  x.t <- t(x)
  y.t <- t(y)
  x.o <- match(rownames(x), rownames(y))
  x.s <- x[order(x.o), ]
  return(t(x.s))
}

##### Rownames check
rch <- function(x, y){
  plot(match(rownames(x), rownames(y)))
}

##### get x from y based on two specific columns
rows_by_crop_col <- function(x, xc, y, yc){
  n.m <- match(x[,xc], y[,yc])
  n.b <- cbind(n.m, x)
  n.c <- n.b[complete.cases(n.b[,1]), ]
  n.c <- n.c[,2:ncol(n.c)]
  return(n.c)
}

##### order x by y from two specific columns
order_by_col <- function(x, xc, y, yc){
  x.o <- match(x[,xc], y[,yc])
  x.s <- x[order(x.o), ]
  return(x.s)
}

##### Get samples in x not in y
om <- function(x, y){
  n.m <- !match(rownames(x), rownames(y))
  n.b <- cbind(n.m, x)
  n.c <- n.b[complete.cases(n.b[,1]), ]
  n.c <- n.c[,2:ncol(n.c)]
  return(n.c)
}


##### Simplified write file function
write.text <- function(x, nm, rn, cn){
  write.table(x, nm, row.names = rn, col.names = cn, quote = F, sep = "  ")
}
