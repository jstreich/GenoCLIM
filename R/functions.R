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


###############################################################################
########################## Climate/Raster Functions ###########################
###############################################################################

extractCells <- function(x, th){
  c.prev <- xyFromCell(x, 1:ncell(x))
  c.prev.suit <- extract(x, cbind(c.prev[,1], c.prev[,2]))
  c.prev <- cbind(c.prev, c.prev.suit)
  c.prev <- c.prev[!is.na(c.prev[,3]), ]
  c.prev <- c.prev[c.prev[,3] > th, ]
  colnames(c.prev)[1:3] <- c("X", "Y", "Suitability")
  return(c.prev)
}


##### Heuristic Clustering
huerClust <- function(x, rps, sbst, seed, mn, mx){
  set.seed(seed)
  if(rps > 9999){
    rps = 9999
  }
  if(sbst > nrow(x)){
    sbst = round(0.9 * nrow(x))
  }
  rnms <- c(1:9999)
  rnms <- sample(rnms, replace = T, size = rps)
  for(j in 1:rps){
    set.seed(rnms[j])
    sbset <- x[sample(nrow(x), size = sbst), ]
    clust.loop <- Mclust(sbset, G = mn:mx)
    mbest.loop <- dim(clust.loop$z)[2]
    if(j == 1){
      data.p <- mbest.loop
    }
    else{
      data.p <- c(data.p, mbest.loop)
    }
  } # End j loop
  return(data.p)
}


##### Create pairwise function
cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X, method = "spearman")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  
  cor.mat <- t(R)
  cor.mat[upper.tri(cor.mat)] <- NA
  cor.mat
}

##### Convert numeric values to numeric damn values
matrix.numeric <- function(x){
  r.nms <- rownames(x)
  c.nms <- colnames(x)
  for(i in 1:nrow(x)){
    x.i <- as.numeric(as.character(as.matrix(x[i,])))
    if(i == 1){
      x.i.p <- x.i
    }
    else{
      x.i.p <- rbind(x.i.p, x.i)
    }
  }
  rownames(x.i.p) <- r.nms
  colnames(x.i.p) <- c.nms
  return(x.i.p)
}




