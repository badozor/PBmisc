getLimits <- function (dfxy, xax=1, yax=2,include.origin=TRUE,origin=c(0,0)){
df <- data.frame(dfxy)
if (!is.data.frame(df))
stop("Non convenient selection for df")
if ((xax < 1) || (xax > ncol(df)))
stop("Non convenient selection for xax")
if ((yax < 1) || (yax > ncol(df)))
stop("Non convenient selection for yax")
x <- df[, xax]
y <- df[, yax]
x1 <- x
if (include.origin)
x1 <- c(x1, origin[1])
x1 <- c(x1 - diff(range(x1)/10), x1 + diff(range(x1))/10)
xlim <- range(x1)
y1 <- y
if (include.origin)
y1 <- c(y1, origin[2])
y1 <- c(y1 - diff(range(y1)/10), y1 + diff(range(y1))/10)
ylim <- range(y1)
return(list(xlim=xlim, ylim=ylim))
}
