data(meuse)
a <- meuse[,1:4]
rownames(a) = NULL
image.orig <- image

library(gstat)

#Exploratory Analysis
stem(a$copper)
boxplot(a$copper)
hist(a$copper)
stem(a$zinc)
boxplot(a$zinc)
hist(a$zinc)
summary(a)

#Log Transformation
log_copper <- log10(a$copper)
log_zinc <- log10(a$zinc)
stem(log_copper)
boxplot(log_copper)
hist(log_copper)
stem(log_zinc)
boxplot(log_zinc)
hist(log_zinc)


#Create a gstat object;
g <- gstat(id="log_copper", formula = log(copper)~1, locations = ~x+y,data = a)

#Plot the variogram:
plot(variogram(g), main="Semivariogram of the log_copper")

#Fit a model variogram to the sample variogram:
v.fit <- fit.variogram(variogram(g), vgm(0.5,"Sph",1000,0.1))

plot(variogram(g),v.fit)

#Use kriging to estimate the value of log(copper) at the grid values.
#First we create the grid.
x.range <- as.integer(range(a[,1]))
x.range
y.range <- as.integer(range(a[,2]))
y.range
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=50),y=seq(from=y.range[1], to=y.range[2], by=50))

#We want now to use kriging to predict log(copper) at each point on the grid:
pr_ok <- krige(id="log_copper",log(copper)~1, locations=~x+y,model=v.fit, data=a, newdata=grd)


#To find what the object pr_ok contains type:
names(pr_ok)

#To see the predicted values you type:
pr_ok$log_copper.pred
#And the kriging variances:
pr_ok$log_copper.var

plot(variogram(g), main="Semivariogram of the log_copper")

plot(variogram(g),v.fit)

plot(grd)


library(scatterplot3d)
scatterplot3d(pr_ok$x, pr_ok$y, pr_ok$log_copper.pred, main="Predicted values")

library(lattice)
levelplot(pr_ok$log_copper.pred~x+y,pr_ok, aspect ="iso",main="Ordinary kriging predictions")

levelplot(pr_ok$log_copper.var~x+y,pr_ok, aspect ="iso",main="Ordinary kriging variances")

levelplot(sqrt(pr_ok$log_copper.var)~x+y,pr_ok, aspect ="iso",main="Ordinary kriging standard errors")

vec.2.Mx <- function( xvec, nrow, ncol ) {
  Mx.out <- matrix(0, nrow, ncol )
  for(j in 1:ncol) {
    for(i in 1:nrow) {
      Mx.out[i, j] <- xvec[ (j-1)*nrow + i ]
    }
  }
  return( Mx.out )
}

qqq <- vec.2.Mx( pr_ok$log_copper.pred,
                 length(seq(from=x.range[1], to=x.range[2], by=50)),
                 length(seq(from=y.range[1], to=y.range[2], by=50)) )


qqq <- matrix(pr_ok$log_copper.pred,
              length(seq(from=x.range[1], to=x.range[2], by=50)),
              length(seq(from=y.range[1], to=y.range[2], by=50)))

image.orig(seq(from=x.range[1], to=x.range[2], by=50),
           seq(from=y.range[1], to=y.range[2], by=50), qqq,
           xlab="West to East", ylab="South to North", main="Predicted values")

points(a) #The data points can be plotted on the raster map.

contour(seq(from=x.range[1], to=x.range[2], by=50),seq(from=y.range[1],to=y.range[2], by=50), qqq, add=TRUE, col="black", labcex=1)

qqq <- vec.2.Mx( pr_ok$log_copper.var,
                 length(seq(from=x.range[1], to=x.range[2], by=50)),
                 length(seq(from=y.range[1], to=y.range[2], by=50)) )

image.orig(seq(from=x.range[1], to=x.range[2], by=50),
           seq(from=y.range[1], to=y.range[2], by=50), qqq,
           xlab="West to East", ylab="South to North", main="Kriging variances")

qqq <- vec.2.Mx( sqrt(pr_ok$log_copper.var),
                 length(seq(from=x.range[1], to=x.range[2], by=50)),
                 length(seq(from=y.range[1], to=y.range[2], by=50)) )
image.orig(seq(from=x.range[1], to=x.range[2], by=50),
           seq(from=y.range[1], to=y.range[2], by=50), qqq,
           xlab="West to East", ylab="South to North", main="Kriging standard errors")

