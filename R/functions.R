

checkDependencies<-function()
{
  print('Dependencies on ggplot2, gridExtra, grid')
  check<-as.data.frame(installed.packages())
  check<-data.frame(check$Package,1)
  if(check$X1[check$check.Package=='ggplot2']==1)
  {
    print('calling ggplot2')
    library(ggplot2)
  }else
  {
    print('installing ggplot2')
    install.packages('ggplot2')
    print('calling ggplot2')
    library(ggplot2)
  }

  if(check$X1[check$check.Package=='gridExtra']==1)
  {
    print('calling gridExtra')
    library(gridExtra)
  }else
  {
    print('installing gridExtra')
    install.packages('gridExtra')
    print('calling gridExtra')
    library(gridExtra)
  }



  if(check$X1[check$check.Package=='grid']==1)
  {
    print('calling grid')
    library(grid)
  }else
  {
    print('installing grid')
    install.packages('grid')
    print('calling grid')
    library(grid)
  }

}

#library(ggplot2)
#library(gridExtra)
#library(grid)

CLT.generator <- function(n, K) { # definition of CLM method
  U <- runif(n * K, min = 0, max = 1) # sample n*K numbers from unifrom distribution
  Y <- rowMeans(matrix(U, nrow = n, ncol = K)) # assemble the U vector into the matrix
  # of n rows and K columns,
  # and calculate mean of each row
  return(Y) # output is the vector Y of length n which is output of CLM method and
  # therefore normally distributed
}

BoxMuller.generator <- function(n) { # definition of Box-Muller method
  U1 <- runif(n, min = 0, max = 1) # sample n numbers from uniform distribution
  U2 <- runif(n, min = 0, max = 1) # sample n numbers from uniform distribution
  a <- sqrt (-2 * log(U1)) # calculate variable a
  # see the reference, enclosed sd pdf, Chapter 2.2., Algorithm 1
  b <- 2 * pi * U2 # calculate variable b
  # see the reference, enclosed as pdf, Chapter 2.2., Algorithm 1
  Y <- a * sin(b) # calculate variable Y
  # see the reference, Chapter 2.2., Algorithm 1
  # vector Y is the first vector of normally distributed numbers of lenght n
  Y2 <- a * cos(b) # vector Y is the second vector of normally distributed numbers of lenght n
  # note: Box-Muller method provides two gaussian distributions using two
  # input uniform distibutions, here we use only one resulting gaussian
  # distribution as output
  return(Y) # output is the vector Y of length n which is output of Box-Muller method and
  # therefore normally distributed
}

Leva.generator <- function(n) { # definition of Leva method
  s <- 0.449871
  t <- -0.386595
  r1 <- 0.27597
  r2 <- 0.27846
  a <- 0.196
  b <- 0.25472 # values of parameters s, t, r1, r2, a, and b were obtained in the reference,
  # Chapter 2.3., Algorithm 6
  U1 <- runif(n, min = 0, max = 1) # sample n numbers from uniform distribution
  V1 <- runif(n, min = -1, max = 1) # sample n numbers from uniform distribution, minimal value is -1
  u <- U1
  v <- sqrt(2 / exp(1)) * V1
  x <- u - s
  y <- abs(v) - t
  Q <- x ^ 2 + y * (a * y - b * x)
  Y <- 0
  for (i in 1:n) {
    u <- U1[i]
    v <- sqrt(2 / exp(1)) * V1[i]
    x <- u - s
    y <- abs(v) - t
    Q <- x ^ 2 + y * (a * y - b * x)
    if (Q < r1) {
      Y[i] <- v / u
    } else if (Q < r2) {
      if (v ^ 2 < 4 * u ^ 2 * log(u)) {
        Y[i] <- v / u
      }
    }
  }
  Y <- Y[!is.na(Y)] # all transformations above are performed exactly as given in reference,
  # Chapter 2.3., Algorithm 6
  return(Y) # output is the vector Y which is output of Leva method and
  # therefore normally distributed
}

create.plots <- function(Y, title, n, time) { # definition of function for plotting results
  p1 <- qplot(Y, geom = "histogram", bins = 50, main = "Histogram") # plot given vector Y as histogram and save it to variable p1
  p2 <- qqplot.data(Y) # plot q-q plot of given vector Y using function from this file, line 70, and save it to variable p2
  grid.arrange(p1, p2, ncol=2,
               top = textGrob(paste(title,
                                    ", n =",toString(n),
                                    ", runtime =",toString(time),
                                    "s"),
                              gp=gpar(fontsize=18,font=8))) # arrange the two plots next to each other
  # and add runtime of the method to the title of the plot
}

qqplot.data <- function(vec) { # definition of function for q-q plot
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75)) # calculate first and third quantile of the given data
  x <- qnorm(c(0.25, 0.75)) # calculate first and third quantile of normal distribution
  slope <- diff(y)/diff(x) # calculate slope of the red line
  int <- y[1L] - slope * x[1L] # calculate intercept of the red line
  d <- data.frame(resids = vec) # convert given data to data.frame format
  ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int, colour="red") + ggtitle("Q-Q plot") # plot the given data together with the red line
}
