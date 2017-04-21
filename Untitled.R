
dependent <- "Sepal.Length"
independent <- "Sepal.Width + Petal.Width"
dependent + " , ~ " + independent

f1 <- function(x, y){
  x <- as.numeric(x)
  y <- as.numeric(y)
  r <- x+y
  return(r)
}
f1(1,4)


myvar <- 'Titanic'
myfun <- function(mydataset) {
  data(list=mydataset)   
  str(get(mydataset))
}
myfun(myvar)


d <- 'Sepal.Length'
i <- "Sepal.Width + Petal.Width"


formula <- function(d,i) {
  iris[d]
  }
formula(d)
