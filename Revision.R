#STAT295 1st lesson

1:100
2.35:50
#Use R as a calculator
1+5
3*5
100/5

1:6
mydice <- 1:6

aaa <- 555; aaa
AAA <- 666; AAA
aaa == AAA

aaa <- 777;aaa

ls()

mydice-1
mydice * mydice
mydice%*%mydice
mydice %o% mydice
dim(mydice) <- c(2,3);mydice

dim(mydice) <- c(1,2,3);mydice

m <- matrix(mydice, nrow = 2);m
m <- matrix(mydice, nrow = 2, byrow = T);m

myarray <- array(c(1:12), dim = c(2, 2, 3));myarray

now <- Sys.time()
now
class(now)
unclass(now)

data(iris)
head(iris)

class(iris)
class(iris$Sepal.Length)
class(iris$Species)
str(iris)
summary(iris)

iris$Sepal.Length

#indexing
iris[1,]
iris[,2]
iris[,"Species"]

table(iris$Species)

#Logical indexing
LogicIndex <- iris[, "Petal.Length"] > 5.5
LogicIndex

iris[LogicIndex,]

#summary statistics
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)

#functions
meanandstd <- function(x){
  c(mean=mean(x), std = sd(x))
}
meanandstd(iris$Sepal.Length)

stats <- aggregate(Sepal.Length ~ Species, data=iris, 
                   FUN=meanandstd)
stats
?aggregate

#visualization

plot(iris$Sepal.Length, col=as.numeric(iris$Species),
     ylab="Sepal Length")
legend('topleft', legend = levels(iris$Species), pch = 1:3)

boxplot(Sepal.Length ~ Species, data=iris)
