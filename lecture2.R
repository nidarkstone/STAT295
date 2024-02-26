#Lecture 2

#Exceptions and Timings

vec <- c(1,2,3)
vec2 <- c(1,2)
vec*vec2 #give waning message but still give answer

warn_test <- function(x){
  if(x<=0){
    warning("WATCH OUT! 'x' is less than or equal to 0. Set it 1.")
    x<-1
  }
  return(2/x)
}
warn_test(10)
warn_test(0)

error_test <-  function(x){
  if(x<=0){
    stop("'x' is less than or equal to 0... TERMINATE!")
  }
  return(x/2)
}
error_test(0)

myfibrec <- function(n){
  if(n<0){
    warning("Assuming you meant 'n' to be positive -- doing that instead")
    n <- n*-1
  } else if(n==0){
    stop("'n' is uninterpretable at 0")
  }
  if(n==1||n==2){
    return(1)
  }else {
    return(myfibrec(n-1)+myfibrec(n-2))
  }
}
myfibrec(-3)
myfibrec(0)
myfibrec(6)

try(myfibrec(0), silent = T)
attempt1 <- try(myfibrec(0), silent = TRUE);attempt1
class(attempt1) #try-error

attempt2 <- try(myfibrec(6), silent = T);attempt2

myfibrecvector <-  function(nvec){
  nterms <- length(nvec)
  result <- rep(0, nterms)
  for (i in 1:nterms) {
    result[i]<- myfibrec(nvec[i])
  }
  return(result)
}

foo <- myfibrecvector(nvec = c(1,2,10,8,-4));foo
bar <- myfibrecvector(nvec = c(3,2,7,8,0,11,2));bar #does not show any answer due to 0

myfibrecvectorTRY <-  function(nvec){
  nterms <- length(nvec)
  result <- rep(0, nterms)
  for (i in 1:nterms) {
    attempt <- try(myfibrec(nvec[i]), silent = TRUE)
    if(class(attempt)== "try-error"){
      result[i]<- NA
    } else {
      result[i]<- attempt
    }
  }
  return(result)
}

baz <- myfibrecvectorTRY(nvec = c(3,2,7,8,0,11,2));baz

myfibrec(-3)
attempt3 <- suppressWarnings(myfibrec(-3));attempt3 #does not show warning message

#Progress and Timing

## Textual Progress Bars

Sys.sleep(10)

sleep_test <- function(n){
  result <- 0 
  for (i in 1:n) {
    result <- result + 1
    Sys.sleep(0.5)
  }
  return(result)
}

sleep_test(8)

prog_test <- function(n){
  result <- 0
  progbar <- txtProgressBar(min = 0, max = n, style = 1,
                            char = "=")
  for (i in 1:n) {
    result <- result + 1
    Sys.sleep(0.5)
    setTxtProgressBar(progbar, value = i)
  }
  close(progbar)
  return(result)
}
prog_test(3)
prog_test(15)

#Measuring completion time
Sys.time()

t1 <- Sys.time()
Sys.sleep(3) #3 saniye durdur demek
t2 <- Sys.time()
t2 - t1

#Avoid for loops

d <- as.data.frame(cbind(runif(10000),runif(10000)))
head(d)
system.time(for(loop in 1:dim(d)[1]){
  d$mean2[loop]<-mean(c(d[loop,1], d[loop,2]))
})

system.time(d$mean1 <- apply(d, 1, mean)) #faster

df <- 1:10
df
lapply(2:3, function(i) df <- df*i)
#lapply ve for loop farklı sonuçlar veriyor
df <- 1:10
for (i in 2:3) {
  df <- df*i
}
df

install.packages("reshape2")
data(tips, package = "reshape2")
head(tips)

tips$tipgroup <- ifelse(tips$tip < 3, "lowtip", "hightip" )
head(tips)

timecal <- function(n){
  a <- numeric(n)
  for (i in 1:n){
    a[i]<-2*pi*sin(i)
  }
}
system.time(timecal(100000))

timecal2 <- function(n){
  a <- numeric(n)
  for (i in 1:n) 
    a[i] <- sin(i)
  2 * pi * a
}
system.time(timecal2(100000))

#Piping in R

library(tidyverse)
#shift + command + m %>% or |>
data(tips, package = "reshape2")
head(tips)

tips %>% 
  subset(total_bill>19) %>% 
  aggregate(.~sex, .,mean)

tips |>
  subset(total_bill>19)|>
  {function(x) aggregate(.~sex, data = x,mean)}()
#they give same output but syntax different

tips %>% 
  subset(total_bill>19) %>% 
  aggregate(.~day, .,max)
#1st way too long
a <- rnorm(10)
a
a1 <- abs(a)
a1
a2 <- log(a1)
a2
a3 <- round(a2,1)
a3
#2nd way better
round(log(abs(a)),1) #inside to outside
#3rd way better
a %>% abs() %>% 
  log() %>% 
  round(1)

a %<>% abs() %>% 
  log() %>% 
  round(1) #change a 

a <- rnorm(15)
a
round(a,2)
#or
a %>% round(2)

assign("a", pi)
a

"a" %>% assign(20)
a #does not change

env <- environment()
"a" %>% assign(20, envir = env)
a

rnorm(100) %>% 
  matrix(ncol = 2) %>% 
  plot() %>% 
  str() #does not show str

rnorm(100) %>% 
  matrix(ncol = 2) %T>% 
  plot() %>% 
  str() #now we can see str

































