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









































