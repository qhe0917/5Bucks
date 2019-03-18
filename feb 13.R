
loglikpois<- function(lambda = NA, x=NA){
  if (sum(x < 0)<0){stop()}
  if (lambda == 0){stop("lambda must be positve")}
  lik<--length(x)*lambda+sum(x)*log(lambda)
  return(lik)
}

loglikpois(1.6,c(1,3,2,3,0,1,0,1,3,3))
X=c(1,3,2,3,0,1,0,1,3,3)

lmda<- seq(from=0.1, to=10, by=0.1)
dframe<- data.frame(lambda=lmda, L= sapply(lmda, FUN = loglikpois, x=X))
library(tidyverse)
dframe %>% 
  ggplot(aes(x=lambda, y = L)) + geom_point()
