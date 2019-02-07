library(classdata)
samp<-c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")
samf<- as.factor(samp)
levels(samf)
samf<- factor(samp,levels=samp[c(7,1:6)])
levels(samf)

data("eurodist")
str(eurodist)

mod<-glm(mpg~cyl+disp+hp+drat+wt, data=mtcars)
mode(mod)

install.packages("repurrrsive")
library(repurrrsive)
data("got_chars")
str(got_chars)

got_chars[2]

got_chars[[2]][["alive"]]


i=1
alive<-NULL
for (i in 1:30){
  if (got_chars[[i]][["alive"]]==TRUE)
  {print(got_chars[[i]][["name"]])
  alive<-c(alive,got_chars[[i]][["name"]])
  }
}

got_chars[]

read.fwf()
