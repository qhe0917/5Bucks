library(tidyverse)
attach(mtcars)
library(jsonlite)
library(utils)
jason<- toJSON(head(mtcars,n=3), pretty=TRUE)

a<-c('[{"P0010001":710231,"NAME":"Alaska","state":"02"},
  {"P0010001":4779736,"NAME":"Alabama","state":"01"},
  {"P0010001":2915918,"NAME":"Arkansas","state":"05"},
  {"P0010001":6392017,"NAME":"Arizona","state":"04"},
  {"P0010001":37253956,"NAME":"California","state":"06"}]')

fromJSON(a)
dir<- getwd
b<-fromJSON("https://api.census.gov/data/2010/surname?get=NAME,COUNT&RANK=1:100")
dim(b)
c<-toJSON(b, pretty=TRUE)

ope
