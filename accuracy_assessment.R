#### Accuracy assessment by Sam ####
# need to be rewritten to be more generalized applicable


install.packages("raster")
install.packages("rgdal")
library(raster)
library(rgdal)

### build confusion matrix based on own classification

setwd("path_to_working_directory")

classif_res<-raster("classification_result")
class_pix<-data.frame(sum(classif_res[]==1),sum(classif_res[]==2),sum(classif_res[]==3),sum(classif_res[])) 
class_pix

valid_res<-raster("validation_result")
valid_pix<-data.frame(sum(valid_res[]==1),sum(valid_res[]==2),sum(valid_res[]==3),sum(valid_res[]))
valid_pix

matrix.names <- list(c("water","forest","deforestation","no. of classified pixel"),c("water","forest","deforestation","no. of validation pixels"))
conf.matr <- matrix(NA,4,4,dimnames=matrix.names)

classes=1:3

for (i in classes){
  conf.matr[i,i] <- sum(classif_res[]==i & valid_res[]==i)
  conf.matr[4,4] <- sum(classif_res[]) # or valid_res[]
}
conf.matr

### rebuild confusion matrix from lecture slides
matrix.names <- list(c("water","forest","deforestation","no. of classified pixel"),c("water","forest","deforestation","no. of validation pixels"))
conf.matr <- matrix(c(55,9,3,67,3,78,1,81,2,6,81,89,60,93,85,247),4,4,dimnames=matrix.names)
conf.matr

### functions for accuracy assessment
## accuracy value for classes
ac.as.class <- function (x,y){
  ac <- x/y
  ac
}

# example
users.acc.water <- ac.as.class(55,60)
# or
users.acc.water <- ac.as.class(conf.matr[1,1],conf.matr[1,4])

prods.acc.water <- ac.as.class(55,67)
# or
prods.acc.water <- ac.as.class(conf.matr[1,1],conf.matr[4,1])

## overall accuracy value
ac.as.all <- function (a,b,c,d){
  ac <- (a+b+c)/d
  ac
}

# example
overall.acc <- ac.as.all(55,78,81,247)
# or
overall.acc <- ac.as.all(conf.matr[1,1],conf.matr[2,2],conf.matr[3,3],conf.matr[4,4])

## omission error, comission error
om.com.err <- function(z){
  err <- 1-z
  err
}

## generating a dataframe with results using a for-loop

result <- data.frame(c(NA,NA,NA,NA),c(NA,NA,NA,NA),row.names=c("water","forest","deforestation","overall"))
colnames(result) <- c("users.accuracy","producers.accuracy")
result

classes <- 1:3

for(i in classes){
  result[i,1] <- ac.as.class(conf.matr[i,i],conf.matr[i,4])
  result[i,2] <- ac.as.class(conf.matr[i,i],conf.matr[4,i])
  result[4,1] <- overall.acc
  result[4,2] <- overall.acc # ac.as.all(conf.matr[1,1],conf.matr[2,2],conf.matr[3,3],conf.matr[4,4])
}

result



