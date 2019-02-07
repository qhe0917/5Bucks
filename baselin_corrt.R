baseline_corrt <- function(data, method ='modpolyfit', degree = 3, tol = 0.001, 
                          rep = 100) {

  data_orig <- data
  nocol <- dim(data_orig)[2]
  if (length(data$sample) == 0) {
    flag <- (data_orig$group == data_orig$group[1]) & (data_orig$spec.no == 
                                                         data_orig$spec.no[1])
  }
  if (length(data$sample) != 0) {
    flag <- (data_orig$group == data_orig$group[1]) & (data_orig$spec.no == 
                                                         data_orig$spec.no[1]) & (data_orig$sample == data_orig$sample[1])
  }
  wt_no <- data_orig$WtNo[flag]
  data <- t(data$Intensity)
  n <- length(wt_no)
  spec_no <- dim(data)[2]/length(wt_no)
  np <- dim(data)
  yrslt <- NA * data[1, ]
  yold <- NA * data[1,1:n ]
  ywork <- NA * data[1,1:n ]
  ypred <- NA * data[1,1:n ]
  
  
    for (j in 1:spec_no) {
      ywork[1:n]<- data[(n * (j - 1) + 1):(n * j)]
      yold <- as.matrix(t(ywork))
      ypred <- baseline(yold,method = method, degree = degree, tol = tol, 
                                  rep = rep)
      yrslt[(n * (j - 1) + 1):(n * j)] <- slot(ypred, "corrected")

    }
plot(ypred)
  corrected <- rbind(data_orig$WtNo,yrslt)
  return(corrected)
}