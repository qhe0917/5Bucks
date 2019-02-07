##' Spectra data file transformation from the long format data frame to wide format list
##'
##' This function transform the long format data format with columns group, spec.no,
##' WtNo and Intensity
##' to wide format list with 'sp'(wide format data frame with spectra intensity, group, (sample)
##' and spec.no information)
##' and 'WtNo' (wavenumber)
##' @author Qi Wang
##' @param data long format data frame with 4 or 5 variables: group, (sample),
##' spec.no, WtNo and Intensity
##' @return a wide format list with 'sp'(wide format data frame with spectra intensity,
##' group, (sample) and spec.no information) and 'WtNo' (wavenumber)
##' @example # transform the long formated file to wide formated file
a<-read.csv(file.choose())
sp_wide_beef <- trans_wide(fe.long)
`##' sp_wide_dog <- trans_wide(sp_dog)
data<-a
length(a$sample)
trans_wide <- function(data) {
  if (length(data$sample) != 0) {
  flag <- (data$sample == data$sample[1]) & (data$group ==
    data$group[1]) & (data$spec.no == data$spec.no[1])
  XVector <- data$WtNo[flag]
  n <- length(XVector)
  A <- unique(paste(data$sample, data$group, data$spec.no,
                    sep = ","))
  MaxClass <- length(A)
  x <- data.frame(Intensity = matrix(0, MaxClass, n))
  x$sample <- NA
  x$group <- NA
  x$spec.no <- NA
  cat(nrow(x), " ", ncol(x), "\n")
  dim(data)
  sm <- unique(data$sample)
  gp <- unique(data$group)
  spno <- unique(data$spec.no)
  indx <- 0
  for (j in 1:length(gp))
    for (i in 1:length(sm))
      for (k in 1:length(spno)) {
        if (length(unique(data$sample == sm[i] & data$group == gp[j] & data$spec.no == spno[k])) == 2) {
          indx <- indx + 1
          cat(indx, "\n")
          m <- length(data$Intensity[data$sample ==
            sm[i] & data$group == gp[j] & data$spec.no ==
            spno[k]])
          x[indx,1:m] <- as.vector(data$Intensity[data$sample ==
            sm[i] & data$group == gp[j] & data$spec.no ==
            spno[k]])

          x$group[indx] <- j
          x$spec.no[indx] <- k
          x$sample[indx]<- i
        }
      }
}
else {
  flag <- (data$group == data$group[1]) & (data$spec.no ==
    data$spec.no[1])
  XVector <- data$WtNo[flag]
  n <- length(XVector)
  A <- unique(paste(data$group, data$spec.no, sep = ","))
  MaxClass <- length(A)
  x <- data.frame(Intensity = matrix(0, MaxClass, n))
  x$group <- NA
  x$spec.no <- NA
  cat(nrow(x), " ", ncol(x), "\n")
  dim(data)
  gp <- unique(data$group)
  spno <- unique(data$spec.no)
  indx <- 0
  for (j in 1:length(gp))
    for (k in 1:length(spno)) {
      if (length(unique(data$group == gp[j] & data$spec.no ==
        spno[k])) == 2) {
        indx <- indx + 1
        cat(indx, "\n")
        m <- length(data$Intensity[data$group == gp[j] & data$spec.no ==
          spno[k]])
        x[indx, 1:m] <- as.vector(data$Intensity[data$group ==
          gp[j] & data$spec.no == spno[k]])

        x$group[indx] <- levels(gp[j])[j]
        x$spec.no[indx] <- levels(spno[k])[k]

      }
    }
}
WtNo <- XVector
return(list(sp = x, WtNo = WtNo))
}

