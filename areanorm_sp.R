##' Spectra Normalization with the area calculation of all peaks
##'
##' This function normalize the spectra with the way that all Intensities of
##' same spectra devided by areas of all peaks of that spectra and then calibrated by multiplying
##' the range of the Wavenumber
##' @author Qi Wang
##' @param data long format data frame with 4 or 5 variables: group, (sample), spec.no, WtNo and Intensity
##' @example sp_areanorm <- areanorm_sp(sp_dog)

areanorm_sp <- function(data) {
    data_orig <- data
    if (length(data$sample) == 0) {
        flag <- (data_orig$group == data_orig$group[1]) & (data_orig$spec.no ==
            data_orig$spec.no[1])
    }
    if (length(data$sample) != 0) {
        flag <- (data_orig$group == data_orig$group[1]) & (data_orig$spec.no ==
            data_orig$spec.no[1]) & (data_orig$sample == data_orig$sample[1])
    }

    wt_no <- data_orig$WtNo[flag]
    Intensity <- data_orig$Intensity
    WtNo <- data_orig$WtNo
    n <- length(wt_no)
    data <- t(cbind(data$WtNo, data$Intensity))
    spec_no <- dim(data)[2]/length(wt_no)
    np <- dim(data)
    area <- matrix(0, 1, spec_no)
    areanorm <- matrix(0, 1, np[2])

    for (j in 1:spec_no) {
        for (i in 2:n) {
            area[j] <- area[j] + (abs(Intensity[n * (j - 1) +
                i]) + abs(Intensity[n * (j - 1) + i - 1])) *
                (WtNo[(n * (j - 1) + i)] - WtNo[(n * (j - 1) +
                  i - 1)])/2
        }
        for (i in 1:n) {
            areanorm[n * (j - 1) + i] <- Intensity[n * (j - 1) +
                i] * (max(WtNo) - min(WtNo))/area[j]
        }
    }
    data_orig$Intensity <- as.vector(areanorm)
    return(data_orig)
}

