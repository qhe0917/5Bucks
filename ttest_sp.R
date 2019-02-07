ttest_sp <- function(data)
  {
  	data_orig <- data
  if (length(data$sample) == 0) {
    flag <- (data_orig$group == data_orig$group[1]) & (data_orig$spec.no ==
                                                         data_orig$spec.no[1])
  }
  if (length(data$sample) != 0) {
    flag <- (data_orig$group == data_orig$group[1]) & (data_orig$spec.no ==                                                       data_orig$spec.no[1]) & (data_orig$sample == data_orig$sample[1])
  }
  wt_no <- data_orig$WtNo[flag]
  data_orig$group <- as.factor(data_orig$group)
  nwtno <- length(wt_no)
  ttest <- pairwise.t.test(data_orig$Intensity, data_orig$group)
  pvalue <- melt(ttest$p.value)
  pvalue<- na.omit(pvalue)
  pvalue$wtno <- rep(NA,nrow(pvalue))
  p <- pvalue
  p[] <- NA
  for (i in 1:nwtno)
  {
    subdata <- subset(data_orig,WtNo == wt_no[i])
    ttest <- pairwise.t.test(subdata$Intensity, subdata$group)
    pvalue <- melt(ttest$p.value)
    pvalue$wtno <- rep(wt_no[i],nrow(pvalue))
    pvalue<- na.omit(pvalue)
    p <- rbind(p,pvalue)
    p <- na.omit(p)
  }
  return(p)

}
