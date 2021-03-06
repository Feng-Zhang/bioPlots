##' @title plotLmCor
##' @description This function would generate a list including a countData and colData as input data for testing
##' @param mydata a dataframe with two column of x and y
##' @return a ggplot2 object
##' @examples
##' require(ggplot2)
##' mydata = mpg[,8:9];colnames(mydata)=c("x","y")
##' plotLmCor(mydata)
##' @import ggplot2
##' @importFrom ggpubr stat_cor
##' @importFrom stats lm
##' @export

plotLmCor <- function(mydata) {
  p<-ggplot(mydata,aes_(~x,~y))+geom_point()+
    stat_smooth(method=lm) +
    stat_cor(method = "pearson")
  return(p)
}
