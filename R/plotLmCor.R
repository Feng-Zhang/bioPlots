##' @title plotLmCor
##' @description This function would generate a list including a countData and colData as input data for testing
##' @param data Default dataset to use for plot.
##' @param mapping Default list of aesthetic mappings to use for plot
##' @return a ggplot2 object
##' @examples
##' require(ggplot2)
##' print(plotLmCor(mpg,aes(cty,hwy)))
##' @import ggplot2
##' @importFrom ggpubr stat_cor
##' @importFrom stats lm
##' @export

plotLmCor <- function(data,mapping) {
  p<-ggplot(data,mapping)+geom_point()+
    stat_smooth(method=lm) +
    stat_cor(method = "pearson")
  return(p)
}

##' @title plotLmCor2
##' @description This function would generate a list including a countData and colData as input data for testing
##' @param data Default dataset to use for plot.
##' @param x Default variable from data for aesthetic mappings to use for plot
##' @param y Default variable from data for aesthetic mappings to use for plot
##' @return a ggplot2 object
##' @examples
##' require(ggplot2)
##' print(plotLmCor2(mpg,cty,hwy))
##' @import ggplot2
##' @importFrom ggpubr stat_cor
##' @importFrom stats lm
##' @export
plotLmCor2 <- function(data,x,y){
  p <- ggplot(data,aes_(substitute(x),substitute(y)))+
    geom_point()+
    stat_smooth(method=lm) +
    stat_cor(method = "pearson")
  return(p)
}
