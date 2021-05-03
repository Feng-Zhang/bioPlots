##' @title plotBarSdSig
##' @description This function would generate a list including a countData and colData as input data for testing
##' @param data Default dataset to use for plot.
##' @param x Default variable from data for aesthetic mappings to use for plot
##' @param y Default variable from data for aesthetic mappings to use for plot
##' @param rowOrder the order of x-axis
##' @param comparisons A list of length-2 vectors. The entries in the vector are either the names of 2 values on the x-axis or the 2 integers that correspond to the index of the columns of interest.
##' @param y_position numeric vector with the y positions of the brackets
##' @return a ggplot2 object
##' @examples
##' \dontrun{
##' library(ggplot2);library(dplyr);library(ggsignif)
##'
##' compareGroup = list(c("f","4"),c("f","r"),c("4","r"))
##' plotBarSdSig(mpg,drv,hwy,comparisons=compareGroup,y_position=TRUE)
##'
##' plotBarSdSig(mpg,drv,hwy,rowOrder=c("r","4","f"),
##' comparisons=compareGroup,y_position=c(48.2,50.8,32.07))
##'}
##' @import ggplot2
##' @import dplyr
##' @importFrom ggsignif geom_signif
##' @importFrom stats sd
##' @export

plotBarSdSig <- function(data,x,y,rowOrder=c(),comparisons=NULL,y_position= NULL) {
  topPos <- NULL
  #data = data %>% arrange(match({{x}}, rowOrder))
  summ = data %>% group_by({{x}}) %>%
    summarise(mean=mean({{y}}),sd=sd({{y}}),n=length({{y}}),se=sd/sqrt(n),max=max({{y}}),topPos=max+sd) #对各组计算mean和sd等

  if(is.null(comparisons) | is.null(y_position)){
    y_position = NULL
  } else if(is.logical(y_position)){
    if(y_position){
      y_position = sapply(comparisons,function(groupName){summ%>%filter({{x}} %in% groupName) %>% select(topPos) %>%max()})
      print(paste0("The position of y axis for significant plotting: ",y_position))
    }
  }
  # else if (!is.numeric(y_position)){
  #   stop("y_postion should be TURE or FALSE or a numeric vector with same length as comparisons argument")
  # }

  if(is.null(rowOrder)) p = ggplot(data, aes({{x}}, {{y}})) else p=ggplot(data, aes(factor({{x}},levels = rowOrder), {{y}}))

  p = p+ geom_bar(stat="identity", position="dodge", width=.5)+
    geom_errorbar(data = summ, aes(x = {{x}}, y=max,ymin = max, ymax = max + sd, width=.2),  position = position_dodge(width=.8))+ #画标准差
    geom_signif(comparisons=comparisons,
                y_position=y_position,
                tip_length = 0.02, vjust=0.2, map_signif_level=TRUE) #画显著星号
  p = p+theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border = element_blank(),axis.line = element_line(colour = "black")) #去除背景色、格子和边框

  return(p)
}


