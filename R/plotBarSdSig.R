##' @title plotBarSdSig
##' @description This function would generate a list including a countData and colData as input data for testing
##' @param mydata a dataframe with two column of group and phe
##' @return a ggplot2 object
##' @examples
##' library(ggplot2);library(plyr);library(ggpubr)
##' mydata = mpg[,c("drv","hwy")];colnames(mydata)=c("group","phe")
##' plotBarSdSig(mydata)
##' @import ggplot2
##' @importFrom plyr ddply
##' @importFrom ggsignif geom_signif
##' @importFrom stats sd
##' @export

plotBarSdSig <- function(mydata) {
  #mydata=mpg
  groupName = unique(mydata$group)
  summ = ddply(mydata,"group",summarise,mean=mean(phe),sd=sd(phe),n=length(phe),se=sd/sqrt(n),max=max(phe)) #对各组计算mean和sd等
  row.names(summ)=summ$group
  p = ggplot(mydata, aes(group, phe)) + geom_bar(stat="identity", position="dodge", width=.5)+ #画柱状图
    geom_errorbar(data = summ, aes(x = group, y=max,ymin = max, ymax = max + sd, width=.2),  position = position_dodge(width=.8))+ #画标准差
    geom_signif(comparisons=list(c(groupName[1],groupName[2]),
                                 c(groupName[1],groupName[3]),
                                 c(groupName[2],groupName[3])),
                y_position=c(summ[groupName,"max"] ),tip_length = 0.02, vjust=0.2, map_signif_level=TRUE) #画显著星号


  p = p+theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border = element_blank(),axis.line = element_line(colour = "black")) #去除背景色、格子和边框

  return(p)
}
