##' @title plotBarSdSig
##' @description This function would generate a list including a countData and colData as input data for testing
##' @return a ggplot2 object
##' @examples
##' mydata = mpg[,c("drv","hwy")];colnames(mydata)=c("group","phe")
##' plotBarSdSig(mydata)
##' @import ggplot2
##' @importFrom plyr ddply
##' @export

plotBarSdSig <- function(mydata) {
  #mydata=mpg
  groupName = unique(mydata$group)
  summ = ddply(mydata,"group",summarise,mean=mean(phe),sd=sd(phe),n=length(phe),se=sd/sqrt(n),max=max(phe)) #对各组计算mean和sd等

  p = ggplot(mydata, aes(group, phe)) + geom_bar(stat="identity", position="dodge", width=.5)+ #画柱状图
    geom_signif(comparisons=list(c("promoter","C"),
                                 c("promoter","T"),
                                 c("C","T")),
                y_position=c(11.2,5.8,10.7 ),tip_length = 0.02, vjust=0.2, map_signif_level=TRUE)+ #画显著星号
    geom_errorbar(data = summ, aes(x = group, y=max,ymin = max, ymax = max + sd, width=.2),  position = position_dodge(width=.8)) #画标准差


  p = p+theme_bw() +theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border = element_blank(),axis.line = element_line(colour = "black")) #去除背景色、格子和边框

  return(p)
}
