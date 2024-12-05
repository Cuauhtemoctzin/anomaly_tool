
#libraries  
library(ggplot2)
library(AnomalyScore)
library(ggpubr)
library(gridExtra)
library(gitcreds)

dist_names<-c(
  'Cort',
  'WS',
  'MH',
  'CortN',
  'Coh',
  'PDC',
  'CGCI',
  'RGPDC',
  'PMIME',
  'mvLWS',
  'BD'
)
functions=list(
  distance_matrix_cort,
  distance_matrix_wasserstein,
  distance_matrix_mahalanobis,
  distance_matrix_cortNorm,
  distance_matrix_coherence,
  distance_matrix_PDC,
  distance_matrix_CGCI,
  distance_matrix_RGPDC,
  distance_matrix_PMIME,
  distance_matrix_mvLWS,
  distance_matrix_banddepth
)

## auxiliary function to find the index of one set of strings based on a second set of strings
## the name is based on finding subsets of meters codes across different tables
findmeters=function(namestosearch, meters ){
  meterloc=c()
  for(i in 1: length(meters)){
    meterloc[i]<- grep(paste0("^",  meters[i],"$"  )  ,namestosearch)
  }
  return(meterloc)
}

colorreg=c("green4", "yellow","orange","red")
coloralt=c("#648FFF","#785EF0","#DC267F","#FE6100","#FFB000")

#first create the graph i want with a ggplot 
# load simple data example, 

# frameallenergy=readRDS( "/Users/guillermocgg/Dropbox/Lancaster/GRP_net0i/basesraw/baseallLUdatarate.rds"  )
# #filter the data for one day and generate an example data set and save it as csv
# startday="2022-01-05"
# endday="2022-01-06"
# 
# 
# sum_series=frameallenergy[which(frameallenergy$time>= as.character(startday) &frameallenergy$time<=as.character(endday)),]
# if( dim(sum_series)[1]!=0 ){
#   sumvert= apply(sum_series  ,2, function(x) sum(is.na(x)))
#   devsout= which(sumvert>0 )
#   if(length(devsout)>0){sum_series=sum_series[,-devsout]}
#   #exclude devices where all observations are Zero
#   mtk= which(names(sum_series)=="time"|names(sum_series)=="timechar" )
#   chekcdev= apply(sum_series[,-mtk],2,sum, na.rm=T )
#   zerodevices= as.vector(which(chekcdev<.0001))
#   if(length(zerodevices)>0){ sum_series=sum_series[,-zerodevices]}
#   #get the series into matrix format without time variable for the score function
#   mtk= which(names(sum_series)=="time"|names(sum_series)=="timechar" )
#   unittest=as.matrix( sum_series[,-mtk ] )
#   #unittest=scale(unittest, center = T, scale = T)
# }
# 
# matplot(unittest, type="l")
# 
# unit=as.matrix(unittest)
# write.csv(unittest,row.names = F,"/Users/guillermocgg/Dropbox/Lancaster/MyWork/shinnyapp/anomaly_tool/unittest.csv" )
# write.csv(unittest[,10:18],row.names = F,"/Users/guillermocgg/Dropbox/Lancaster/MyWork/shinnyapp/anomaly_tool/unittestV2.csv" )
# dparams=list(
#   list(unit=unit, k=3),
#   list(unit=unit ),
#   list(unit=unit ),
#   list(unit=unit, k=3),
#   list(unit=unit, span1=3, span2=3, period = 5 ),
#   list(unit=unit, kvar=ncol(unit), ar=c( 5,5*2 ), period = 5 ),
#   list(unit=unit , pmax=10 ),
#   list(unit=unit , pmax=10, period=5),
#   list(unit=unit, Lmax=10, Tl=5, nnei=10, A=.95 ),
#   list(unit=unit  ),
#   list(unit=unit )
# )

#compute a table compte anomaly scores for a measure 
# unit=read.csv("/Users/guillermocgg/Dropbox/Lancaster/MyWork/shinnyapp/anomaly_tool/unittestV2.csv" )
# knn=5
# measures=c(1,2,3,5,11)

frameallscores=function(unit, knn,measures, dparams){
  unit=as.matrix(unit)
  # dparams=list(
  #   list(unit=unit, k=3),
  #   list(unit=unit ),
  #   list(unit=unit ),
  #   list(unit=unit, k=3),
  #   list(unit=unit, span1=3, span2=3, period = 5 ),
  #   list(unit=unit, kvar=ncol(unit), ar=c( 5,5*2 ), period = 5 ),
  #   list(unit=unit , pmax=30 ),
  #   list(unit=unit , pmax=30, period=5),
  #   list(unit=unit, Lmax=30, Tl=5, nnei=10, A=.95 ),
  #   list(unit=unit  ),
  #   list(unit=unit )
  # )
frame_scores= data.frame(series=colnames(unit))
for(i in 1:length( measures) ){
distance=functions[[measures[ i] ]]
mydparams=dparams[[ measures[ i] ]]
AScat1=kneighbors_distance_docall(knn, distance, mydparams )
frame_scores$score= AScat1$anomalyscore
names(frame_scores)[(i+1)]=dist_names[measures[ i]]
}
#rank is ordered with respect to the first measure
frame_scores=frame_scores[order(frame_scores[,2], decreasing = T),]
frame_scores$Rank= length( frame_scores$series):1
return(frame_scores)
}

#generate horizontal bar depicting the score, with the proper order
# library("RColorBrewer")
# p1 <- ggplot(frame_scores, aes_string(x = "Rank", y =dist_names[1]  ) )+ggtitle(dist_names[1])+
#   geom_bar(aes_string( fill=dist_names[1]), colour="black", stat="identity", width =1) + 
#   scale_fill_gradientn(colours =c("darkgreen", "yellow","orange","red" ) )+
#   coord_flip()+
#   theme_bw() + 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(), axis.line = element_blank(),legend.position="none",
#   axis.title=element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
#   plot.title = element_text(hjust = 0.5)
#   )
#                 
# print(p1)
# p2 <- ggplot(frame_scores, aes_string(x = "Rank", y =dist_names[11]  ) )+ggtitle(dist_names[11])+
#   geom_bar(aes_string( fill=dist_names[11]), colour="black", stat="identity", width = 1) + 
#   scale_fill_gradientn(colours =c("darkgreen", "yellow","orange","red" ) )+
#   coord_flip()+
#   theme_bw() + 
#   theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), axis.line = element_blank(),legend.position="none",
#         axis.title=element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
#         plot.title = element_text(hjust = 0.5)
#   )
#frame_scores=frameallscores(unit, knn, measures)


plot_score=function(frame_scores,measures, colorblind){
if(colorblind){
  colorsplot=coloralt
}else{
  colorsplot=colorreg
}    
  
eff_measures=as.list(dist_names[ measures ])
plotList <- lapply(
  eff_measures,
  function(key) {
    # Need to assign the plot to a variable because 
    if(key!='empty'){mytitle=key}else{mytitle=''}
    x <- p2 <- ggplot(frame_scores, aes_string(x = "Rank", y =key  ) )+ggtitle(mytitle)+
      geom_bar(aes_string( fill=key), colour="black", stat="identity", width = 1) + 
      scale_fill_gradientn(colours =colorsplot )+
      coord_flip()+
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_blank(),legend.position="none",
            axis.title=element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5)
      )
    x
  }
)
#ggscores=  
ggarrange(plotlist =plotList , ncol = length(eff_measures) )

# #generate a table with 
# mytable=data.frame(Series=frame_scores$series,Rank=frame_scores$Rank,color=layer_data(plotList[[1]])[,1])
# tbl <- tableGrob(mytable[,c(1,2)],theme=ttheme_default(core=list(bg_params = list(fill =layer_data(plotList[[1]])[,1] , col=NA), fg_params=list(cex=1)  )  )  )#, 
#                                                        
#                                                           # core = list(fg_params=list(cex = 2.0)),
#                                                           # colhead = list(fg_params=list(cex = 1.0)),
#                                                           # rowhead = list(fg_params=list(cex = 1.0))            
#                                                           # 
#                                                         #)  )  )
# 
# # stable.p <- ggtexttable(mytable, rows = NULL, 
# #                         theme = ttheme("mOrange"))
# #                                     #   ,base_size = 6.7,colnames.style = colnames_style(size = 6.7),padding = unit(c(1, 2.7), "mm")  ) )
# # display the table and plots together
# # grid.arrange(ggplotGrob(stable.p), ggscores, ncol=2, widths=c(1,length(eff_measures)))
# 
# grid.arrange(tbl, ggscores, ncol=2, widths=c(2,length(eff_measures)))
 }



### plotting the series in a nice format   #####

#steps
plotseries=function(unit,frame_scores, measures,colorblind){
# unit contains the series  
# table contains the ranks and colors 
  unit=as.matrix(unit)
  if(colorblind){
    colorsplot=coloralt
  }else{
    colorsplot=colorreg
  }    
  
  eff_measures=as.list(dist_names[ measures ])
  plotList <- lapply(
    eff_measures,
    function(key) {
      # Need to assign the plot to a variable because 
      if(key!='empty'){mytitle=key}else{mytitle=''}
      x <- p2 <- ggplot(frame_scores, aes_string(x = "Rank", y =key  ) )+ggtitle(mytitle)+
        geom_bar(aes_string( fill=key), colour="black", stat="identity", width = 1) + 
        scale_fill_gradientn(colours =colorsplot )+
        coord_flip()+
        theme_bw() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_blank(),legend.position="none",
              axis.title=element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5)
        )
      x
    }
  )  
  
mytable=data.frame(Series=frame_scores$series,Rank=frame_scores$Rank,color=layer_data(plotList[[1]])[,1])
  
# make vertical with columns series , index, value, color 
thenames=colnames(  unit)
  
for(i in 1:length(thenames) ){

tempframe=data.frame(Series= thenames[i]  ,index=1:length(unit[,i]), values=unit[,i] )  
if(i==1){finframe=tempframe}else{
  finframe=rbind(finframe,tempframe)
} 

} 

# ordering the labels and colors according to how ggplot order labeled time series
# that is alphabetical order
thenames=thenames[ order(thenames) ]
mytable=mytable[findmeters(mytable$Series,thenames ), ]

p=ggplot(finframe, aes(x=index, y=values, colour=Series )  )+
  geom_line()+ scale_color_manual(values = mytable$color, labels=mytable$Series )+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                      panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")+ylab("")+xlab("")
ggarrange(p, ncol=1)
}


#plotseries(unit,frame_scores, measures,colorblind=F)



plot_onlytable=function(frame_scores,measures, colorblind){
  if(colorblind){
    colorsplot=coloralt
  }else{
    colorsplot=colorreg
  }    
  
  eff_measures=as.list(dist_names[ measures ])
  plotList <- lapply(
    eff_measures,
    function(key) {
      # Need to assign the plot to a variable because 
      if(key!='empty'){mytitle=key}else{mytitle=''}
      x <- p2 <- ggplot(frame_scores, aes_string(x = "Rank", y =key  ) )+ggtitle(mytitle)+
        geom_bar(aes_string( fill=key), colour="black", stat="identity", width = 1) + 
        scale_fill_gradientn(colours =colorsplot )+
        coord_flip()+
        theme_bw() + 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_blank(),legend.position="none",
              axis.title=element_blank(), axis.text = element_blank(),axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5)
        )
      x
    }
  )
  
  #generate a table with 
mytable=data.frame(Series=frame_scores$series,Rank=frame_scores$Rank,color=layer_data(plotList[[1]])[,1])
p=tableGrob(mytable[,c(1,2)],theme=ttheme_default(core=list(bg_params = list(fill =layer_data(plotList[[1]])[,1] , col=NA), fg_params=list(cex=1.2,col="white")  ),
                                                  colhead=list(fg_params=list(cex=1)) )  )
ggarrange(p, ncol=1)  
}
