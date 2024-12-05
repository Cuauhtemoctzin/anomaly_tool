
#libraries  
library(ggplot2)
library(AnomalyScore)
library(ggpubr)
library(gridExtra)
library(gitcreds)

# definition of abreviates names for the distances
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
# list of all the function available to compute distances matrix in the AnomalyScore package
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

# definition of the color palettes
colorreg=c("green4", "yellow","orange","red")
coloralt=c("#648FFF","#785EF0","#DC267F","#FE6100","#FFB000")


# this function computes the anomaly scores based on the selected distances
frameallscores=function(unit, knn,measures, dparams){
  unit=as.matrix(unit)

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


# this function plots in a horizontal bars all the scores
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
  
  ggarrange(plotlist =plotList , ncol = length(eff_measures) )
}



### plotting the series in a nice format   #####

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




# this function plots only the table and ranks and set color according to the score
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
