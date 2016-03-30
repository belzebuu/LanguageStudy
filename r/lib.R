

anonymize.wrap<-function(names) {
  require(anonymizer)
  require(stringr)
#  print(table(names))
  n<-length(unique(names))
  names <- str_sub(anonymize(names, .algo = "sha256", .seed = 2016, .chars = "osaf"),end=8) #,.n_chars = 5L, ...))
#  print(table(names))
  m<-length(unique(names))
  stopifnot(n==m)
  return(names)
}



randeff.plot<-function(lmm,rname,level=0.95) {
  randoms<-ranef(lmm, condVar = TRUE)
  qq <- attr(ranef(lmm, condVar = TRUE)[[rname]], "postVar")
  rand.interc<-randoms[[rname]]
  df<-data.frame(Intercepts=randoms[[rname]][,1],
                 sd.interc=sqrt(qq[,,1:length(qq)])*qnorm(1 - ((1 - level)/2)),
                 lev.names=rownames(rand.interc))
  
  df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$Intercepts)])
  .randeff.plot(df)  
}  

randeff.plot2<-function(lmm,level=0.95) {
    randoms<-ranef(lmm, condVar = TRUE)
  dff <-data.frame()
      for (rname in names(randoms)) {
      qq <- attr(ranef(lmm, condVar = TRUE)[[rname]], "postVar")
  rand.interc<-randoms[[rname]]
  df<-data.frame(Intercepts=randoms[[rname]][,1],
                 sd.interc=sqrt(qq[,,1:length(qq)])*qnorm(1 - ((1 - level)/2)), #2*
                 lev.names=rownames(rand.interc),
                 which=rname)
  df$lev.names<-factor(df$lev.names,levels=df$lev.names[order(df$Intercepts)])
  dff<-rbind(dff,df)
  
      }
#    return(dff)
    .randeff.plot.h(dff)
}  
  
.randeff.plot<-function(df) {  
  require(ggplot2)
  p <- ggplot(df,aes(y=lev.names,x=Intercepts,shape=lev.names))
  p <- p+facet_wrap(~which,ncol=nlevels(df$which),scales="free")
  #Added horizontal line at y=0, error bars to points and points with size two
  p <- p + geom_vline(xintercept=0) +geom_errorbarh(aes(xmin=Intercepts-sd.interc, xmax=Intercepts+sd.interc), height=0,color="black") 
  p<- p + geom_point(size=1.3) 
  
  #Removed legends and with scale_shape_manual point shapes set to 1 and 16
  p <- p + guides(size=FALSE,shape=FALSE) 
  p<-p+ scale_shape_manual(values=rep(16,dim(df)[1])) #c(1,1,1,16,16,16))
  
  #Changed appearance of plot (black and white theme) and x and y axis labels
  #  p <- p + theme_bw() + xlab("Levels") + ylab("")
  
  #Final adjustments of plot
  p <- p + theme(axis.text.x=element_text(size=rel(1.2)),
                 axis.title.x=element_text(size=rel(1.3)),
                 axis.text.y=element_text(size=rel(0.8)))
  #                panel.grid.minor=element_blank(),
  #                 panel.grid.major.x=element_blank())
  
  #To put levels on y axis you just need to use coord_flip()
 # p <- p+ coord_flip()
  print(p)
}





.randeff.plot.h<-function(df) {  
  require(ggplot2)
  df[,"ymin"]<-df[,"Intercepts"]-df[,"sd.interc"]
  df[,"ymax"]<-df[,"Intercepts"]+df[,"sd.interc"]
  df[, "sig"] <- df[, "ymin"] > 0 | df[, "ymax"] < 0
  p <- ggplot(df,aes(x=lev.names,y=Intercepts,shape=lev.names))
  p <- p+facet_wrap(~which,ncol=nlevels(df$which),scales="free_x")
  #Added horizontal line at y=0, error bars to points and points with size two
  p <- p + geom_hline(yintercept=0) 

  p <- p + geom_point(color = "gray55", alpha = 1/(nrow(df)^0.33), size = I(1.5)) 
  p <- p + geom_point(data = subset(df, sig == TRUE), size = I(1.3)) 

  p <- p+geom_errorbar(aes(ymin=Intercepts-sd.interc, ymax=Intercepts+sd.interc), width=0,alpha = 1/(nrow(df)^0.33)) 
  p <- p+geom_errorbar(aes(ymin=Intercepts-sd.interc, ymax=Intercepts+sd.interc), width=0,data = subset(df, sig == TRUE), 
                       alpha = 0.25) 

  
  #Removed legends and with scale_shape_manual point shapes set to 1 and 16
  p <- p + guides(size=FALSE,shape=FALSE) 
  p<-p+ scale_shape_manual(values=rep(16,dim(df)[1])) #c(1,1,1,16,16,16))
#  p<-p+ scale_shape_manual(values=sapply(df[,"sig"],function(x) ifelse(x,1,16))) #c(1,1,1,16,16,16))
  
  #Changed appearance of plot (black and white theme) and x and y axis labels
  #  p <- p + theme_bw() + xlab("Levels") + ylab("")
  
  #Final adjustments of plot
  p <- p + theme(axis.ticks.x = element_blank(), 
                 axis.text.x = element_blank(),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
            #      axis.text.x=element_text(size=rel(1.2)),
                 axis.title.x=element_text(size=rel(0.8)),
                 axis.text.y=element_text(size=rel(0.8)))
  #                panel.grid.minor=element_blank(),
  #                 panel.grid.major.x=element_blank())
  p<-p+  labs(x = "Groups", y = "Effect Range") 
#       title = "Effect Ranges")
  #To put levels on y axis you just need to use coord_flip()
  # p <- p+ coord_flip()
  print(p)
}