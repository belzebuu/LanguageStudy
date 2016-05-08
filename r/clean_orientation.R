library(dplyr)

D0<-read.csv("../data/factors_orientation.csv",na.strings="Err:512",sep=";",header=TRUE)
#D0<-na.omit(D0)
#D<-rbind(data.frame(D0[1:23],time=D0[,24],type="MATCH"),data.frame(D0[1:23],time=D0[,25],type="MISMATCH"))
# D1<-D0 %>% select(Name,CoR,Hand,EO,List,CEF,SRRC,PRE,POST1,POST2,STAY,LEAYRS,HRSD,RPV,AMGE,AMSP)#,time,type) 
D1<-D0 %>% select(Name,List,PRE,POST1,POST2,LEAYRS,HRSD,AMSP)
# %>% transmute(LanguageAge=Age-AoA)
# D2<-D1[,c("Hand")] %>% mutate_each(funs(factor))
D1$Name<-factor(D1$Name)
#D1$Hand<-factor(D1$Hand)
#D1$EO<-factor(D1$EO)
D1$List<-factor(D1$List)
D1<-D1 %>% mutate(PRE=PRE/10) %>% mutate(POST1=POST1/10) %>% mutate(POST2=POST2/10) %>% mutate(LEA=POST1-PRE)

#D1$CEF<-factor(D1$CEF,levels=c(3:5),ordered=TRUE)
#D1$SRRC<-factor(D1$SRRC,levels=c(1:5),ordered=TRUE)
#D1$AMGE<-factor(D1$AMGE,levels=c(1:5),ordered=TRUE)
#D1$AMSP<-factor(D1$AMSP,levels=c(1:5),ordered=TRUE)
#D1$RPV<-factor(D1$RPV,levels=c(1:5),ordered=TRUE)
#i <- which(rownames(D1)=="1071")
#D1<-D1[-i,]
Lists<-read.csv("../data/L2GermanLists.csv",na.strings="Err:512",sep=";",header=FALSE)
levels(Lists$V2)<-c(0,1)
names(Lists)<-c("Name","List")
D1<-full_join(D1,Lists,by="Name") %>% mutate(List=ifelse(is.na(List.y),as.character(List.x),as.character(List.y)))
D1$List<-factor(D1$List)



L0 <- read.csv("../data/L2GermanMatch.csv",na.strings="Err:512",sep=";",header=FALSE)
L1 <- reshape(L0,direction="long",varying=list(2:9),idvar="id",ids=1:NROW(L0),times=1:8,timevar = "order")
names(L1)<-c("Name","Order","Time","ID")
DM<-right_join(D1,L1,by="Name")

L0 <- read.csv("../data/L2GermanMisMatch.csv",na.strings="Err:512",sep=";",header=FALSE)
L1 <- reshape(L0,direction="long",varying=list(2:9),times=1:8,timevar = "order")
names(L1)<-c("Name","Order","Time","ID")
DMM<-right_join(D1,L1,by="Name")

D0<-rbind(data.frame(DM,type="MATCH"),data.frame(DMM,type="MISMATCH"))
#D0<-na.omit(D0) # lmer does this already
D0$Name<-factor(D0$Name)
D0$Order<-factor(D0$Order)
# head(D)
# count(D,Name,type)
D.good <- D0 %>% filter(POST1>7.5) %>% filter(POST2>7.5)
D.good$Name<-factor(D.good$Name)
Orientation <- D0


#DD <- D %>% group_by(Name,type) %>% filter(length(Time)>4) %>% sample_n(5,replace=FALSE)
## we might want to study only those with a decent result in the test (others might be outliers)
#count(D2,Name,type)$n


##################################################################################################
Lists<-read.csv("../data/L1GermanLists.csv",na.strings="Err:512",sep=";",header=FALSE)
levels(Lists$V2)<-c(0,1)
names(Lists)<-c("Name","List")

L0 <- read.csv("../data/L1GermanMatch.csv",na.strings="Err:512",sep=";",header=FALSE)
L1 <- reshape(L0,direction="long",varying=list(2:9),idvar="id",ids=1:NROW(L0),times=1:8,timevar = "order")
names(L1)<-c("Name","Order","Time","ID")
DM<-right_join(Lists,L1,by="Name")

L0 <- read.csv("../data/L1GermanMisMatch.csv",na.strings="Err:512",sep=";",header=FALSE)
L1 <- reshape(L0,direction="long",varying=list(2:9),times=1:8,timevar = "order")
names(L1)<-c("Name","Order","Time","ID")
DMM<-right_join(Lists,L1,by="Name")

D10<-rbind(data.frame(DM,type="MATCH"),data.frame(DMM,type="MISMATCH"))
#D10<-na.omit(D10) # lmer does this already
D10$Name<-factor(D10$Name)


# Bind together L1 and L2
DDD <- rbind(data.frame(Orientation[c("Name", "List", "Order", "Time", "ID", "type")],Lang="L2"),data.frame(D10,Lang="L1"))
DDD$Name<-factor(DDD$Name)
OrientationL1L2<-DDD

source("lib.R")
levels(Orientation$Name)<-anonymize.wrap(levels(Orientation$Name))
levels(OrientationL1L2$Name)<-anonymize.wrap(levels(OrientationL1L2$Name))


save(Orientation,OrientationL1L2,file="orientation.RData")




#DD <- D %>% group_by(Name,type) %>% filter(length(Time)>4) %>% sample_n(5,replace=FALSE)
## we might want to study only those with a decent result in the test (others might be outliers)
#count(D2,Name,type)$n

