
```{r plots, echo=FALSE}
q<-ggplot(data=DLM, aes(y=Time, x=Order, color=Lang))
q<-q+facet_wrap(~type+List,ncol=2,drop=TRUE)
q<-q+geom_jitter(width = 0.4, height = 0.0)
#q<-q+geom_smooth(method=lm,se=FALSE)
q<-q+theme(legend.position="top") 
q<-q+scale_y_log10()
print(q)
```


```{r plots, echo=FALSE}
DLM$Name <- factor(str_trim(DLM$Name))
ss <- sample(levels(DLM$Name), 15)
q<-ggplot(data=subset(DLM,Name %in% ss), aes(y=Time, x=Order, color=type))
q<-q+facet_wrap(~Name,ncol=5,drop=TRUE)
q<-q+geom_point()
#q<-q+geom_smooth(method=lm,se=FALSE)
q<-q+theme(legend.position="top") 
q<-q+scale_y_log10()
print(q)
```

```{r mixedmodels1, warning=FALSE}
lm.0 <- lm(log(Time) ~ 1, data = DLM)
# LEAYRS AMSP AMGE
lmm.0 <- lmer(log(Time) ~ (1 | List:Name)+ (1 | List:type:Order), data = DLM)
AIC(lm.0,lmm.0)
```

<!--
  ## Regression tree
  
  Another type of analysis by means of regression tree:
  
  ```{r regtree, eval=FALSE, echo=FALSE}
## ct <- ctree(data=D2,Time~ type + CoR + Hand + EO + List + CEF + SRRC + PRE + POST1 + POST2 + STAY + LEAYRS + HRSD + RPV + AMGE + AMSP )
ct <- ctree(data=DLM,log(Time)~ type + HRSD + RPV + AMGE + AMSP)
ct
plot(ct)
```



-->
  
  
  
  <!--
  
  # Bootstrapped confidence intervals
  
  
  
  ```{r confint}
#op <- options(contrasts = c("contr.sum", "contr.poly"))
## fm01ML <- lmer(Yield ~ 1|Batch, Dyestuff, REML = FALSE)
## see ?"profile-methods"
mySumm <- function(.) { s <- sigma(.)
c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
(t0 <- mySumm(lmm)) # just three parameters
## alternatively:
mySumm2 <- function(.) {
  c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}

set.seed(101)
## 3.8s (on a 5600 MIPS 64bit fast(year 2009) desktop "AMD Phenom(tm) II X4 925"):
system.time( boo01 <- bootMer(lmm, mySumm, nsim = 100) )

## to "look" at it
require("boot") ## a recommended package, i.e. *must* be there
boo01
## note large estimated bias for sig01
## (~30% low, decreases _slightly_ for nsim = 1000)

## extract the bootstrapped values as a data frame ...
head(as.data.frame(boo01))

## ------ Bootstrap-based confidence intervals ------------

## warnings about "Some ... intervals may be unstable" go away
##   for larger bootstrap samples, e.g. nsim=500

## intercept
(bCI.1 <- boot.ci(boo01, index=1, type=c("norm", "basic", "perc")))# beta

## Residual standard deviation - original scale:
(bCI.2  <- boot.ci(boo01, index=2, type=c("norm", "basic", "perc")))
## Residual SD - transform to log scale:
#(bCI.2L <- boot.ci(boo01, index=2, type=c("norm", "basic", "perc"),
#                   h = log, hdot = function(.) 1/., hinv = exp))

## Among-batch variance:
(bCI.3 <- boot.ci(boo01, index=3, type=c("norm", "basic", "perc"))) # sig01

## Extract all CIs (somewhat awkward)
bCI.tab <- function(b,ind=length(b$t0), type="perc", conf=0.95) {
  btab0 <- t(sapply(as.list(seq(ind)),
                    function(i)
                      boot.ci(b,index=i,conf=conf, type=type)$percent))
  btab <- btab0[,4:5]
  rownames(btab) <- names(b$t0)
  a <- (1 - conf)/2
  a <- c(a, 1 - a)
  pct <- stats:::format.perc(a, 3)
  colnames(btab) <- pct
  return(btab)
}
bCI.tab(boo01)

## Graphical examination:
plot(boo01,index=3)
-->