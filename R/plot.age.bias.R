plot.age.bias <-  function(xin,alpha=0.05,ref.count=TRUE,plot.type="age.bias",add.1to1=TRUE,add.actual=TRUE,
                           add.CV=TRUE,error.CV=2,posit.CV=c(0.85*(max(tmp.x)),0.15*(max(tmp.x))),...)
{
  # A function to draw an age bias graph for two sets of counts
  # and calculate CV after Campana et al. (1995)
  # -----------------------------------------------------------
  # MJM 11 Feb 2003

  # Read in and preprocess data
  xin.df <- as.data.frame(xin)
 
  if(dim(xin.df)[2]!=2)
    {
      stop("Your data is not a dataframe with ncol=2...")
    }
  else
    {
      cat("\nIf your data contains cases with missing values (e.g. NA counts with readability \"5\"), these will be omitted...\n")

      xin.df <- as.data.frame(na.omit(xin.df))

      if(ref.count==TRUE)
        {
          count.x <- xin.df[,1]
          count.y <- xin.df[,2]
        }
      else
        {
          count.x <- xin.df[,2]
          count.y <- xin.df[,1]
        }
      
      xin.list <- split(count.y,as.factor(count.x))

    }

  # Calc CV and IAPE...
  cat("\nCalculating CV and IAPE...\n")

  tmp.cv.out <- calc.age.cv(xin.df,print.level=0,return.cv=TRUE)
  mean.CV <- tmp.cv.out$mean.CV
  tmp.iape.out <- calc.iape(xin.df,print.level=0,return.iape=TRUE)
  iape <- tmp.iape.out$iape
  
#  tmp.N <- nrow(xin.df)
#  tmp.R <- ncol(xin.df)
#  xj <- (1/tmp.R)*(apply(xin.df,1,sum))
#  CV <- 100* sqrt(apply((((xin.df - xj)^2)/(tmp.R-1)),1,sum))/xj
#  mean.CV <- sum(CV)/tmp.N

  cat("\n\tCV = ",round(mean.CV,digits=error.CV),"%\n",sep="")
  cat("\tIAPE = ",round(iape,digits=error.CV),"%\n",sep="")
    
  

  # Draw age bias plot
  cat("\nDrawing plot...\n\n")

  if(plot.type=="age.bias")
    {
      # Calc CI
      tmp.mean <- unlist(lapply(xin.list,mean))
      tmp.s <- sqrt(unlist(lapply(xin.list,var)))
      tmp.n <- unlist(lapply(xin.list,length))
      tmp.t <- qt(p=(1 - (alpha/2)),df=tmp.n)
      tmp.width <- tmp.t*(tmp.s/sqrt(tmp.n))
      tmp.x <- as.numeric(names(tmp.mean))
      tmp.CV <- (tmp.s/tmp.mean)*100

      # Draw age.bias  
      plot(tmp.x,tmp.mean,type="o", ...)
      
      if(add.1to1==TRUE)
        {abline(coef=c(0,1),lty=1)}
      
      if(add.actual==TRUE)
        {tmp.lm <- lm(xin.df[,2]~xin.df[,1]);abline(tmp.lm,lty=3);print(tmp.lm)}

      segments(tmp.x,tmp.mean,tmp.x,tmp.mean+tmp.width)
      segments(tmp.x,tmp.mean+tmp.width,tmp.x-0.425,tmp.mean+tmp.width)
      segments(tmp.x,tmp.mean+tmp.width,tmp.x+0.425,tmp.mean+tmp.width)

      segments(tmp.x,tmp.mean,tmp.x,tmp.mean-tmp.width)
      segments(tmp.x,tmp.mean-tmp.width,tmp.x-0.425,tmp.mean-tmp.width)
      segments(tmp.x,tmp.mean-tmp.width,tmp.x+0.425,tmp.mean-tmp.width)

       if(add.CV==TRUE)
        {
          tmp.label <- paste("CV = ",round(mean.CV,digits=error.CV),"%",sep="")
          text(posit.CV[1],posit.CV[2],labels=tmp.label,pos=4)
        }
    }
  else
    if(plot.type=="age.CV")
      {
        # "age.CV" will plot a Campana-esque CV profile with IAPE profile overlaid
        # both relative to the FIRST SET of readings in xin, i.e. the  set used to
        # predict the values of the other reading(S) set(s).

        # NB: Campana uses a "case-wise" CV rather than a "conventional" CV,
        # i.e. the CVs for all cases (each reading) are split by the original reading

        
        tmp.list.cv <- split(tmp.cv.out$out.df$CV,tmp.cv.out$out.df[,1])
        tmp.list.iape <- split(tmp.iape.out$out.df$ape,tmp.iape.out$out.df[,1])

        tmp.list.cv.mean <- unlist(lapply(tmp.list.cv,mean))
        tmp.list.iape.mean <- unlist(lapply(tmp.list.iape,mean))
        tmp.list.x <- as.numeric(unlist(names(tmp.list.cv.mean)))

        plot(tmp.list.x,tmp.list.cv.mean,type="o",pch=1,lty=1, ...)
        points(tmp.list.x,tmp.list.iape.mean,type="o",pch=1,lty=3)

#        max.y <- max(max(tmp.list.cv.mean),max(tmp.list.iape.mean))
#        if(max.y<100)
#          {
#            max.y <- 100
#          }
#        else
#          {
#            max.y <- max.y
#          }
        
#        legend(0,max.y,legend=c("CV","IAPE"),lty=c(1,3))#<-- must sort position out...presently easier to add it after the plot is produced...
        
        out.df <- data.frame(tmp.list.x,tmp.list.cv.mean,tmp.list.iape.mean)
        dimnames(out.df)[[1]] <- 1:dim(out.df)[1]
        dimnames(out.df)[[2]] <- c("Count","CV","APE")
        print(out.df)
      }
  else
    if(plot.type=="age.diff")
      {
        plot(xin.df[,1],(xin.df[,1]-xin.df[,2]), ...)

        if(add.1to1==TRUE)
        {abline(h=0,lty=1)}

        if(add.actual==TRUE)
        {tmp.lm <- lm((xin.df[,1]-xin.df[,2])~xin.df[,1]);abline(tmp.lm,lty=3)}
        
        
      }
  else
    if(plot.type=="age.diff.num")
      {

        tmp.tab <- table((xin.df[,1]-xin.df[,2]),xin.df[,1])
        print(tmp.tab)


        tmp.x.names <- as.numeric(colnames(tmp.tab))
        tmp.y.names <- as.numeric(rownames(tmp.tab))

        tmp.x <- unlist(lapply(tmp.x.names,rep,length(tmp.y.names)))
        tmp.y <- rep(tmp.y.names,length(tmp.x.names))
        tmp.z <- as.vector(tmp.tab)
        index <- ifelse(tmp.z>0,TRUE,FALSE)
        tmp.x <- tmp.x[index]
        tmp.y <- tmp.y[index]
        tmp.z <- tmp.z[index]
        print(data.frame(tmp.x,tmp.y,tmp.z))
        
        plot(tmp.x,tmp.y,type="n",...)
        text(tmp.x,tmp.y,labels=tmp.z,adj=c(0.5,0.5))

        if(add.1to1==TRUE)
        {abline(h=0,lty=1)}

        if(add.actual==TRUE)
        {tmp.lm <- lm((xin.df[,1]-xin.df[,2])~xin.df[,1]);abline(tmp.lm,lty=3)}

      }
  else
    if(plot.type=="age.bias.num")
    {
        tmp.tab <- table(xin.df[,1],xin.df[,2])    


        print(tmp.tab)

        tmp.x.names <- as.numeric(colnames(tmp.tab))
        tmp.y.names <- as.numeric(rownames(tmp.tab))

        tmp.x <- unlist(lapply(tmp.x.names,rep,length(tmp.y.names)))
        tmp.y <- rep(tmp.y.names,length(tmp.x.names))
        tmp.z <- as.vector(tmp.tab)
        index <- ifelse(tmp.z>0,TRUE,FALSE)
        tmp.x <- tmp.x[index]
        tmp.y <- tmp.y[index]
        tmp.z <- tmp.z[index]
        print(data.frame(tmp.x,tmp.y,tmp.z))
        
        plot(tmp.x,tmp.y,type="n",...)
        text(tmp.x,tmp.y,labels=tmp.z,adj=c(0.5,0.5))

        if(add.1to1==TRUE)
        {abline(coef=c(0,1),lty=1)}
      
        if(add.actual==TRUE)
        {tmp.lm <- lm(xin.df[,2]~xin.df[,1]);abline(tmp.lm,lty=3)}

        if(add.CV==TRUE)
        {
          tmp.label <- paste("CV = ",round(mean.CV,digits=error.CV),"%",sep="")
          text(posit.CV[1],posit.CV[2],labels=tmp.label,pos=4)
        }
      }
  else
    {
      stop("Not a valid plot.type try \"age.bias\", \"age.bias.num\", \"age.diff\", \"age.diff.num\", or \"age.CV\"")
    }

}
