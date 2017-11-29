calc.age.cv <-  function(xin,print.level=2,return.cv=FALSE)
{
  # A function to calculate the CVs of a set of age readings
  # usually produced as the result of a reader comparison test
  # ----------------------------------------------------------
  # MJM 2 Oct 2002

  # calc.cvc
  calc.cv <- function(tmp.xin)
    {      
      CV <- 100* sqrt(apply((((clean.xin - xj)^2)/(R-1)),1,sum))/xj

      return(CV)
    }
  
  # Remove NAs
  clean.xin <- na.omit(as.matrix(xin))

  # Calculate CV(s)
  N <- nrow(clean.xin)
  R <- ncol(clean.xin)
  
  if(R<2)
    {
      stop("\nHey, xin has less than two columns, try again idjit...\n")
    }
  else
    {
      xj <- (1/R)*(apply(clean.xin,1,sum))
 
      CV <- ifelse(xj==0,0,calc.cv(tmp.xin=clean.xin))
      
      mean.CV <- sum(CV)/N

      out.df <- data.frame(clean.xin,xj,CV)
    }

  # Print output
  if(print.level==0)
    {
      # i.e. do nothing...
    }
  else
    if(print.level==1)
      {
        cat("\nmean.CV:\t",mean.CV,"\n")
        cat("N:\t",N,"\n")
        cat("R:\t",R,"\n")
    }
  else
    if(print.level==2)
      {
        cat("\nReadings:\n")
        print(out.df)

        cat("\nmean.CV:\t",mean.CV,"\n")
        cat("N:\t\t",N,"\n")
        cat("R:\t\t",R,"\n")
      }

  # Return CV?
  if(return.cv==TRUE)
    {
      out.list <- list(out.df,mean.CV)
      names(out.list) <- c("out.df","mean.CV")
      invisible(out.list)
    }
  
}
