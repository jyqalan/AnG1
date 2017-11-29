calc.iape <- function(xin,print.level=2,return.iape=FALSE)
{
  # A function to calculate the Index of Average Percentage Error (IAPE)
  # for fish otolith readings and return some useful results...
  # --------------------------------------------------------------------
  # MJM 22 June 2002

  # Requires:
  # xin -- a matrix or dataframe of fish otolith readings
  # print.level -- degree of detail in the results

  # calc.ae
  # -------
  calc.ae <- function(tmp.xin)
    {
      ae <- (1/R)*(apply(((abs(clean.xin-xj)/xj)),1,sum))
      return(ae)
    }  
  
  # Clean xin, remove NAs...
  # ------------------------
  clean.xin <- na.omit(as.matrix(xin))

  # Calculate IAPE
  # --------------
  #N <- nrow(as.data.frame(clean.xin))
  N <- nrow(clean.xin)

  #R <- ncol(as.data.frame(clean.xin))
  R <- ncol(clean.xin)

  if(R<2)
    {
      stop("\nHey, xin has less than two columns, try again idjit...\n")
    }
  else
    {
      xj <- (1/R)*(apply(clean.xin,1,sum))

      ae <- ifelse(xj==0,0,calc.ae(tmp.xin=clean.xin))

      ape <- 100*ae

      iape <- 100*((1/N)*(sum(na.omit(ae))))

      out.df <- data.frame(clean.xin,xj,ape)
    }

  # Print results
  # -------------
  if(print.level==0)
    {
      # i.e. do nothing...
    }
  else
    if(print.level==1)
      {
        cat("\nIAPE:\t",iape,"\n")
        cat("N:\t",N,"\n")
        cat("R:\t",R,"\n")
    }
  else
    if(print.level==2)
      {
        cat("\nReadings:\n")
        print(out.df)

        cat("\nIAPE:\t",iape,"\n")
        cat("N:\t",N,"\n")
        cat("R:\t",R,"\n")
      }

  # Return IAPE?
  # ------------
  
      
  
  if(return.iape==TRUE)
    {
      out.list <- list(out.df,iape)
      names(out.list) <- c("out.df","iape")
      invisible(out.list)
    }

}
