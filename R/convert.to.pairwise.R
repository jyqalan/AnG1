convert.to.pairwise <-  function(xin, row.col.values=FALSE,by.row=TRUE)
{
    # A function to convert tabulated data (e.g. otolith readings...)
    # to simple "pairwise" values (e.g. single pairs of otolith readings...)
    # ----------------------------------------------------------------------

    # MJM 12 Feb 2003

    # Error checking (that xin matches x.values by  y.values...) should go here

    tmp.1 <- NULL
    tmp.2 <- NULL


    if(row.col.values==FALSE)
    {
        row.values <- dimnames(xin)[[1]]
        col.values <- dimnames(xin)[[2]]

        xin <- as.matrix(xin)
    }
    else
    {
        stop("row.col.values=TRUE not implemented yet")
    }




    if(by.row==TRUE)
    {
        for(i in 1:length(row.values))
        {
            for(j in  1:length(col.values))
            {
                tmp.1 <-  append(tmp.1,rep(row.values[i],xin[i,j]))
                tmp.2 <-  append(tmp.2,rep(col.values[j],xin[i,j]))
            }
        }
    }
    else
        if(by.row==FALSE)
        {
            for(i in 1:length(col.values))
            {
                for(j in 1:length(row.values))
                {
                    tmp.1. <- append(tmp.1,rep(col.values[i],xin[i,j]))
                    tmp.2. <- append(tmp.2,rep(row.values[j],xin[i,j]))
                }
            }
        }


    xout <- cbind(as.numeric(tmp.1),as.numeric(tmp.2))
    return(xout)

}
