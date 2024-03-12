

#' generatePref Generates a matrix of preferences
#' @param n number of alternatives
#' @param consistent boolean paramater that indicates if the maxtrix of preference is consistent or not
#'
#' @return pref a matrix of preferences
#' @export
#'
#' @examples
generatePref=function(n,consistent)
{
  #pref= matrix(0,n,n);
  pref= matrix(0,1,n*n-n);

  for (i in 1:n){
    for(j in 1:n){
      if (i==j)
      {
        #pref[i,j]=0.5;
      }
      else
      {
        if(j>i)
        {

          #pref[i,j]=runif(1, 0, 1);
          pref[(i-1)*(n-1)+j-1]=runif(1, 0, 1);
          if (consistent)
          {
            #pref[j,i]=1-pref[i,j];
            pref[(j-1)*(n-1)+i]=1-pref[(i-1)*(n-1)+j-1];
          }

          else
          {
            #pref[j,i]=runif(1, 0, 1);
            pref[(j-1)*(n-1)+i]=runif(1, 0, 1);
          }

        }
      }
    }

  }
  return (pref);
}

#' generateIncompletePref Generates a matrix of incomplete preferences ramdomly colocated
#' @param n number of alternatives
#' @param consistent boolean paramater that indicates if the maxtrix of preference is consistent or not
#'
#' @return pref a matrix of preferences
#' @export
#'
#' @examples

generateIncompletePref=function(n,consistent)
{
  pref=generatePref(n,consistent);
  for (k in 1:(2*n))
  {
    index=sample(1:n, 2)
    i=index[1];
    j=index[2];
    if ((i!=j)&&(sample(1:n)==n))
    {
      pref[i,j]=NA;
    }

  }

  return(pref);
}


generateDF=function(valorRef, M, diferencia)
{
  dataset=(matrix(NA, nrow = M, ncol = 1))
  for (i in 1:M){
    valor=valorRef+sign(runif(1,-1,1))*runif(1,0,diferencia);
    if (valor<0)
      valor=0;
    if (valor > 1)
      valor =1;

    dataset[i,1]=valor;
  }

  return (dataset);
}


