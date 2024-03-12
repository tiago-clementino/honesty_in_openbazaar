


#' Title complete
#'\code{complete} Completes the missing preferences values in a matrix of preferences following the additive consistency property
#' @param preferences  a matrix where all the incomplete experts preferences are included, that is, if there are n alternatives and h experts it receives a matrix of
#' h*n rows and n cols
#'
#' @return the complete matrix of preferences

complete=function(preferences)
{
  size=dim(preferences);

  nFil=size[1];
  nCol=size[2];
  N=nFil;
  pref_est=preferences;
  conta=0;
  cp=0;
  #while (dim(which(preferences==-1))!=NULL)
  #tab=table(is.na(preferences));

  while (dim(which(is.na(preferences),arr.ind=TRUE))>0)
  {
    index=which(is.na(preferences),arr.ind=TRUE);
    numberIncom=dim(index)[1];
    for (h in 1:numberIncom)
    {
        i=index[h,1];
        k=index[h,2];
        if(i != k)
        {
          if (is.na(preferences[i,k])) #Estimate
          {

                  aux=estimate (i,k, preferences,1);
                  if (!is.na(aux))
                  {
                    conta=conta+1;
                    cp=cp+aux;
                  }
                  aux=estimate (i,k, preferences,2);
                  if (!is.na(aux))
                  {
                    conta=conta+1;
                    cp=cp+aux;

                  }
                  aux=estimate (i,k, preferences,3);


                  if( !is.na(aux))
                  {
                    conta=conta+1;
                    cp=cp+aux;
                  }
                  if (conta>0)
                  {
                    cp=cp/conta;

                    if (cp<0)
                    {
                      cp=0;
                    }
                    else
                    {
                      if (cp>1)
                      {
                        cp=1;
                      }
                    }
                    preferences[i,k]=cp;

                    print(i)
                    print(k)
                    cp=0;
                    conta=0;

                  }
                  else
                  {
                    print("-----------------------------")
                    print(i)
                    print(k)
                    print('no he podido estimar');
                    print("-----------------------------")
                  }



            }
        }

    }
  }
  return(preferences)
}

#' Estimates the missing value using the additive consistency properties
#'
#'
#' @param i number of row where the preference is located
#' @param k number of column where the preference is located
#' @param preferences matrix of preferences
#' @param type type of estimation
#'
#' @return the estimated value
#' @export
#'
#' @examples
estimate=function(i,k,preferences,type)
{
  cp=0;
  cont=0;

  size=dim(preferences);

  nFil=size[1];
  nCol=size[2];
  N=nFil;
  for (j in 1:nFil)
  {

    if(type==1)
    {


      if ((i!=j) && (j!=k) && (!is.na(preferences[i,j])) && (!is.na(preferences[j,k])))
      {
        cp=cp+preferences[i,j]+preferences[j,k]-0.5;
        cont=cont+1;
      }


    }

    if(type==2)
    {


      if (j!=k && j!=i && !is.na(preferences[j,k]) && !is.na(preferences[j,i]))
      {
        cp=cp+preferences[j,k]-preferences[j,i]+0.5;
        cont=cont+1;

      }
    }

    if(type==3)
    {
      if (i!=j && j!=k && !is.na(preferences[i,j]) && !is.na(preferences[k,j]))
      {

        cp=cp+preferences[i,j]-preferences[k,j]+0.5;
        cont=cont+1;
      }
    }
  } #END for

  if (cont>0)
  {
    cp=cp/cont;
  }
  else{
    cp=NA;
  }

}

