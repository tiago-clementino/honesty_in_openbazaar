

consistencyDF=function(preferencesDF)
{

  consi=vector(mode = "numeric", length = M)
  for (i in 1:M)
  {
    prefe=t(matrix(preferencesDF[i,],nrow = N,ncol = N))
    consi[i]=consistency(prefe)

  }

  return(consi)


}


consistency=function(preferences)
{
  #N=5;
  #   preferences=generatePref(N,TRUE)
  estimada=preferences;
  conta=0;
  cp=0;
  #cp2=0;
  #cp3=0;
  epsilon=preferences;


  for (i in 1:N)
  {
    for (j in 1:N)
    {
      aux=estimate (i,j, preferences,1);
      if (!is.na(aux))
      {
        conta=conta+1;
        cp=cp+aux;
      }
      aux=estimate (i,j, preferences,2);
      if (!is.na(aux))
      {
        conta=conta+1;
        cp=cp+aux;

      }
      aux=estimate (i,j, preferences,3);

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
      }
      estimada[i,j]=cp;
      cp=0;
      conta=0;

    }

  }
  epsilon=abs(estimada-preferences);
  cl_preference_level<<-(1-epsilon);
  #print(cl_preference_level);

  cl_alternative_level<<-apply(cl_preference_level, 1,sum)/(2*(N-1));

  cl_relation_level<<-sum(cl_alternative_level)/N;

  return(cl_relation_level)
}


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
      i
      j

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
