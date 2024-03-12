
#' Calculates the distance between two elements
#'
#' @param a Firt element
#' @param b Second element
#' @param type Type of distance, could be 'manhattan', 'euclidean', 'cosine', 'dice', 'jacard'
#'
#' @return d the distance between the two given elements
#' @export
#'
#' @examples
distanceCalculation=function(a,b,type)
{

  if (type=='manhattan'){d=manhattan(a,b);}
  else if (type=='euclidean') {d=euclidean(a,b);}
  else if (type=='cosine') {d=cosine(a,sqrt(1-a*a),b,sqrt(1-b*b));}
  else if(type=='dice') {d=dice(a,sqrt(1-a*a),b,sqrt(1-b*b));}
  else if(type=='jacard') {d=jacard(a,sqrt(1-a*a),b,sqrt(1-b*b));}
  return(d);

}

manhattan=function(a,b)
{

  d=sum(abs(a-b));
  return(d)
}


euclidean=function(a,b)
{
  d=sqrt(sum((a-b)*(a-b)));
  return(d);
}

cosine=function(a,ma,b,mb)
{
  den=sqrt(sum(a*a)+sum(ma*ma))*sqrt(sum(b*b)+sum(mb*mb));

  num=sum(a*b)+sum(ma*mb);
  d=num/den;

  return(sqrt(1-d*d));
}

dice=function(a,ma,b,mb)
{
  #den=sqrt(sum(a*a))+sqrt(sum(b*b));
  #den=sum(a*a)+sum(b*b);
  den=sum(a*a)+sum(ma*ma)+sum(b*b)+sum(mb*mb);
  num=2*sum(a*b)+sum(ma*mb);
  #d=2*sum(a*b)/den;
  d=num/den;
  return(1-d);
}

jacard=function(a,ma,b,mb)
{
  num=sum(a*b)+sum(ma*mb);

  den=sum(a*a)+sum(ma*ma)+sum(b*b)+sum(mb*mb)-num;

  d=num/den;
  return(1-d);
}



#' Computes the weights in the aggregation when a Yager Ordering weighting averaging operator (OWA) is used
#'
#' @param r
#' @param type Type of quantifier to be used in the aggregation, could be 'leasthalf', 'most' and 'asmany'.
#'
#' @return weight for the BUM function
#' @export
#'
#' @examples
weight=function(r,type)
{

  if( type=='leasthalf')
  {
    a=0;
    b=0.5;
  }
  else if(type=='most'){
    a=0.3;
    b=0.8;
  }
  else if(type=='asmany'){
    a=0.5;
    b=1;
  }
  if (r>=0 && r<a){q=0;}
  else if(r>=a && r<=b){q=(r-a)/(b-a);}
  else q=1;

  return(q);
}

#'
#' \code{agregation} aggregates all the experts preferences following and Yager OWA quantifier
#' @param preferences amtrix with the experts preferences
#' @param M Number of experts
#' @param N  Number of aternatives
#' @param quantifier Type of quantifier could be 'leasthalf', 'most', 'asmany'
#'
#' @return A NxN matrix with the preferences of all the experts aggregated using the given quantifier
#' @export
#'
#' @examples
agregation=function(preferences,M,N,quantifier)
{
  w=array(0,M);
  agregated=data.frame(matrix(0,1,N*N-N));
  a=matrix(0,1,M);

  #print('Estoy en agregacion GDMR2')
  for(i in 1:M){
    w[i]=weight(i/M,quantifier)-weight((i-1)/M,quantifier);
  }

  preferences=preferences;
  #colnames(preferences) <- paste('X',1:(N*N-N));
  agregated=apply(preferences,2,owa,w= w);
  return(agregated);
}


#' Exploitation
#'
#' @param agregated NxN matrix of preference where all the experts preferences has been included
#' @param N Number of expert
#' @param dominance Type of choice degree "GDD" or "GNDD"
#' @param quantifier Type of quantifier
#'
#' @return
#' @export
#'
#' @examples
exploitation=function(agregated,N,dominance,quantifier)
{
  #quantifier='most';
  #dominance degree
  w=array(1,N);
  GDD=matrix(0,1,N);
  GNDD=matrix(0,1,N);
  ps=matrix(0,1,N);
  agregated_2=matrix(0,N,N);

  for (i in 1:N){
    for (j in 1:N){
      if(i==j){
        agregated_2[i,j]=0.0;
      }else if(j>i){
        agregated_2[i,j]=agregated[(i-1)*(N-1)+(j-1)];
      }else{
        agregated_2[i,j]=agregated[(i-1)*(N-1)+j];
      }
    }
  }

  for (i in 1:N){
    w[i]=weight(i/N,quantifier)-weight((i-1)/N,quantifier);
  }
  for (i in 1:N){
    GDD[i]=owa(agregated_2[i,],w);
    for (j in 1:N){
      ps[j]=max(c(agregated_2[j,i]-agregated_2[i,j],0));
    }
    GNDD[i]=owa(1-ps,w);

  }
  #GDD
  #GNDD
  if(dominance=='GDD')
  {
    final=max(GDD);
  }
  else{final=max(GNDD);}
  tabla=rbind(GDD/max(GDD),GNDD/max(GNDD));
  rownames(tabla)=c('GDD','GNDD');
  colnames(tabla) <- 1:N;

  return (tabla);
}


#' owa Aggregates the elementes in the vector A following an Ordering Weighting Averaging operator, OWA, with the weights provided as a parameter
#' @param A number of alternatives
#' @param w Weights for the aggregation
#'
#' @return h Aggregated value
#' @export
#'
#' @examples
owa=function(A,w){
  h=sum(sort(A,decreasing = TRUE, na.last = TRUE)*w);
  return(h);
}


#'
#' \code{proximity} returns the proximity mensure on level of experts and alternatives
#' @param M Number of experts
#' @param N  Number of aternatives
#' @param preferences matrix with the experts preferences
#' @param agregated matrix with the consensus agregated preferences
#' @param distance Type of distance mensure could be 'manhattan', 'euclidean', 'cosine', 'dice' e 'jacard'
#'
#' @return
#' @export
#'
#' @examples
proximity=function(M,N,preferences,agregated,distance)
{
  #***********CONSENSUS MEASURES************************************
  #Level 1 Preference level
  #Obtengo la Combinaci?n de m elemetos tomados de n en matrices de dimensi?n nxn
  #browser();

  npref=N*N-N;
  prox_level1=data.frame(matrix(0,M,npref));
  dist_level1=data.frame(matrix(0,M,npref));
  prox_level2=data.frame(matrix(0,M,N));
  dist_level2=data.frame(matrix(0,M,N));
  aux=vector();
  cont=1;
  for (m1 in 1:M)
  {
        for (i in 1:npref)
        {
          a=preferences[m1,i];
          b=agregated[i];
          dist_level1[m1,i]=distanceCalculation(a,b,distance);
        }
  }
  prox_level1=1-dist_level1;
  #Level 2 Alternative Level
  cont=1;
  for (m in 1:M)
  {
    for (i in seq(1, ((N-1)*(N-1)+1), N-1))
    {
      #substituir por owa
      a=preferences[m,i:(i+N-2)];
      b=agregated[i:(i+N-2)];
      valor=distanceCalculation(a,b,distance);
      aux=c(aux,valor)
    }


    dist_level2[cont,]=aux;
    cont=cont+1;
    aux=vector();
  }
  prox_level2=1-dist_level2;

  #Proximity level3
  #think about replace this by owa
  proximity_level3_experts=apply(prox_level2, 1, mean);

  proximity_level3_alternatives=apply(prox_level2, 2, mean);

  proximity_level1_paired_alternatives=apply(prox_level1, 2, mean);

  return (list(proximity_level3_experts,prox_level2,prox_level1));
}

#'
#' \code{consensus} returns the global consensus mensure
#' @param M Number of experts
#' @param N  Number of aternatives
#' @param preferences amtrix with the experts preferences
#' @param agregated amtrix with the experts preferences
#' @param distance Type of distance mensure could be 'manhattan', 'euclidean', 'cosine', 'dice' e 'jacard'
#' @param quantifier
#' @param level
#'
#' @return The scalar global consensus value
#' @export
#'
#' @examples
consensus=function(M,N,preferences,agregated,distance,quantifier,level=3)
{
  #***********CONSENSUS MEASURES************************************
  #Level 1 Preference level
  #Obtengo la Combinaci?n de m elemetos tomados de n en matrices de dimensi?n nxn
  #browser();

  npref=N*N-N;
  numMa=(M*M-M)/2;#simplified from #factorial(M)/(factorial(2)*factorial(M-2));
  consensusExperts=data.frame(matrix(0,M,M))
  level1=data.frame(matrix(0,M,npref));#number of paired expert comparisons X number of paired alternatives comparisons (all possible preferences for each expert)

  for (m1 in 1:M)
  {
    for (i in 1:npref)
    {
      a=preferences[m1,i];
      b=agregated[i];
      level1[m1,i]=distanceCalculation(a,b,distance);
    }
  }

  similarity_level1=1-level1;

  w=array(1,M);
  for (i in 1:M){
    w[i]=weight(i/M,quantifier)-weight((i-1)/M,quantifier);
  }

  consensus_level1=apply(similarity_level1, 2, owa, w=w);

  #Level 2 Alternative Level

  consensus_level2=data.frame(matrix(0,1,N));

  w=array(1,N-1);
  for (i in 1:(N-1)){
    w[i]=weight(i/(N-1),quantifier)-weight((i-1)/(N-1),quantifier);
  }

  count=1;
  for (i in seq(1, (N*N-N), N-1))
  {
    consensus_level2[count]=owa(consensus_level1[i:(i+N-2)],w);
    count=count+1;

  }

  # Level 3 on experts

  w=array(1,N);
  for (i in 1:N){
    w[i]=weight(i/N,quantifier)-weight((i-1)/N,quantifier);
  }

  consensus_level3=owa(consensus_level2,w);

  if(level==2){
    consensus_level3=apply(consensus_level2, 1, mean);
  }else if(level==1){
    consensus_level3=mean(consensus_level1);
  }

  return (consensus_level3);
}





completenes=function(N,preference)
{

  numberIncom=length(which(is.na(preference)));
  completennesLevel=((N*N-N)-numberIncom)/(N*N-N);
  return (completennesLevel);
}


dataFrame2Matrix=function(df,N)
{

  contador=1;
  matriz=matrix(NA,N,N)
  for (i in 1:N)
  {
    for (j in 1:N)
    {
      if(i==j)
      {
        matriz[i,j]=0.5;
      }
      else
      {
        matriz[i,j]=df[contador];
        contador=contador+1;
      }
    }
  }
  return (matriz);

}

matrix2dataFrame=function(matrix,N)
{


}
