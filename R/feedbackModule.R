
#'
#' \code{feedback} returns the preferences ajusted by feedbacks
#' @param agregated preferences agregated
#' @param preferencesDF preferencesDF
#' @param preferences preferences
#' @param M Number of experts
#' @param N  Number of aternatives
#' @param delta constant about consistence (future)
#' @param threshold limit for consensus
#' @param quantifier
#'
#' @return
#' @export
#'
#' @examples
feedback=function(agregated,preferencesDF,pref,M,N,delta,threshold,quantifier)
{

  EXPCH=list(matrix(0,1,M),data.frame(matrix(0,M,N)),data.frame(matrix(0,M,N*N-N)));
  APS=matrix(0,M,N+1);

  for (i in 1:M) {
    if ((preferencesDF[[1]][i])<threshold){
      EXPCH[[1]][i]=preferencesDF[[1]][i];
    }
    else{
      EXPCH[[1]][i]=NaN;
    }
  }


  for (i in 1:M) {
    if (!is.na(EXPCH[[1]][i])){
      for (j in 1:N) {
        if (preferencesDF[[2]][i,j]<threshold){
          EXPCH[[2]][i,j]=preferencesDF[[2]][i,j];
          for (k in 1:(N-1)) {
            if (preferencesDF[[3]][i,(j-1)*(N-1)+k]<threshold){
              #EXPCH[[3]][i,(j-1)*(N-1)+k]=preferencesDF[[3]][i,(j-1)*(N-1)+k];
              EXPCH[[3]][i,(j-1)*(N-1)+k]=pref[i,(j-1)*(N-1)+k];
            }
            else{
              EXPCH[[3]][i,(j-1)*(N-1)+k]=NaN;
            }
          }
        }
        else{
          EXPCH[[2]][i,j]=NaN;
          EXPCH[[3]][i,(j-1)*(N-1)+(1:(N-1))]=NaN;
        }

      }

    }else{
      EXPCH[[2]][i,(1:N)]=NaN;
      EXPCH[[3]][i,(1:(N*N-N))]=NaN;
    }
    agregation_aux=matrix(0,2,N*N-N);
    agregation_aux[1,]=data.matrix(EXPCH[[3]][i,]);
    agregation_aux[2,]=agregated;

    #EXPCH[[3]][i,]=agregation(agregation_aux,2,N,quantifier);
    EXPCH[[3]][i,]=apply(agregation_aux, 2, mean);
    EXPCH[[3]][i,is.na(EXPCH[[3]][i,])]<-pref[i,is.na(EXPCH[[3]][i,])];

  }

  return (data.matrix(EXPCH[[3]]));

}
