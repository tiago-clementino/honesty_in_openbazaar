setClass("expert", representation(fuzzyPref = "matrix", compFuzzyPref= "matrix", id="numeric",consistencyLevel="numeric",completennesLevel="numeric"))


#setGeneric("createExpertTest", function(distri, ...) standardGeneric("createExpertTest"))

# setMethod("createExpertTest", "expert", function(n){
#
#   new("expert", fuzzyPref = generatePref(n), linPref= generatePref(n))
#
# })

# setMethod("generatePref", "expert", function(n) {
#
#     pref= matrix(0,n,n);
#
#     for (i in 1:n){
#       for(j in 1:n){
#         if (i==j)
#         {
#           pref[i,j]=0.5;
#         }
#         else
#         {
#           if(j>i
#           {
#             pref[i,j]=runif(1, 0, 1);
#             pref[j,i]=1-pref[i,j];
#           }
#         }
#       }
#
#     }
#     return (pref);
#
#   })



# expert <- new("expert", fuzzyPref = generatePref(4), linPref=generatePref(4))
#
# experts=vector("list", 5)
#
# for (i in 1:5)
# {
#   experts[i] <- new("expert", fuzzyPref = generatePref(4), linPref=generatePref(4))
#
# }
