#library(lattice)
#library(scatterplot3d)
mapRepresentations=function(agregada, mydata, NAlternatives)
{


  agregatedDig=dataFrame2Matrix(agregated,NAlternatives)


  consAgregated=consistency(agregatedDig)

  agregada2=agregated[ -toDelete ]
  mydata=rbind(mydata,agregada2)
  row.names(mydata)[length(row.names(mydata))] <- "AGR";


  d <- dist(mydata) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=3) # k is the number of dim
  fit # view results

  # plot solution
  x <- fit$points[,1]
  y <- fit$points[,2]
  z <- fit$points[,3]

# Uncomment to save the plot
#   cad=paste("./graphics/Maps",round,sep="");
#   cad=paste(cad,".pdf",sep="")
#   pdf(cad)


  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
       main=paste("Experts' preferences map, round ",round),	 type="n",xlim=c(xmin, xmax), ylim=c(ymin, ymax))
  #Plot in black and white
  #text(x, y, labels = row.names(mydata), cex=1)
  # Plot with colors
  text(x, y, labels = row.names(mydata), cex=1,col=rainbow(length(x)))

# Uncomment to save the plot
  #mydadev.off()

  # scatterplot3d(x,y,z, data = NULL,
  #       main="3D Experts preferences map", type="n")
  # text(x, y,z, labels = row.names(mydata), cex=1)
  #

# Uncomment to save the plot
#   cad=paste("./graficos/Mapas3D",round,sep="");
#   cad=paste(cad,".pdf",sep="")
#   pdf(cad)


  z=t(as.vector(consistencyLevels));
  z=c(z,consAgregated)


  sd3=scatterplot3d(x,y,z, type="h", lwd=5, pch=" ",
              color=rainbow(length(x)), main=paste("Consistency vs experts' positions, round ",round),xlab="Coordinate 1", ylab="Coordinate 2",zlab="Global consistency")


  #Uncomment for black and white map
  sd3=scatterplot3d(x,y,z, type="p", lwd=5, pch=" ", main=paste("Consistency vs experts' positions, round ",round),xlab="Coordinate 1", ylab="Coordinate 2",zlab="Global consistency")
  text(sd3$xyz.convert(x,y,z), labels = row.names(mydata))

  # Uncomment to save the plot
  #dev.off()

  #Representacion 3d descomentar si es necesario
  #sd4=plot3d(x,y,z, type="h", lwd=5, pch=" ",col=rainbow(length(x)), main=paste("Experts' preferences map, round ",round),xlab="Coordinate 1", ylab="Coordinate 2",zlab="Experts' global consistency")
  #text(sd4$xyz.convert(x,y,z), labels = row.names(mydata))
  #plot3d(x,y,z, col="red", size=10)
}
