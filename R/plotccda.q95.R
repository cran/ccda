plotccda.q95 <-
function(x, pl="max"){
	if(pl!="max"&(pl>length(x$nameslist)|pl==0)){
	stop("pl is not a valid grouping number")
	}
	if(is.null(x$RCDP)){
	stop("Missing RCDP. Save ccda.main with the output RCDP!")
	}
	k=which(x$difference==max(x$difference))
	par(mfrow=c(1,1))
	if(pl=="max")
	k=k
	else(k=pl)
	plot(density(x$RCDP[k,]*100),xlim=range(x$RCDP[k,]*100, x$ratio[k]*100),lwd=2, xlab="LDA-percentages (%)",main="")
	abline(v=x$ratio[k]*100,col="red",lwd=2)
	abline(v=x$q95[k]*100, col="blue",lwd=2)
	legend("topright", c("ratio", "q95", paste("difference=",round(x$difference[k]*100,digits=2),"%",sep="")), col=c("red","blue","white"),lty=1,lwd=2)
}