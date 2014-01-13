plotccda.results <-
function(x){
  ngroups=length(x$ratio)
  par(mfrow=c(1,2))
  plot(x$ratio*100, type="o", col="black", ylim=c(0,130),lwd=2, xlab="number of groups", ylab="LDA-percentages (%)", main="Results", font.main=3, xaxt="n")
  axis(1, at=1:ngroups)
  lines(x$q95*100, type="o", col="green", lty=2,lwd=2)
  lines(x$difference*100, type="o", col="blue", lty=3,lwd=2)	
  legend("top", c( "ratio", "q95", "d=ratio-q95"), cex=1, col=c("black", "green", "blue"), pch=c(1, 1, 1), lty=c(1, 2, 3),lwd=c(2,2,2))
  plot(x$difference*100, type="o", main="Difference", col="blue", lty=3,lwd=2, xlab="number of groups", ylab="LDA-differences (%)", font.main=3, xaxt="n")
  axis(1, at=1:ngroups)	
}

