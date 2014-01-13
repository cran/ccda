percentage <-
function(dataset, starting_vector, prior){
  
  if(prior=="proportions"){ 
    confmtx=table(predict(lda(dataset,starting_vector), dataset)$class, true=starting_vector)
  }; # in case of different group sizes 
  
  if(prior=="equal"){
    confmtx=table(predict(lda(data,starting_vector, prior=c(rep(1/length(table(starting_vector)),times=length(table(starting_vector))))), data)$class, true=starting_vector)
  }; # in case of equal group sizes 
  
  perctg=sum(diag(confmtx))/sum(confmtx)
  return(perctg)
}
