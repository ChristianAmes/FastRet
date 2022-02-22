validate.inputdata<- function(raw_data){

  if((!"RT" %in% colnames(raw_data))) stop("RT could not be found in excel sheet")
  
  
}
