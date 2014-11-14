corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  for (i in 1:332){
    id_char=as.character(i)
    if (nchar(id_char)==1){
      id_char <- paste('00',id_char,sep='')
    }
    if (nchar(id_char)==2){
      id_char <- paste('0',id_char,sep='')
    }
    df <- read.csv(paste(directory,'/',id_char,'.csv',sep=''))
    if (!'data'%in%ls()){data <- df}else{data <- rbind(data,df)}     
  }  
  id_cc <- sapply(1:332,function(x){
    y=sum(complete.cases(data[data$ID==x,]))
    result=c(id=x,nobs=y)
    return(result)})  
  id_cc <- t(id_cc)
 
  short_id=id_cc[id_cc[,2]>threshold,]
  if (length(short_id)==0){
    return(0)
  }else{
  result=matrix(NA,nrow(short_id),2)
  cc=data[complete.cases(data),]
  for (i in 1:nrow(short_id)){
    result[i,1]=i
    
    result[i,2]=cor(cc[which(cc$ID==short_id[i]),'sulfate'],cc[which(cc$ID==short_id[i]),'nitrate'])
  }
  return(result[,2])
  }
  
  
  
}