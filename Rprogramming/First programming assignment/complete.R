complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  # 'H:/Coursera/datasciencecoursera/Rprogramming/First programming assignment'
  for (i in 1:length(id)){
    id_char=as.character(id[i])
    if (nchar(id_char)==1){
      id_char <- paste('00',id_char,sep='')
    }
    if (nchar(id_char)==2){
      id_char <- paste('0',id_char,sep='')
    }
    df <- read.csv(paste(directory,'/',id_char,'.csv',sep=''))
    if (!'data'%in%ls()){data <- df}else{data <- rbind(data,df)}     
  }  
#   result=by(data,data$ID,function(x)sum(complete.cases(x)))
  result <- sapply(id,function(x){
    y=sum(complete.cases(data[data$ID==x,]))
    result=c(id=x,nobs=y)
    return(result)})  
  return(as.data.frame(t(result)))
}