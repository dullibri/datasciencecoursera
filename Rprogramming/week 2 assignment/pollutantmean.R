pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  for (i in 1:length(id)){
    id_char=as.character(id[i])
    if (nchar(id_char)==1){
      id_char <- paste('00',id_char,sep='')
    }
    if (nchar(id_char)==2){
      id_char <- paste('0',id_char,sep='')
    }
    df <- read.csv(paste(directory,'/',id_char,'.csv',sep=''))
    if (!'data'%in%ls()){
      data <- df
    }else{
      data <- rbind(data,df)
    }     
  }
  result <- mean(data[,pollutant],na.rm=T)
  return(result)
}