library(RCurl)
library(XML)
library(rvest)


get_month <- function(text){
  if(length(text)==0 | nchar(text) < 10){
    n=NA
  } else if (grepl("Jan", text)==TRUE){
    n="1"
  } else if (grepl("Feb", text)==TRUE){
    n="2"
  } else if (grepl("Mar", text)==TRUE){
    n="3"
  } else if (grepl("Apr", text)==TRUE){
    n="4"
  } else if (grepl("May", text)==TRUE){
    n="5"
  } else if (grepl("Jun", text)==TRUE){
    n="6"
  } else if (grepl("Jul", text)==TRUE){
    n="7"
  } else if (grepl("Aug", text)==TRUE){
    n="8"
  } else if (grepl("Sep", text)==TRUE){
    n="9"
  } else if (grepl("Oct", text)==TRUE){
    n="10"
  } else if (grepl("Nov", text)==TRUE){
    n="11"
  } else if (grepl("Dec", text)==TRUE){
    n="12"
  }
  return(n)
}



movies_table <- data.frame(matrix(data = NA, nrow=0, ncol=2),stringsAsFactors = FALSE)

for(i in c(1:8)){
  url <-paste0("http://www.boxofficemojo.com/alltime/world/?pagenum=",i,"&p=.htm")
  url2 <- getURL(url)
  parsed <- htmlParse(url2)
  links <- xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
  links <- data.frame(links)
  for(i in c(1:length(links$links))){
    links[i,2] <- grepl("/mov", links[i,1])
    
  }
  links <- links[links$V2==TRUE,]
  links <- links[-1,]
  movies_table <- rbind(movies_table, links)
}


movies_data <- data.frame(matrix(data = NA, nrow=0, ncol=4),stringsAsFactors = FALSE)
percent_complete <- 0

for(i in c(1:length(movies_table$links))){
  temp_data <- NULL
  
  url <- paste0("http://www.boxofficemojo.com",movies_table[i,1])
  
  webpage <- read_html(url)
  
  name_data_html <- html_nodes(webpage,"br+ font b")
  name_data <- html_text(name_data_html)
  name_data <- name_data[[1]]
  
  gross_data_html <- html_nodes(webpage,".mp_box_content tr+ tr td+ td b")
  gross_data <- html_text(gross_data_html)
  if(length(gross_data)==0){
    prod_data <- NA
  } else{
    gross_data <- gsub(",","",gross_data[[1]])
    gross_data <- substr(gross_data,2,nchar(gross_data))
    gross_data <- as.numeric(gross_data)
  }
  
  prod_data_html <- html_nodes(webpage,"center tr:nth-child(4) td+ td b")
  prod_data <- html_text(prod_data_html)
  if(length(prod_data)==0){
    prod_data <- NA
  } else{
    prod_data <- prod_data[[1]]
    prod_data <- substr(prod_data, 2, 4)
    prod_data <- as.numeric(prod_data)*1000000
  }
  
  release_data_html <- html_nodes(webpage,"nobr")
  release_data <- html_text(release_data_html)
  year <- substr(release_data, nchar(release_data)-3, nchar(release_data))
  month <- get_month(release_data)
  day <- substr(release_data, nchar(release_data)-7, nchar(release_data)-6)
  day <- gsub(",","",day)
  if(is.na(month)==TRUE){
    release_data <- NA
  } else{
    release_data <- as.Date(paste0(year,"-",month,"-",day))
  }
  
  temp_data <- data.frame(name_data,gross_data,prod_data,release_data)
  movies_data <- rbind(movies_data, temp_data)
  
  percent_complete <- percent_complete + (100/(length(movies_table$links)))
  print(paste(round(percent_complete,digits=2),"% complete",sep=""))
}

movies_data <- movies_data[complete.cases(movies_data),]
movies_data$perc_return <- 100*movies_data$gross_data/movies_data$prod_data

movies_data$days_since_jan_1 <- NA
for(i in c(1:length(movies_data$name_data))){
  year <- substr(movies_data[i,4],1,4)
  movies_data[i,6] <- as.numeric(movies_data[i,4]-as.Date(paste0(year,"-01-01")))
}

write.csv(movies_data, file = "movies_data.csv", row.names = FALSE)