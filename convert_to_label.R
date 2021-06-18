library(dplyr)
library(plyr)

data_df <- read.csv("Image Set 2 Labels.csv", stringsAsFactors = F)

for (i in 1:nrow(data_df)){
  annotation = unlist(strsplit(data_df$annotation[i],","))
  
  
  x1 <-  unlist(strsplit(annotation[1],":"))[2]
  x1 <-  gsub(" ", "",x1)
  
  y1 <-  unlist(strsplit(annotation[2],":"))[2]
  y1 <-  gsub("^\\s+|}","", y1)
  
  x2 <-  unlist(strsplit(annotation[3],":"))[2]
  x2 <-  gsub(" ", "",x2)
  
  y2 <-  unlist(strsplit(annotation[4],":"))[2]
  y2 <-  gsub("^\\s+|}|]","", y2)
  
  data_df$X1[i] = x1
  data_df$Y1[i] = y1
  data_df$X2[i] = x2
  data_df$Y2[i] = y2
  
  if (data_df$topic_id[i]==102){
    data_df$Label[i] <- paste(0,x1,y1,x2,y2,sep = " ")
  }else if (data_df$topic_id[i]==103){
    data_df$Label[i] <- paste(1,x1,y1,x2,y2,sep = " ")
  }else if(data_df$topic_id[i]==104) {
    data_df$Label[i] <- paste(2,x1,y1,x2,y2,sep = " ")
  }else{
    data_df$Label[i] <- paste(3,x1,y1,x2,y2,sep = " ")
  }
  
  filename <- unlist(strsplit(data_df$origin[i],"/"))[2]
  data_df$Filename[i] <- paste0(unlist(strsplit(filename,"\\."))[1],".txt") 
  
}

# Remove all .txt files
#do.call(file.remove, list(list.files("img/", pattern="*.txt")))
unlink("img/*.txt")

generate_text <- function(df){

df <- df %>% arrange(topic_id)

for (i in 1:nrow(df)){
  #writeLines(df$Label[i], fileConn)
  write(df$Label[i],file=paste0("img/",df$Filename[i]),append=TRUE)
}

}

test <- ddply(data_df, "Filename", generate_text)



