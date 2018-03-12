# 
# 
# 
# prep_word2vec2("cosam.csv","cosam_10000.txt",lowercase=T)   
# 
# origin<-"cosam.csv"
#   destination<-"cosam_10000.txt"
#   split_characters="\\W";lowercase=F
prep_word2vec2 <- function(origin,destination,
                           split_characters="\\W",lowercase=F)
{
  # strsplit chokes on large lines. I would not have gone down this path if I knew this
  # to begin with.
  non_choking_strsplit <- function(lines,...) {
    splitLineIfNecessary = function(line,limit=10000) {
      # recursive function.
      chars = nchar(line)
      if (chars < limit) {
        return(line)
      } else {
        first_half = substr(line,1,nchar(line) %/% 2)
        second_half = substr(line,1,nchar(line) %/% 2)
        return(c(splitLineIfNecessary(first_half),splitLineIfNecessary(second_half)))
      }
    }
    lines = unlist(lapply(lines,splitLineIfNecessary))
    unlist(strsplit(lines," "))
  }
  
  message("Beginning tokenization to text file at ", destination)
  if (dir.exists(origin)) {
    origin = list.files(origin,recursive=T,full.names = T)
  }
  
  getwd()
  cat("",file=destination,append=F)
  # filename="w2c_comment2.csv"
  for (filename in origin) {
    message("\n",filename,appendLF=F)
    con = read.csv(filename)
    head(con)
    dim(con)
    con<-as.character(con[,1])
    
    
    length(con)
    j<-1
    while(length(con)>j) {
      
      if(length(con) < j+999){
        cnt_n<-length(con)
      }else{
        cnt_n<-j+999
      }
      
      lines<-con[j:(cnt_n)]
      message(".",appendLF=F)
      words = non_choking_strsplit(lines,split_characters,perl=T)
      if (lowercase) {words=tolower(words)}
      cat(c(words," "),file=destination,append=T)
      j<-j+1000
    }
    
    # close(con)
    cat(c("\n"),file=destination,append=T)
    
  }
  silent = destination
  
  
}