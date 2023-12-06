AOCd01_1<-function(input_file_path){
data<-read.table(file = input_file_path)
output<-vector()
for(i in 1:nrow(data)){
ncar<-nchar(data$V1[i])
x<-1
for(x in 1:ncar){
if(is.na(as.integer(substring(data$V1[i],first = x,last = x)))==T){
  x=x+1
}
  else{break}}
car1<-substring(data$V1[i],first = x,last = x)

y=ncar
for(x in 1:ncar){
  if(is.na(as.integer(substring(data$V1[i],first = y,last = y)))==T){
    y=y-1
  }
  else{break}}
car2<-substring(data$V1[i],first = y,last = y)
num<-as.integer(paste0(car1,car2))
output<-c(output,num)
}
return(sum(output))
}
