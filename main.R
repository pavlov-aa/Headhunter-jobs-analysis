### INSTALLATION--------
install.packages(c("httr", "jsonlite", "lubridate","SnowballC"))
install.packages("qdapDictionaries")
install.packages("tm")  # for text mining
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes

library("tm")
library("wordcloud")
library("RColorBrewer")
library("qdapDictionaries")
library("SnowballC")
library("httr")
library("jsonlite")
library("lubridate")

options(stringsAsFactors = FALSE) # dont change char to factors during the session (technical)
### USEFUL LINKS-------
# https://github.com/hhru/api/blob/master/docs/vacancies.md  # here full set of parameters that can be written in QUERY inside GET.
# https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/

### API calling--------
# HH gives no more than 2000 vacancies per query => 
url_hh<-"https://api.hh.ru/"
path <-"/vacancies"
# next we generate GET query to specific url. Inside QUERY several parameters are possible. For different days get, then combine
# r<-GET(url=url_hh,path=path,query=list(text="python",area=1,only_with_salary=TRUE,period=30,per_page=100,page=1))
getVacancies<-function (r) {fromJSON(rawToChar(r$content))[[2]]}
getCols<-function(table) {
  t<-as.data.frame(cbind(table$salary$to,table$salary$from,table$salary$gross,table$salary$currency,table$snippet$requirement,table$snippet$responsibility,table$name))
}

text<-'("python" AND ("numpy" OR "tensorflow" OR "keras")) or "machine learning" or "data mining" or "компьютерное зрение" or "распознавание речи" or "нейронные сети" or "nlp" or "computer vision" or "построение моделей" or "аналитик данных" OR "машинное обучение" OR "big data" OR "специалист по анализу данных" OR ("аналитик" AND "SAS") OR ("deep learning") OR "data analyst"'
r<-GET(url=url_hh,path=path,query=list(text=text,area=1,per_page=100,page=1))
data<-getVacancies(r)
data<-getCols(data)
colnames(data)<-c("salary_to","salary_from","salary_gross","salary_curr","req","resp","name")
unique(sort((data$name)))


### GETTING DATA --------
for ( i in 2:20){
Sys.sleep(3)
print(paste("iteration #",i))
r<-GET(url=url_hh,path=path,query=list(text=text,area=1,period=30,per_page=100-5*trunc(i/20),page=i))
table2<-getVacancies(r)
table2<-getCols(table2)
colnames(table2)<-c("salary_to","salary_from","salary_gross","salary_curr","req","resp","name")
data<-rbind(data,table2)
}

### END OF GETTING DATA-------
rm(table2,i,r,path,url_hh,getCols,getVacancies)
setwd("/home/alexander/Рабочий стол/diploma")
write.csv(data,"vacancies_08052018.csv")

### PREPROCESSING --------

setwd("/home/alexander/Рабочий стол/diploma")
data<-read.csv("vacancies_08052018.csv")[-1]


data$salary_curr[data$salary_curr == "RUR"] <- "1"
data$salary_curr[data$salary_curr == "EUR"] <- "70"
data$salary_curr[data$salary_curr == "USD"] <- "60"
data$salary_curr<-as.numeric(data$salary_curr)

data$salary_gross[data$salary_gross=="TRUE"]<-"1"
data$salary_gross[data$salary_gross=="FALSE"]<-"0.87"
data$salary_gross<-as.numeric(data$salary_gross)

data$salary_to<-data$salary_to*data$salary_curr/data$salary_gross
data$salary_from<-data$salary_from*data$salary_curr/data$salary_gross

data$salary_curr<-NULL
data$salary_gross<-NULL
#write.csv(data,"vacancies_08052018_all_in_RUR_and_gross.csv")
table(is.na(data$salary_from)+is.na(data$salary_to))
data2<-data[!is.na(data$salary_to) & !is.na(data$salary_from),]
mean((data2$salary_to-data2$salary_from)/data2$salary_from*100)

#####-----

data<-read.csv("vacancies_08052018_all_in_RUR_and_gross.csv")[-1]
data<-data[!is.na(data$salary_from),]
write.csv(data,"data_with_sal.csv")


hist(data$salary_to/1000,breaks=50)
# data<-data[!is.na(data$salary_from) & data$salary_curr=="RUR",] # only in rubles, only with known min salary
singleString = paste(data$req, encoding = 'UTF-8', collapse = " ")
# Далее, удаляем пунктуаторы:
clean_string = gsub('[".,/!/?:–«»()-+*%;><]', ' ', singleString)
clean_string
stop_list<-c("highlighttext","&quot"," а "," и ","UTF-8"," от "," за "," в "," с ", " по "," на ", " при ", " для "," к "," из ", " со "," о ", " их ", " все "," до ", " без "," об ", " у ",0,1,2,3,4,5,6,7,8,9,"  ")
stop_list_2<-readLines("stoplist.txt")
stop_list<-unique(c(paste0(" ",stop_list_2," "),stop_list))

for (i in 1:length(stop_list)){
  clean_string=gsub(stop_list[i],' ',clean_string)
}



clean_string

# Заменяем все заглавные буквы строчными:
clean_string = tolower(clean_string)
# Составляем список слов:
words = unlist(strsplit(clean_string, "[ ]"))
words<-unique(words[words!=""])
# Наконец, стеммируем:
stemmedStory = wordStem(words, language = 'russian')
stemmedStory
#head(stemmedStory,20)
# Частотный словарик основ можно составить с помощью стандартной функции table():
dict = as.data.frame(table(stemmedStory))
dict$length<-nchar(as.character(dict$stemmedStory))

dict2 = as.data.frame(table(data$name))

# Пауза на экспертную разметку -------
# Подготовка данных для получения матрицы нулей и единиц ---------
# Посмотреть самые частотные основы можно отсортировав полученный датафрейм:
write.csv(dict,"skills.csv")

skills<-read.csv("skills.csv")
colnames(skills)<-c("number","word","freq","length","category")
us<-unique(skills$category)
us<-as.vector(us[us!=""])
sort(us)
data$req<-tolower(data$req)
#data$X<-NULL

# getting matrix of skills for every vacation-----
b<-matrix(0,nrow=nrow(data),ncol=length(us))

for (k in 1:length(us)){
  sof<-as.vector(skills[skills$category==us[k],2])
  
  for (j in 1:nrow(data)){
    
    for (i in 1:length(sof)) {
      a<-grepl(sof[i],data[j,5])
      b[j,k]<-a+b[j,k]
    }
  }
}
#

#
b[b > 0] <- 1
data_all<-data.frame(cbind(b,data$salary_from))
colnames(data_all)<-c(us,"sal")

data<-data_all[!is.na(data_all$sal),]
colSums(data)
# Mann-Whitney Test with results --------
values<-NULL
salary_with<-NULL
salary_without<-NULL
for (i in 1:47){
  values[i]<-wilcox.test(data[data[,i]==1,"sal"], data[data[,i]==0,"sal"],paired=F)$p.value
  salary_with[i]<-mean(data[data[,i]==1,"sal"])
  salary_without[i]<-mean(data[data[,i]==0,"sal"])
}

wilcox.test(data[data[,i]==1,"sal"], data[data[,i]==0,"sal"],paired=F)$p.value
data[data[,10]==1,"sal"]

result<-data.frame(cbind(colnames(data[,-(length(us)+1)]),values,salary_with,salary_without,salary_with-salary_without,colSums(data)[-(length(us)+1)]))

result$values<-as.numeric(result$values)
result$V6<-as.numeric(result$V6)
result$V5<-as.numeric(result$V5)

result$salary_with<-as.numeric(result$salary_with)
result$salary_without<-as.numeric(result$salary_without)

str(result)
write.csv(result,"result.csv",row.names = F)
