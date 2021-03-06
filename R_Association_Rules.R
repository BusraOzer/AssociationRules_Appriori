#Kurulumlar

if (require('arules') == FALSE){
  install.packages('arules')
  library(arules)
}

if (require('arulesViz') == FALSE){
  install.packages('arulesViz')
  library(arulesViz)
}


#dataset okuma

getwd()
data <- read.csv("egitim_hepsi.csv")

#dataset yorumlama

str(data)
class(data)
dim(data)
summary(data)    


#Birliktelik kural� - Apriori Algoritmas�

min_supp <- 0.01 #destek degeri
min_conf <- 0.25 #guven degeri

#birliktelik araligini belirtme
#min_lenght <- 8 #minimum birliktelik kurali
#max_lenght <- 8  #maksimum birliktelik kurali 
#rules <- apriori(data,parameter = list(supp = min_supp, conf = min_conf,maxlen=max_lenght,minlen=min_lenght, target = "rules"), appearance = list(rhs=c("Class=H", "Class=M","Class=L"), default="lhs"), control = list(verbose=F))

#Birden fazla birliktelik kurallar� i�in apriori fonk.
rules <- apriori(data,parameter = list(supp = min_supp, conf = min_conf, target = "rules"), appearance = list(rhs=c("Class=H", "Class=M","Class=L"), default="lhs"), control = list(verbose=F))

inspect(rules) #kurallari goruntuleme
inspect(rules[1:25]) #ilk 25 kural

#Confidence g�re s�ralama
Rules_Conf <- sort(rules, by="confidence", decreasing=TRUE)
inspect(Rules_Conf )
inspect(head(Rules_Conf, n=50))
inspect(tail(Rules_Conf, n=15))

#Support g�re s�ralama
Rules_Sup <- sort(rules, by="support", decreasing=TRUE)
inspect(Rules_Sup)
inspect(head(Rules_Sup, n=15))
inspect(tail(Rules_Sup, n=15))


#kural� tablo haline getirme
rules_df <- data.frame(lhs = labels(lhs(rules)), rhs = labels(rhs(rules)), rules@quality)
rules_df$lhs <- gsub("[{}]", "", rules_df$lhs)
rules_df$rhs <- gsub("[{}]", "", rules_df$rhs)
rules_df <- rules_df[order(rules_df$confidence, decreasing = T),] 
#View(rules_df)
#rules_df <- rules_df[order(rules_df$count, decreasing = T), ]


#G�rselle�tirme
plot(rules, main="Scatter Plot for Association Rules")
plot(rules[1:25], main="Scatter Plot for Association Rules")


#noktalar�n toplam� kural # rules:ka� adet lsh oldu�u
plot(rules[1:25], method = "grouped",control = list(k = 20))


dev.off()






