getwd()
setwd("/Users/baljindersmagh/Desktop/Data Mining/HW-3")
forest_data<-read_baskets("forests.txt")
#Question_1
rules1<-apriori(forest_data,parameter = list(supp=0.5,target="frequent itemsets"))
show<-inspect(rules1,by="support")
show
#q2<-is.maximal(rules1)
#show2<-inspect(rules1[q2],by="support")
#arules::itemFrequencyPlot(forest_data,topN=20,main='Plot',type="relative",ylab="Item Frequency (Relative)",xlab="items")
#class(forest_data)

        
summary(rules1)
#arulesViz::plotly(max_frequent_1)
#Question_2
max_frequent<-apriori(forest_data,parameter = list(supp=0.5,target="maximally frequent itemsets"))
max_frequent_1<-inspect(max_frequent,by="support")
plot(max_frequent_1,method="paracord",control=list("items"))

#Question_3
apriori_sup_conf<-apriori(forest_data,parameter = list(supp=0.4,conf=0.7,target="rules"))
show3<-inspect(sort(apriori_sup_conf))
