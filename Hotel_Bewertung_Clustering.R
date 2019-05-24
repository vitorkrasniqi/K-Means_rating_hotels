questionnaire <- read.csv("C:/Users/z0040ecz/Downloads/Group5_Assignment_Hotel (1)/Group5_Assignment_Hotel/questionnaire.csv") 
view(questionnaire)


str(questionnaire)
summary(questionnaire) 
boxplot(questionnaire)

# Str() ➔ 30 Einträge und 7 Attribute

# Der arithmetische Mittel und Median sind überall ähnlich.
# Amenitites und Service verfügen über Ausreisser
# PAGE 2
# Amenitites wurden am schlechtesten bewertet.
# Service wurde am besten bewertet.
# Quartilabstand: 75 % der Daten von Amentities sind schlechter als 47.75
# 75 % der Daten verfügen über Werte gleich oder kleiner als 47.75
# 50 % der Daten von facilities sind schlechter als 51.50

plot(questionnaire)
fourCluster <- kmeans(questionnaire,4, nstart=20) 
plot(questionnaire, col=fourCluster$cluster)
normalisiert <- (questionnaire - min(questionnaire))/(max(questionnaire) - min(questionnaire)) 
fourCluster <- kmeans(normalisiert,4, nstart=20)

# Mir war bewusst, dass der Output oftmals viel zu gross war.
# Deswegen wollte ich diese Daten zunächst normalisieren, damit ich mir ein besseres Bild machen kann
# und ich eventuell nur die grössen Verhältnisse bewerte!
#   
#   fourCluster$withinss: Gibt einen Vektor aus, welche den within-cluster Sum of Squares pro Cluster ausgibt.
# Wir wollen, dass diese Zahl möglichst klein ist, damit wir eine Homogenität innerhalb der Cluster haben.
# Leider müssen wir feststellen, dass vor allem drei Cluster einen sehr hohen Wert aufweisen.
# Wobei zwei Cluster einen ähnlichen Wert aufzeigen.
# 
# 
# fourCluster$totss: Dies würde sich auch so berechnen lassen:
# $totss = $tot.withinss + $betweenss ➔ Denn dies ist der totale SUM of Squares. 
# Auch dort wollen wir, dass er möglichst klein ist. 
# Doch leider ist diese Zahl viel zu gross. Denn es lässt sich sagen, dass:
# fourCluster$tot.whint / fourCluster$
# fourCluster$tot.withinss / fourCluster$totss
  
  
# Die Summe der Withinss der Cluster ist ziemlich gross. Wir können sagen, dass mit mehr als 40% vom gesamten 
# Sum of Squares diese Kennzahl stark vertreten ist.
# Wir wollen aber, dass diese Zahl möglichst klein ist!
#   
#   fourCluster$betweenss:Dies ist der arithmethische Mittel der Entfernungen zwischen den Clusterzentren.
# Man erwartet, dass dieses Verhältnis so hoch wie möglich ist, da wir uns heterogene Cluster wünschen.
# Hier könnte diese Zahl ein Bisschen grösser sein!
# 
# fourCluster$betweenss / fourCluster$totss: “The closer the cluster centers are to the global mean, the smaller
# is this fraction.” Nun fokussieren wir uns auf die betweens. Wir wollen, dass diese Zahl möglichst gross ist.
# Ich finde, dass diese Zahl grösser sein muss!
#   
#   
# • Der Cluster 2 hat am schlechtestens Abgeschnitten und verdient mehr Aufmerksamkeit vom Management.
# • Bei Value hat der zweite Cluster am schlechtesten abgeschlossen. Cluster Nummer 1 ist knapp nicht genügend!
# • Bei Complaint hat nur der zweite Cluster richtig schlecht abgeschlossen. Cluster Nummer 1 ist auch knapp ungenügend.
# • Bei Facilities haben eigentlich alle ungenügend abgeschlossen. Cluster Nummer 1 und 2 sind ungenügend. Cluster nummer 3 und 4 sind knapp ungenügend.
# • Bei Clean haben die ersten zwei Cluster ungenügend abgeschlossen. Der dirtte Cluster ist knapp ungenügend.
# • Bei athm haben die ersten zwei Cluster ungenügend abgeschlossen. Schlimm ist es aber nur beim zweiten Cluster.
# • Bei Service haben alle gut abgeschlossen, ausser der Cluster Nummer 2.
# • Bei amentities haben alle schlecht abgeschlossen.

## CHOOSING K
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(questionnaire, i)
}

k

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)")

for(i in 1:4){
  plot(iris, col = k[[i]]$cluster)
}


library(factoextra)
fviz_nbclust(questionnaire, kmeans, method = "wss")
fviz_nbclust(questionnaire, kmeans, method = "silhouette")
fviz_nbclust(questionnaire, kmeans, method = "gap_stat")

# 
# Gemäss der Elbogenmethode würde ich sagen, dass drei oder sechs Cluster Frage kommen würde.
# Da ich nicht sicher bin, welche ich davon wähle, 
# werde ich noch andere Methoden testen:
# 
#   
# Den Between SS/ Total SS haben wir schon vorher berechnet. Wir wollen, dass diese Zahl möglichst gross ist,
# damit die Clusters auch möglichst heterogen sind. Wir können sagen, 
# dass ab sechs Cluster dieser Ratio zufriedenstellend ist.
# 
# 
# Hier kommt es vor allem darauf an, welche Punkte hoch sind. 
# Somit würden die Punkte 2, 4, 6 und 8 in Frage kommen.
# 
# 
# Bei Gap_Stat will ich wissen, welcher Punkt der Idealste ist. Dafür muss ich untersuchen, 
# welche Punkte auf eine Steigung aufbauen und von einer negativen Steigung gefolgt sind.
# Hier kann ich sagen, dass die Punkte
# 2,4,6,8 in Frage kommen würden.
# 
# Nach meiner Meinung sind vier Cluster in Ordnung. Daraus lässt sich schon viel herauslesen und
# intepretieren. Jedoch würde ich trotzdem lieber mit Sechs Clustern arbeiten,
# da nach meiner Meinung dies idealer ist.
