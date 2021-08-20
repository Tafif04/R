#Import Library
library(psych) #using KMO Test
library(car) #using vif Test
library(NbClust)
library(clValid)
library(factoextra)

#Import Data
data_k.hierarki = read.csv("D:/Portofolio/K.Hierarki.CSV", sep = ";")
View(data_k.hierarki)
Kab.kota = data_k.hierarki[,1]
rownames (data_k.hierarki) = Kab.kota
data = data_k.hierarki[,-c(1)]
View(data)

#Statistic Descriptive
summary(data)

#Preprocessing Data
##Type Data
str(data)
##Missing Value
is.null(data)
##Normalization
###Z-Score
Zdata = as.data.frame(round(scale(data),3))
View(Zdata)

#Cek Asumption
##1. Sample Representativenes
KMO(Zdata)
##2. Multicolieneritas (VIF)
SUM = rowSums(data[,1:length(data)])
data_baru = data.frame(data, SUM)
View(data_baru)
Zdata_baru = as.data.frame(scale(data_baru))
model = lm(SUM ~.,data = Zdata_baru)
VIF = vif(model)
Multikolinearitas = round(data.frame(VIF),3)
View(Multikolinearitas)

#Model
##Distance (Euclidean)
Distance = dist(Zdata, method = "euclidean") #Using Ecludiean Distance
Distance

##Visualization Agglomerative Hierarchical
###1. Average Linkage
Average = hclust(Distance,method = "average")
win.graph()
plot(Average,col="black", sub = " ",hang=-1,main="Cluster Dendrogram: Average Linkage", xlab = "Kab/Kota")
###2. Single Linkage
Single = hclust(Distance,method = "single")
win.graph()
plot(Single,col="black", sub = " ",hang=-1,main="Cluster Dendrogram: Single Linkage", xlab = "Kab/Kota")
###3. Complete Linkage
Complete = hclust(Distance,method = "complete")
win.graph()
plot(Complete,col="black", sub = " ",hang=-1,main="Cluster Dendrogram: Complete Linkage", xlab = "Kab/Kota")
###4. Ward's Method
Ward = hclust(Distance,method = "ward.D2")
win.graph()
plot(Ward,col="black", sub = " ",hang=-1,main="Cluster Dendrogram: Ward's Method", xlab = "Kab/Kota")
###5. Centroid Method
Centroid = hclust(Distance,method = "centroid")
win.graph()
plot(Centroid,col="black", sub = " ",hang=-1,main="Cluster Dendrogram: Centroid Method", xlab = "Kab/Kota")

##Evalution
###The best method
hc_a = Average
d2_a = cophenetic(hc_a)
corave = cor(Distance, d2_a)

hc_s = Single
d2_s = cophenetic(hc_s)
corsig = cor(Distance, d2_s)

hc_co = Complete
d2_co = cophenetic(hc_co)
corcomp = cor(Distance, d2_co)

hc_w = Ward
d2_w = cophenetic(hc_w)
corward = cor(Distance, d2_w)

hc_ce = Centroid
d2_ce = cophenetic(hc_ce)
corcen = cor(Distance, d2_ce)

Metode = c("Average","Complete","Single","Ward","Centroid")
Korelasi = c(corave,corcomp,corsig,corward,corcen)
Cophenetic.Tabel = data.frame(Metode,Korelasi)
View(Cophenetic.Tabel)
#####Take the highest correlation coefficient value. 
###Number of Best Clusters (Internal Measure)
intern = clValid(Zdata, 2:4, clMethods = "hierarchical", validation = "internal",
                   metric = "euclidean", method = "average")
summary(intern)
optimalScores(intern)

##Profiling
best = hc_a
clust_optimum = 2
sub_grup = cutree(best, k = clust_optimum)
aggregate(data, by = list(sub_grup), mean)
Klaster = sub_grup
hasil.Klaster = data.frame(data,Klaster)
View(hasil.Klaster)
fviz_dend(best, cex = 0.6,k = clust_optimum, palette = "jco", rect = TRUE, rect_fill = TRUE, rect_border = "jco", labels_track_height = 3)