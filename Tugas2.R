#library
library(dplyr)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(treemap)
library(OutlierDetection)
library(lattice)
library(grid)
library(DMwR)

#========EKSPLORASI DATA==========#

#1. FREKUENSI TRANSAKSI PER NEGARA
JumlahRecordPerNegara <- table(data_gabung_awal$Country)
View(JumlahRecordPerNegara)


ggplot(TransaksiPerNegara, aes(x=reorder(TransaksiPerNegara$Negara,-TransaksiPerNegara$Transaksi), y=TransaksiPerNegara$Transaksi)) +
  geom_bar(stat="identity", fill="#009999") + theme_bw() +
  xlab("Negara") +
  ylab("Jumlah Transaksi") +
  ggtitle("Jumlah Transaksi Per Negara")

#membuat frame data
TransaksiPerNegara <- ddply(data_gabung_awal, .(data_gabung_awal$Country), nrow)
names(TransaksiPerNegara) <- c("Negara", "Transaksi")
TransaksiPerNegara_urut <- TransaksiPerNegara[order(TransaksiPerNegara$Transaksi),]

#treemap
treemap(TransaksiPerNegara_urut, index="Negara", vSize = "Transaksi", 
        type="value", vColor="Transaksi", title = "Transaksi Per Negara")

#TOP 10
TransaksiPerNegara_TOP10 <- TransaksiPerNegara_urut[39:43,]
ggplot(TransaksiPerNegara_TOP10, aes(x=reorder(Negara,-Transaksi), y=Transaksi)) +
  geom_bar(stat="identity", fill="#009999") + theme_bw() +
  xlab("Negara") +
  ylab("Jumlah Transaksi") +
  ggtitle("TOP 10 Jumlah Transaksi Per Negara")

#BOTT 10
TransaksiPerNegara_BOT10 <- TransaksiPerNegara_urut[1:5,]
ggplot(TransaksiPerNegara_BOT10, aes(x=reorder(Negara,Transaksi), y=Transaksi)) +
  geom_bar(stat="identity", fill="#009999") + theme_bw() +
  xlab("Negara") +
  ylab("Jumlah Transaksi") +
  ggtitle("BOTTOM 10 Jumlah Transaksi Per Negara")

#2. Jumlah pelanggan unik di tiap negara
#buat data
CustomerbyCountry <- ddply(data_gabung_awal, .(Country, `Customer ID`), nrow)
names(CustomerbyCountry) <- c("Country", "CustomerID", "Frequency")
CustomerbyCountry <- ddply(CustomerbyCountry, .(CustomerbyCountry$Country), nrow)
names(CustomerbyCountry) <- c("Country", "Number of Unique Customer")
CustomerbyCountry <- CustomerbyCountry[order(-CustomerbyCountry$`Number of Unique Customer`),]
CustomerbyCountryclean <- head(CustomerbyCountry,4)
CustomerbyCountryclean[nrow(CustomerbyCountryclean) + 1,] = c("Other",sum(CustomerbyCountry$`Number of Unique Customer`[6:43]))
CustomerbyCountryclean$`Number of Unique Customer`<- as.numeric(CustomerbyCountryclean$`Number of Unique Customer`)
#buat pie
piepercent<- round(100*CustomerbyCountryclean$`Number of Unique Customer`/sum(CustomerbyCountryclean$`Number of Unique Customer`), 1)
pie(CustomerbyCountryclean$`Number of Unique Customer`, main = "Persentase Jumlah Pelanggan Berdasarkan Negara",
    labels = piepercent,
    col = rainbow(length(CustomerbyCountryclean$`Number of Unique Customer`)))
legend("topright", CustomerbyCountryclean$Country, cex = 0.6,
       fill=rainbow(length(CustomerbyCountryclean$`Number of Unique Customer`)))

#3. penjualan
qplot(data_gabung_awal$Price, geom="histogram",binwidth = 20, main = "Persebaran Harga Produk", xlab = "Price", 
      ylab = "Jumlah", fill=I("blue"), alpha=I(.2))
qplot(datacleanl$Price, geom="histogram",binwidth = 20, main = "Persebaran Harga Produk", xlab = "Price", 
      ylab = "Jumlah", fill=I("blue"), alpha=I(.2))

#total penjualan
require(lubridate)
dataclean09 <- dataclean[ dataclean$InvoiceDate < as.Date("2010-01-01"), ]
dataclean10 <- dataclean[dataclean$InvoiceDate > as.Date("2009-12-31") & dataclean$InvoiceDate < as.Date("2011-01-01"), ]
dataclean11 <- dataclean[dataclean$InvoiceDate > as.Date("2010-12-31"), ]
month09 <- month(as.Date(dataclean09$InvoiceDate, "%d-%b-%y"))
month10 <- month(as.Date(dataclean10$InvoiceDate, "%d-%b-%y"))
month11 <- month(as.Date(dataclean11$InvoiceDate, "%d-%b-%y"))
data_month09 <- tapply(dataclean09$Ammount, month09, sum)
data_month10 <- tapply(dataclean10$Ammount, month10, sum)
data_month11 <- tapply(dataclean11$Ammount, month11, sum)
plot(data_month10,type = "o",col = "red", xlab = "Bulan", ylab = "Total Penjualan", 
     main = "Total penjualan per Tahun " )
lines(data_month009, type = "o", col = "green")
lines(data_month11, type = "o", col = "blue")
legend("topleft", c("2009","2010","2011"), cex = 0.8,
       fill = c("green","red","blue"))
#data_month09 <- data.frame("Nomer" = 12, "Total Penjualan"=678379.6)

#4. Boxplot
ggplot(data_gabung_awal, aes(y=Price, fill=Price)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2) +
  theme(legend.position="none") +
  ggtitle("Boxplot Harga Barang (Price)")
ggplot(data_gabung_awal, aes(y=Quantity, fill=Price)) +
  geom_boxplot(varwidth = TRUE, alpha=0.2) +
  theme(legend.position="none") +
  ggtitle("Boxplot Kuantitas Barang yang dibeli (Quantity)")

#5. ammount pelanggan
TransaksiPerPelanggan <- ddply(data_gabung_awal, .(data_gabung_awal$`Customer ID`), nrow)
names(TransaksiPerPelanggan) <- c("ID Pelanggan", "Total Transaksi")
TransaksiPerPelanggan_urut <- TransaksiPerPelanggan[order(-TransaksiPerPelanggan$`Total Transaksi`),]
View(TransaksiPerPelanggan_urut)
TransaksiPerPelanggan_TOP10 <- TransaksiPerPelanggan_urut[1:10,]
TransaksiPerPelanggan_TOP10$`ID Pelanggan` <- as.character(TransaksiPerPelanggan_TOP50$`ID Pelanggan`)

ggplot(TransaksiPerPelanggan_TOP10, aes(x=reorder(`ID Pelanggan`,-`Total Transaksi`), y=`Total Transaksi`)) +
  geom_bar(stat="identity", fill="#fcb603") + theme_bw() +
  xlab("Pelanggan") +
  ylab("Jumlah Transaksi") +
  ggtitle("TOP 10 Pelanggan yang Sering Belanja")

#produk
ProductbyQuantity <- ddply(data_gabung_awal,.(StockCode),summarise,`Jumlah Terjual`= sum(Quantity))
ProductbyQuantity <- ProductbyQuantity[order(-ProductbyQuantity$`Jumlah Terjual`),]
ProductbyQuantityTOP20 <- ProductbyQuantity[1:20,]
ggplot(ProductbyQuantityTOP15, aes(x=reorder(StockCode,`Jumlah Terjual`), y=`Jumlah Terjual`,fill=`Jumlah Terjual`)) +
  geom_bar(stat="identity") + coord_flip() +
  xlab("Kode Produk") +
  ylab("Jumlah Terjual") +
  ggtitle("20 Produk Terlaris")

#penjualan menurut waktu
Max_week_sale <- filter(online_retail_2009_2010, !is.na(`Customer ID`),!is.na(StockCode))
Max_week_sale$InvoiceDate=mdy_hm(Max_week_sale$InvoiceDate)
Max_week_sale$Weekdays <- weekdays(Max_week_sale$InvoiceDate)
Max_week_sale$Sales <- Max_week_sale$Quantity*Max_week_sale$Price
highsales <- Max_week_sale %>% group_by(Max_week_sale$Weekdays) %>% summarize(SalesAmount=sum(Sales)) %>% arrange(desc(SalesAmount))
head(highsales)

#Customer yang paling banyak belanja
Max_week_sale %>% group_by(CustomerID) %>% summarise(Spend=sum(Sales)) %>% arrange(desc(Spend)) %>%head(5)

#========PRA PROSES==========#

#P1. Menggabungkan Dataset
data_gabung_awal <- rbind(online_retail_2009_2010, online_retail_2010_2011)


#P2. Redundan
data_noduplikat <- data_gabung_awal %>% distinct()

#P3. Missing value
#-apakah ada?
any(is.na(data_noduplikat))
#-dimana aja?
sapply(data_noduplikat, function(x) any(is.na(x)))
#-berapa yang N/A?
sapply(data_noduplikat, function(x) sum(is.na(x)))

#remove leading and trailing function
data_noduplikat$Invoice = as.character(data_noduplikat$Invoice)
trim = function (x) gsub("^\\s+|\\s+$", "", x)
data_noduplikat$Invoice = trim(data_noduplikat$Invoice)
data_noduplikat$Description = trim(as.character(data_noduplikat$Description))

#menghilangkan C
is_C = function (x) startsWith(x,"C")
data_tanpacancel = data_noduplikat[which(!is_C(data_noduplikat$Invoice)),] #subsetting

#hilangin null
data_tanpaNULLDesc = subset(data_tanpacancel,!is.na(data_tanpacancel$Description)) #subsetting
data_tanpaNULLCust = subset(data_tanpacancel,!is.na(data_tanpacancel$`Customer ID`)) #subsetting

#hapus yang negatif
data_tanpanegatif <- data_tanpaNULLCust[which(data_tanpaNULLCust$Quantity>=0 
                                  & data_tanpaNULLCust$Price >=0),]

#menghilangkan buzzword
is_Buzzword = function(x) {
    str_detect(toupper(x),"AWAY") | str_detect(toupper(x),"CHARGES") |
    str_detect(toupper(x),"FEE") | str_detect(toupper(x),"FAULT")
    str_detect(toupper(x),"SALES") | str_detect(toupper(x),"ADJUST") |
    str_detect(toupper(x),"COUNTED") | str_detect(toupper(x),"INCORRECT") |
    str_detect(toupper(x),"WRONG") | str_detect(toupper(x),"LOST") |
    str_detect(toupper(x),"CRUSHED") | str_detect(toupper(x),"DAMAGE") |
    str_detect(toupper(x),"FOUND") | str_detect(toupper(x),"THROWN") |
    str_detect(toupper(x),"SMASHED") | str_detect(toupper(x),"\\?") |
    str_detect(toupper(x),"BROKEN") | str_detect(toupper(x),"BARCODE") |
    str_detect(toupper(x),"RETURNED") | str_detect(toupper(x),"MAILOUT") | 
    str_detect(toupper(x),"DELIVERY")| str_detect(toupper(x),"MIX UP") | 
    str_detect(toupper(x),"MOULDY") | str_detect(x, "Bank") |
    str_detect(toupper(x),"PUT ASIDE") | str_detect(toupper(x),"ERROR") |
    str_detect(toupper(x),"DESTROYED") | str_detect(toupper(x),"RUSTY") |
    str_detect(toupper(x),"MANUAL") | str_detect(toupper(x),"AMAZON") |
    str_detect(toupper(x),"POSTAGE") | str_detect(toupper(x),"PADS")
}
data_tanpabuzzword = data_tanpanegatif[which(!is_Buzzword(as.character(data_tanpanegatif$Description))),]

#format waktu
Time = format(as.POSIXct(strptime(data_tanpabuzzword$Invoice,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M:%S")
data_tanpabuzzword$InvoiceDate = as.Date(data_tanpabuzzword$InvoiceDate)
dataclean <- data_tanpabuzzword

#sampling
datasample <- dataclean[sample(nrow(dataclean), 10000), ]

#outlier
library(OutlierDetection)
outlier <- nn(datasample, k=3)$`Location of Outlier`
data_nooutlier<-datasample[-c(outlier),]

#################################
# Create customer-level dataset #
#################################

customers <- as.data.frame(unique(data_nooutlier$`Customer ID`))
names(customers) <- "CustomerID"
customers$CustomerID <-customers[order(customers$CustomerID),]

#R - RECENCY
data_nooutlier$recency <- as.Date("2011-12-10") - as.Date(data_nooutlier$InvoiceDate)
recency <- aggregate(recency ~ `Customer ID`,
                     data=data_nooutlier, FUN=min, na.rm=TRUE)
customers <- merge(customers, recency,
                   by.x="CustomerID",by.y="Customer ID", all=TRUE, sort=TRUE)
remove(recency)
customers$recency <- as.numeric(customers$recency)

#F - FREQUENCY
frequency <- ddply(data_nooutlier, .(data_nooutlier$`Customer ID`), nrow)
names(frequency) <- c("CustomerID", "frequency")
frequency <- frequency[order(-frequency$CustomerID),]
customers <- merge(customers, frequency,
                   by.x="CustomerID",by.y="CustomerID", all=TRUE, sort=TRUE)
remove(frequency)

#M - Monetary
dataclean['Ammount'] = dataclean['Price']*dataclean['Quantity']
monetary <- ddply(data_nooutlier,.(`Customer ID`),summarise,monetary= sum(Ammount))
monetary <- monetary[order(-monetary$`Customer ID`),]
customers <- merge(customers, monetary,
                   by.x="CustomerID",by.y="Customer ID", all=TRUE, sort=TRUE)
remove(monetary)

#outlier
library(OutlierDetection)
coutlier <- nn(customers, k=3)$`Location of Outlier`
customers<-customers[-c(coutlier),]

#clustering
customerscluster <- customers[2:4]
custscale <- as.data.frame(scale(customerscluster ))


#K-means

  #k=4
  kmeans.result4<- kmeans(custscale,center=4, nstart = 25)
  kmeans.result4$cluster
  customers$kmeans <- kmeans.result4$cluster
  #visualisasi hasil
  plot(customers[c("recency", "frequency","monetary")], 
       col = kmeans.result4$cluster, main ="Hasil Clustering K-Means(K=4)")
  #analisis karakteristik kmeans
  library(ggplot2)
  plot(c(0), xaxt = 'n', ylab = "", type = "l", main="Karakteristik Hasil Cluster K-Means(K=4)",
       ylim = c(min(kmeans.result4$centers), max(kmeans.result4$centers)), xlim = c(0, 4))
  axis(1, at = c(1:3), labels = names(custscale))
  for (i in c(1:4))
    lines(kmeans.result4$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3,5),"black", "blue"))
  text(x = 0.5, y = kmeans.result4$centers[, 1], labels = paste("Cluster", c(1:3)))
  
  #k=3
  kmeans.result<- kmeans(custscale,center=3, nstart = 25)
  kmeans.result$cluster
  #visualisasi hasil
  plot(customers[c("recency", "frequency","monetary")], 
       col = kmeans.result$cluster, main ="Hasil Clustering K-Means(K=3)")
  #analisis karakteristik kmeans
  library(ggplot2)
  plot(c(0), xaxt = 'n', ylab = "", type = "l", main="Karakteristik Hasil Cluster K-Means(K=3)",
       ylim = c(min(kmeans.result$centers), max(kmeans.result$centers)), xlim = c(0, 4))
  axis(1, at = c(1:3), labels = names(custscale))
  for (i in c(1:3))
    lines(kmeans.result$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3,5),"black", "blue"))
  text(x = 0.5, y = kmeans.result$centers[, 1], labels = paste("Cluster", c(1:3)))
  
  #k=7
  kmeans.result7<- kmeans(custscale,center=7, nstart = 25)
  kmeans.result7$cluster
  #visualisasi hasil
  plot(customers[c("recency", "frequency","monetary")], 
       col = kmeans.result7$cluster, main ="Hasil Clustering K-Means(K=3)")
  #analisis karakteristik kmeans
  library(ggplot2)
  plot(c(0), xaxt = 'n', ylab = "", type = "l", main="Karakteristik Hasil Cluster K-Means(K=7)",
       ylim = c(min(kmeans.result7$centers), max(kmeans.result7$centers)), xlim = c(0, 4))
  axis(1, at = c(1:3), labels = names(custscale))
  for (i in c(1:7))
    lines(kmeans.result7$centers[i,], lty = i, lwd = 2, col = ifelse(i %in% c(1,3,5),"black", "blue"))
  text(x = 0.5, y = kmeans.result$centers[, 1], labels = paste("Cluster", c(1:3)))
  
  #Aglo Clustering
  library(cluster)
  library(factoextra)
  
  #method can be 'average', 'single', 'complete', 'ward'
  # Agglomerative Nesting (Hierarchical Clustering)
  
  #AG - average
  datacluster.agnes.avg <- agnes(x=custscale, # data matrix
                             stand = TRUE, # standarize the data
                             metric= "euclidian", # metric for distance matrix
                             method="average" # Linkage method
  )
  
  fviz_dend(datacluster.agnes.avg, cex=0.6, k=4)
  
  clust.a.avg<- cutree(datacluster.agnes, k=4)
  customers$Aavg <- clust.a.avg
  plot(customers[c("recency", "frequency","monetary")], 
       col = customers$Aavg, main ="Hasil Clustering Agg - Average")
  fviz_cluster(list(data = custscale, cluster=clust.a.avg))
  
  #AG - single
  datacluster.agnes.sgl <- agnes(x=custscale, # data matrix
                             stand = TRUE, # standarize the data
                             metric= "euclidian", # metric for distance matrix
                             method="single" # Linkage method
  )
  
  fviz_dend(datacluster.agnes.sgl, cex=0.6, k=4)
  clust.a.sgl<- cutree(datacluster.agnes.sgl, k=4)
  customers$Asgl <- clust.a.sgl
  plot(customers[c("recency", "frequency","monetary")], 
       col = customers$Asgl, main ="Hasil Clustering Agg - Single")
  fviz_cluster(list(data = custscale, cluster=clust.a.sgl))
  
  #AG - ward
  datacluster.agnes.wrd <- agnes(x=custscale, # data matrix
                                 stand = TRUE, # standarize the data
                                 metric= "euclidian", # metric for distance matrix
                                 method="ward" # Linkage method
  )
  
  fviz_dend(datacluster.agnes.wrd, cex=0.6, k=4)
  clust.a.wrd<- cutree(datacluster.agnes.wrd, k=4)
  customers$Awrd <- clust.a.wrd
  plot(customers[c("recency", "frequency","monetary")], 
       col = customers$Awrd, main ="Hasil Clustering Agg - Ward")
  fviz_cluster(list(data = custscale, cluster=clust.a.wrd))
  
  #AG - complete
  datacluster.agnes.cmp <- agnes(x=custscale, # data matrix
                                 stand = TRUE, # standarize the data
                                 metric= "euclidian", # metric for distance matrix
                                 method="complete" # Linkage method
  )
  
  fviz_dend(datacluster.agnes.cmp, cex=0.6, k=4)
  clust.a.cmp<- cutree(datacluster.agnes.cmp, k=4)
  customers$Acmp <- clust.a.cmp
  plot(customers[c("recency", "frequency","monetary")], 
       col = customers$Acmp, main ="Hasil Clustering Agg - Complete")
  fviz_cluster(list(data = custscale, cluster=clust.a.cmp))
  
  #Divisive Clustering 
  datacluster.diana <- diana(x=custscale, # data matrix
                             stand = TRUE, # standarize the data
                             metric= "euclidian" # metric for distance matrix
  )
  
  fviz_dend(datacluster.diana, cex=0.6, k=4)
  clust.dvs<- cutree(datacluster.diana, k=4)
  customers$divisive <- clust.dvs
  plot(customers[c("recency", "frequency","monetary")], 
       col = customers$divisive, main ="Hasil Clustering Divisive")
  fviz_cluster(list(data = custscale, cluster=clustdvs))
  
  
  #Density Based
  kNNdistplot(custscale, k=4)
  library(fpc)
  datacluster.ds <- fpc::dbscan(custscale, eps=0.3, MinPts=30)
  customers$DBSCAN <- datacluster.ds$cluster
  summary(datacluster.ds$cluster)
  #Plot Cluster
  plot(datacluster.ds, custscale)
  
  
#Bikin Ammount Column  

  #Evaluation
  
  set.seed(123)
  k.max <- 15
  wss <- sapply(1:k.max,
                function(k){kmeans(custscale, k, nstart=50,iter.max = 15 )$tot.withinss})
  wss
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE,
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares",
       main="WSS")
  
  WSS <- sapply(1:10, function(k){
    kmeans(custscale, centers = 3, nstart = 25)$tot.withinss
  })
  plot(1:10, WSS, type="b", xlab="Number of cluster(k)",
       ylab="Within grops sum of squares")
  
  fviz_nbclust(custscale, FUN = kmeans, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
  
  fviz_nbclust(custscale, FUN = hcut, method = "wss") +
    geom_vline(xintercept = 4, linetype = 2)+
    labs(subtitle = "Elbow method")
  
  library(cluster)
  km.sil <- silhouette(kmeans.result$cluster, dist(custscale))
  head(km.sil[,1:3],10)
  
  #plot
  plot(km.sil, main="silhout plot-kmeans")
  #plot using factoextra
  fviz_silhouette(km.sil)
  customers2 <- customers
  customers$DBSCAN[customers$DBSCAN == 0] <- 1
  customers3 <- customers2
  customers2$DBSCAN[customers2$DBSCAN == 5] <- 6
  customers2$DBSCAN[customers2$DBSCAN == 4] <- 5
  customers2$DBSCAN[customers2$DBSCAN == 3] <- 4
  customers2$DBSCAN[customers2$DBSCAN == 2] <- 3
  customers2$DBSCAN[customers2$DBSCAN == 1] <- 2
  customers2$DBSCAN[customers2$DBSCAN == 0] <- 1
  
  customer2.dist <- dist(custscale)
  library(seriation)
  pimage(customer2.dist)
  pimage(customer2.dist, order=order(customers2$DBSCAN))
  dissplot(customer2.dist, labels=(customers2$DBSCAN), 
           options = list(main = "DBSCAN"))
  
  names(customers) <- c("Customer","recency","frequency","monetary","kmeans","Aavg","Asgl","Awrd","Acmp","divisive","DBSCAN")
  
  #karakteristik aglomerative
  customersHR <- as.data.frame(unique(customers$Awrd))
  names(customersHR) <- "ClusterHR"
  
  #R - RECENCY
  recency.mean <- aggregate(recency ~ Awrd,
                       data=customers, FUN=mean, na.rm=TRUE)
  customersHR <- merge(customersHR, recency.mean,
                     by.x="ClusterHR",by.y="Awrd", all=TRUE, sort=TRUE)
  remove(recency.mean)
  customersHR$recency <- as.numeric(customersHR$recency)
  
  #F - FREQUENCY
  frequency.mean <- aggregate(frequency ~ Awrd,
                            data=customers, FUN=mean, na.rm=TRUE)
  customersHR <- merge(customersHR, frequency.mean,
                       by.x="ClusterHR",by.y="Awrd", all=TRUE, sort=TRUE)
  remove(frequency.mean)
  customersHR$frequency <- as.numeric(customersHR$frequency)
  
  #M - Monetary
  monetary.mean <- aggregate(monetary ~ Awrd,
                              data=customers, FUN=mean, na.rm=TRUE)
  customersHR <- merge(customersHR, monetary.mean,
                       by.x="ClusterHR",by.y="Awrd", all=TRUE, sort=TRUE)
  remove(monetary.mean)
  customersHR$monetary <- as.numeric(customersHR$monetary)
  
  customersHR1 <- customersHR[,1]
  CustomersHR2 <- customersHR[,-1]
  customersHR2 <- scale(CustomersHR2)
  
  ggplot(customersHR, aes(x=reorder(Negara,-Transaksi), y=Transaksi)) +
    geom_bar(stat="identity", fill="#009999") + theme_bw() +
    xlab("Negara") +
    ylab("Jumlah Transaksi") +
    ggtitle("TOP 10 Jumlah Transaksi Per Negara")

  #karakteristik KMEANS
  customersKM <- as.data.frame(unique(customers$kmeans))
  names(customersKM) <- "ClusterKM"
  
  #R - RECENCY
  recency.mean <- aggregate(recency ~ kmeans,
                            data=customers, FUN=mean, na.rm=TRUE)
  customersKM <- merge(customersKM, recency.mean,
                       by.x="ClusterKM",by.y="kmeans", all=TRUE, sort=TRUE)
  remove(recency.mean)
  customersKM$recency <- as.numeric(customersKM$recency)
  
  #F - FREQUENCY
  frequency.mean <- aggregate(frequency ~ kmeans,
                              data=customers, FUN=mean, na.rm=TRUE)
  customersKM <- merge(customersKM, frequency.mean,
                       by.x="ClusterKM",by.y="kmeans", all=TRUE, sort=TRUE)
  remove(frequency.mean)
  customersKM$frequency <- as.numeric(customersKM$frequency)
  
  #M - Monetary
  monetary.mean <- aggregate(monetary ~ kmeans,
                             data=customers, FUN=mean, na.rm=TRUE)
  customersKM <- merge(customersKM, monetary.mean,
                       by.x="ClusterKM",by.y="kmeans", all=TRUE, sort=TRUE)
  remove(monetary.mean)
  customersKM$monetary <- as.numeric(customersKM$monetary)
  
  customersKM1 <- customersKM[,1]
  CustomersKM2 <- customersKM[,-1]
  customersKM2 <- scale(CustomersKM2)
  
  #karakteristik DBSCAN
  customersDB <- as.data.frame(unique(customers$DBSCAN))
  names(customersDB) <- "ClusterDB"
  
  #R - RECENCY
  recency.mean <- aggregate(recency ~ DBSCAN,
                            data=customers, FUN=mean, na.rm=TRUE)
  customersDB <- merge(customersDB, recency.mean,
                       by.x="ClusterDB",by.y="DBSCAN", all=TRUE, sort=TRUE)
  remove(recency.mean)
  customersDB$recency <- as.numeric(customersDB$recency)
  
  #F - FREQUENCY
  frequency.mean <- aggregate(frequency ~ DBSCAN,
                              data=customers, FUN=mean, na.rm=TRUE)
  customersDB <- merge(customersDB, frequency.mean,
                       by.x="ClusterDB",by.y="DBSCAN", all=TRUE, sort=TRUE)
  remove(frequency.mean)
  customersDB$frequency <- as.numeric(customersDB$frequency)
  
  #M - Monetary
  monetary.mean <- aggregate(monetary ~ DBSCAN,
                             data=customers, FUN=mean, na.rm=TRUE)
  customersDB <- merge(customersDB, monetary.mean,
                       by.x="ClusterDB",by.y="DBSCAN", all=TRUE, sort=TRUE)
  remove(monetary.mean)
  customersDB$monetary <- as.numeric(customersDB$monetary)
  
  customersDB1 <- customersDB[,1]
  CustomersDB2 <- customersDB[,-1]
  customersDB2 <- scale(CustomersDB2)
  
  
#date
#print('Min Invoice Date:',df.InvoiceDate.dt.date.min(),'max Invoice Date:',
#      df.InvoiceDate.dt.date.max())

#df.head(3)