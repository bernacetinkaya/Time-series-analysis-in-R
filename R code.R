
#Veri Okutma
data <- read.csv("USvideos_modified_1.csv", header = TRUE, sep = ";")
data

#Kütüphaneleri Çağırma
library(ggplot2)
library(dplyr)



#Data Manipulation
summary(data)
dim(data)
str(data)
head(data)
tail(data)
sapply(data, class)  # Sütunların veri türlerini gösterir
sapply(data, function(x) sum(is.na(x)))  # Sütunlardaki eksik değer sayısını gösterir
lapply(data, mean)
data <- data[order(data$subscriber, decreasing = TRUE), ]
head(data)


#Converting class
data$views <- as.numeric(data$views)
data$comment_count <- as.numeric(data$comment_count)
data$publish_hour <- factor(data$publish_hour)
data$category_id <- factor(data$category_id)
data$publish_date <- factor(data$publish_date)
data$channel_title <- factor(data$channel_title)
data$title <- factor(data$title)
data$last_trending_date <- factor(data$last_trending_date)

#Genel Görselleştirmeler

#1) ABD saat diliminde Youtube'da trend olan bir video yayınlamak için en yoğun saati 14:00 - 18:00 arasında görülebilir. 
#   Ancak, yalnızca trend olan videolar değil, tüm videolar bu saatte yayınlanıyor olabilir

ggplot(aes(x = publish_hour), data = data) +
  geom_bar(fill = "steelblue") +
  labs(x = "Publish Hour", y = "Count") +
  ggtitle("Number of Published Videos by Hour of the Day")

#2) Category_id=24 Youtube trend videolarının en çok  yayınlandığı kategoridir.

ggplot(aes(x = category_id), data = data) +
  geom_bar(fill = "purple") +
  labs(x = "Category ID", y = "Count") +
  ggtitle("Number of Videos by Category")

table(data$category_id)

#3) Bu grafikte 1 milyondan düşük abone sayısına sahip kanalların daha fazla çoğunlukta olduğunu görüyoruz.

data %>%
  filter(!is.na(subscriber)) %>%
  arrange(subscriber) %>%
  mutate(subscriber_millions = subscriber / 1000000) %>%
  ggplot(aes(x = subscriber_millions, fill = ..count..)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 1000000000, 5000000)) +
  ggtitle("Distribution of Subscribers") +
  labs(x = "Subscribers (Millions)", y = "Count", caption = "Bin width = 1000000") +
  
  scale_fill_gradient(low = "blue", high = "red")

#3.1) Ayrıca abone bilgilerinin NA olarak ayarlandığı 6 trend video var.
summary(data$subscriber)


#4) Videolarda kullanılan etiket sayısının dağılımını bu grafikte görüyoruz.
ggplot(aes(x = tags_count), data = data) +
  geom_histogram(binwidth = 1, fill = "red") +
  labs(x = "Tags Count", y = "Count") +
  ggtitle("Histogram of Tags Count")

#4.1) 35 adet etiket kullanılmayan video olduğunu görüyoruz. Ayrıca ortalama 17.98 etiket kullanılmış.
table(data$tags_count)[which.max(table(data$tags_count))]
mean(data$tags_count)

#5) Çoğu video için başlıkta 1 ila 4 arasında etiket kullanıldığıgörülmektedir. 
ggplot(aes(x = tag_appeared_in_title_count), data = data) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  scale_x_continuous(breaks = seq(0, 18, 1)) +
  ggtitle("Distribution of Tag Appearances in Title") +
  labs(x = "Tag Appearances in Title", y = "Count")

#5.1)  Maximum 14 etiket kullanılmıştır. Ayrıca bir videoda ortalama yaklaşık olarak 3 adet etiket kullanılmaktadır.
summary(data$tag_appeared_in_title_count)

#5.2) 744 trend videodan 111 video başlığı, video başlığında etiketini (veya anahtar kelimesini) kullanmamıştı.
summary(data$tag_appeared_in_title)

 
#6) Videoların trendde kaldıkları gün süresini gösteren histogram grafiğini görebiliriz. 
ggplot(aes(x = trend_day_count), data = data) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  ggtitle("Distribution of Trend Day Count") +
  labs(x = "Trend Day Count", y = "Count")
    
#6.1) 55 günlük gözlemden, belirli bir videonun en fazla 10 gün boyunca trend olduğu görülebilir.
#Ayırca Youtube trend videolar listesinde sadece bir gün kalan 130 video olduğunu görüyoruz.
summary(data$trend_day_count)
table(data$trend_day_count)


#7) Burada ilk başta uç değerler var ancak dağılımın genel olarak normal dağılıma uyduğunu söyleyebiliriz.
ggplot(aes(x = comment_count + 1), data = data) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  scale_x_log10() +
  ggtitle("Comment Count Distribution (Log Scale)") +
  labs(x = "Comment Count", y = "Count") +
  theme_minimal()



#8) Comments disabled data$comments_disabled sütununda 744 gözlem bulunmaktadır. Burada yorumlara kapatıldığı sadece 13 video vardır.

summary(data$comments_disabled=='True')



#Hipotez Testleri

#1. Hipotez: Yüksek beğeni sayısına sahip videolar, daha fazla görüntülenme sayısına sahip olma eğilimindedir.

data %>%
  filter(likes > 0, views > 0) %>%
  mutate(likes_plus_1 = likes + 1) %>%
  group_by(likes_plus_1) %>%
  summarise(avg_views = mean(views)) %>%
  arrange(likes_plus_1) %>%
  ggplot(aes(x = likes_plus_1, y = avg_views)) +
  geom_point(color = "blue", alpha = 0.5) +  #Alpha değeri şeffaflığı belirliyor.
  labs(x = "Likes + 1", y = "Average Views") +
  ggtitle("Relationship between Likes and Average Views") +
  scale_x_log10() +
  scale_y_log10()


#2. Hipotez: Başlıkta etkikete sahip olan videolar, başlıkta etiket yer almayan videolara kıyasla daha yüksek görüntülenme sayısına sahip olma eğilimindedir.

# Başlıkta etikete sahip olan videoların görüntülenme sayılarını alıyoruz
tagged_views <- data %>%
  filter((tag_appeared_in_title)>0, views > 0) %>%
  pull(views)

# Başlıkta etiket olmayan videoların görüntülenme sayılarını alıyoruz
non_tagged_views <- data %>%
  filter((tag_appeared_in_title_count)==0, views > 0) %>%
  pull(views)

# Verileri birleştiriyoruz
df <- data.frame(
  Category = c(rep("Tagged Videos", length(tagged_views)), rep("Non-Tagged Videos", length(non_tagged_views))),
  Views = c(tagged_views, non_tagged_views))

# X ve Y değerlerinin logaritmasını alarak kutu grafiği oluşturuyoruz.
df$Log_Views <- log(df$Views)

ggplot(df, aes(x = Category, y = Log_Views)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Category", y = "Log Views") +
  ggtitle("Distribution of  Views for Tagged vs. Non-Tagged Videos")


#3. Hipotez: Abone sayısı daha yüksek olan kanalların videoları, daha yüksek beğeni alma eğilimindedir.

data %>%
  filter(subscriber > 0, likes > 0) %>%
  mutate(log_subscriber = log(subscriber), log_likes = log(likes)) %>%
  ggplot(aes(x = log_subscriber, y = log_likes)) +
  geom_point(color = "blue", alpha = 0.5) + 
  xlab("Subscriber ") +
  ylab("Likes ") +
  ggtitle("Likes by Subscriber Count (Log Scale)")


#4. Hipotez: Videolarda görüntülenme sayısı arttıkça yorum sayıları da artma eğilimindedir. 

ggplot(aes(y=views,x=comment_count+1),data=data)+geom_point(alpha=.1)+
  scale_x_log10(breaks=c(1,10,100,100000,1000000))+
  scale_y_log10(breaks=c(500,1000,10000,100000,1000000,10000000,100000000))+
  geom_smooth()+labs(caption = "scale_x_log10() & scale_y_log10()")

data$views <- as.numeric(data$views)
data$comment_count <- as.numeric(data$comment_count) # 'views' ve 'comment_count' sütunlarını numerik veri türüne dönüştürme

cor(data$views, data$comment_count) # Korelasyon katsayısını hesaplama


#5. Hipotez: Abone sayısı yüksek olan kanalların toplam dislike sayısı, abone sayısı az olan kanallara kıyasla daha fazladır.

data %>%
  filter(subscriber > 0) %>%
  group_by(subscriber) %>%
  summarise(total_dislikes = sum(dislikes)) %>%
  filter(total_dislikes > 0) %>%
  ggplot(aes(x = subscriber, y = total_dislikes)) +
  geom_point() +
  labs(x = "Subscriber Count", y = "Total Dislikes", title = "Total Dislikes by Subscriber Count") +
  scale_x_log10() +
  scale_y_log10()



#Corelasyon

#1) Grafikten görebildiğimiz gibi, görüşler ve beğeniler arasında çok güçlü bir ilişki var. Aralarındaki korelasyonun değeri 0.84'tür.

ggplot(aes(y=views,x=likes+1),data=data)+geom_jitter(alpha=0.25,size=3/4)+
  scale_x_log10(breaks=c(1,100,1000,10000,100000,1000000))+scale_y_log10()+
  xlab("likes")+labs(caption="scale_x_log10()+scale_y_log10()")+
  ggtitle("Views v/s Likes+1 with log10 transformation for both axes")+
  geom_smooth()

with(data,cor(views,likes))

#2) Olay örgüsü neredeyse izlenmeler ve beğeniler arsasına benzer görünüyor, ancak bu durumda, bazı yerler için beğenmemelerin varyansı daha büyük.
# Görüntülemeler ve beğenmemeler arasındaki korelasyon = 0.8832151.
ggplot(aes(y=views,x=likes+1),data=data)+
  geom_jitter(alpha=0.25,size=3/4)+scale_x_log10(
    breaks=c(1,100,1000,10000,100000))+scale_y_log10()+
  labs(caption="scale_x_log10()+scale_y_log10()")+
  ggtitle("Views v/s Likes+1 with log10 transformation for both axes")


#3) Doğrusal regresyon çizgisi, video başlıklarının ortalama uzunluğu arttıkça ortalama görüntüleme sayılarının biraz azaldığını gösteriyor.

ggplot(aes(y=views,x=nchar(as.character(title))),data=data)+
  geom_point(alpha=0.05)+scale_y_log10(breaks=c(
    1,100000,200000,300000,1000000))+geom_smooth()+
  labs(caption = "scale_y_log10()")+
  ggtitle("views v/s title length relation")+xlab("title length")

#4) Görüntülenme sayılarının ilk %95'i ve trend_day_count verileri için, ortalama trend_day_count arttığından, ortalama görüntüleme sayılarının hızla arttığını söyleyebiliriz.Görüntülenme ve trend_day_count arasındaki korelasyon=0.205823
ggplot(aes(y=views,x=trend_day_count),data=data)+
  geom_point(position ="jitter",alpha=.1)+coord_cartesian(xlim=c(0,quantile(
    data$trend_day_count,0.95)),ylim=c(0,quantile(data$views,0.95)))+
  geom_smooth()

with(data,cor(views,trend_day_count))



#5) Beğenme ve Beğenmeme, 0.78 korelasyon değeri ile güçlü bir ilişkiye sahiptir.
ggplot(aes(x=likes,y=dislikes),data=subset(data,likes!=0 & dislikes!=0))+
  geom_point(alpha=0.1)+scale_x_log10()+scale_y_log10()+
  geom_smooth(method="lm")+labs(caption="scale_x_log10()+scale_y_log10()")
with(data,cor(likes,dislikes,method = "pearson"))


#6)
ggplot(aes(y=views,x=comment_count+1),data=data)+geom_point(alpha=.1)+
  scale_x_log10(breaks=c(1,10,100,100000,1000000))+
  scale_y_log10(breaks=c(500,1000,10000,100000,1000000,10000000,100000000))+
  geom_smooth()+labs(caption = "scale_x_log10() & scale_y_log10()")

#6.1) rho, sıra korelasyonunun parametrik olmayan bir ölçüsüdür. Görüntülenme ve yorum_sayısı arasındaki rho da çok güçlüdür ve değeri 0.8472028 'dur.
with(data,cor.test(views,comment_count,method = "spearman"))

#B Part - Faktör Analizi
df <- read.csv('https://github.com/nchelaru/data-prep/raw/master/telco_cleaned_renamed.csv')
df

head(df)
str(df)
print(colnames(df))

suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))


res.famd <- FAMD(df, 
                 sup.var = 20,  ## Set the target variable "Churn" as a supplementary variable
                 graph = FALSE, 
                 ncp=25)

get_eigenvalue(res.famd)

library(PCAmixdata)

split <- splitmix(df[1:19])  


res.pcamix <- PCAmix(X.quanti=split$X.quanti,  
                     X.quali=split$X.quali, 
                     rename.level=TRUE, 
                     graph=FALSE, 
                     ndim=25)

res.pcamix$eig

options(repr.plot.width = 14, repr.plot.height = 12)

fviz_famd_ind(res.famd, label = "none", 
              habillage = "Churn", palette = c("#00AFBB", "#FC4E07"), # color by groups 
              repel = TRUE, alpha.ind = 0.5) + 
  xlim(-5, 6) + ylim (-4.5, 4.5) +
  theme(text = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20))

