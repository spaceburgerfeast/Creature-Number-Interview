CANLILARI_TANIMLA<- function()
{
#Random olarak canli koordinatlarini tanimliyoruz
#koyunlar
koyun_erkek <<- data.frame(x=sample(1:500,15),y=sample(1:500,15))
koyun_disi <<- data.frame(x=sample(1:500,15),y=sample(1:500,15))

#inekler
inek_erkek <<- data.frame(x=sample(1:500,5),y=sample(1:500,5))
inek_disi <<- data.frame(x=sample(1:500,5),y=sample(1:500,5))

#kus: kus_erkek = horoz .... kus_disi = tavuk
kus_erkek <<- data.frame(x=sample(1:500,10),y=sample(1:500,10))
kus_disi <<- data.frame(x=sample(1:500,10),y=sample(1:500,10))

#kurtlar
kurt_erkek <<- data.frame(x=sample(1:500,5),y=sample(1:500,5))
kurt_disi <<- data.frame(x=sample(1:500,5),y=sample(1:500,5))

#aslanlar
aslan_erkek <<- data.frame(x=sample(1:500,4),y=sample(1:500,4))
aslan_disi <<- data.frame(x=sample(1:500,4),y=sample(1:500,4))

#avci
avci <<- data.frame(x=sample(1:500,1),y=sample(1:500,1))

#Canlilarin isimlerini, ileride kodu rahatlatmasi icin string vektörü icine aliyoruz 

canlilar <<- c("koyun_erkek","koyun_disi","inek_erkek","inek_disi","kus_erkek",
"kus_disi","kurt_erkek","kurt_disi","aslan_erkek","aslan_disi","avci")

#Kurt ve aslanlari erkek ve disi olarak ayirmistik ama ileride kodlama rahatligi
#icin birlestirerek tek tablo haline getiriyoruz

kurtlar <<- rbind(kurt_erkek,kurt_disi)
aslanlar <<- rbind(aslan_erkek,aslan_disi)

#Toplam adim sayisini global olarak tanimliyoruz. Cünkü 1000 adim atildiginda
#programi durduracagiz.

atilan_toplam_adim <<- 0
}



#-----------------AVLANMA fONKSIYONU--------------------------

AVLANMA <- function(){
#Canlilar vektöründen avciyi çikararak yeni bir degisken olusturuyoruz.

avcinin_avladiklari <- canlilar[-11] 
#Asagida kurdugum algoritma sayesinde avcinin 8 birim mesafesinde bir canli
#var mi diye bakiyoruz.Var ise o canlinin koordinatlarini daha önce olusturdu
#gumuz tablolardan kaldiriyoruz.

for(i in 1:length(avcinin_avladiklari)){

temp <- get(avcinin_avladiklari[i])
index <- c(0)

for(j in 1:nrow(temp)){
apsis <- avci[1,1]-temp[j,1]
ordinat <- avci[1,2]-temp[j,2]
mesafe <- sqrt((apsis^2)+(ordinat^2))

if(mesafe <= 8){
print(mesafe)
print(avcinin_avladiklari[i])
print(j)
index <- c(index,c(j))
}

}
if(length(index)>1){
index <- index[-1]
temp <- temp[-index,]
rownames(temp)<- 1:length(rownames(temp))
assign(avcinin_avladiklari[i],temp,envir = .GlobalEnv)
}

}



kurdun_avladiklari <- canlilar[c(1,2,5,6)]

#Asagida kurdugum algoritma sayesinde kurdun 4 birim mesafesinde bir canli
#var mi diye bakiyoruz.Var ise o canlinin koordinatlarini daha önce olusturdu
#gumuz tablolardan kaldiriyoruz.

for(i in 1:length(kurdun_avladiklari)){
for(k in 1:nrow(kurtlar)){

temp <- get(kurdun_avladiklari[i])
index <- c(0)

for(j in 1:nrow(temp)){

apsis <- kurtlar[k,1]-temp[j,1]
ordinat <- kurtlar[k,2]-temp[j,2]
mesafe <- sqrt((apsis^2)+(ordinat^2))

if(mesafe <= 4){
print(mesafe)
print(kurdun_avladiklari[i])
print(j)
index <- c(index,c(j))
}
}
if(length(index)>1){
index <- index[-1]
temp <- temp[-index,]
rownames(temp)<- 1:length(rownames(temp))
assign(kurdun_avladiklari[i],temp,envir = .GlobalEnv)
}
}


}



aslanin_avladiklari <- canlilar[1:4]

#Asagida kurdugum algoritma sayesinde aslanin 5 birim mesafesinde bir canli
#var mi diye bakiyoruz.Var ise o canlinin koordinatlarini daha önce olusturdu
#gumuz tablolardan kaldiriyoruz.

for(i in 1:length(aslanin_avladiklari)){

temp <- get(aslanin_avladiklari[i])
index <- c(0)

for(k in 1:nrow(aslanlar)){
for(j in 1:nrow(temp)){

apsis <- aslanlar[k,1]-temp[j,1]
ordinat <- aslanlar[k,2]-temp[j,2]
mesafe <- sqrt((apsis^2)+(ordinat^2))

if(mesafe <= 5){
print(mesafe)
print(aslanin_avladiklari[i])
print(j)
index <- c(index,c(j))
}
}
if(length(index)>1){
index <- index[-1]
temp <- temp[-index,]
rownames(temp)<- 1:length(rownames(temp))
assign(aslanin_avladiklari[i],temp,envir = .GlobalEnv)
}

}
}

#fonksiyon bitimi
}
#------------------AVLANMA FONKSIYONU SONU------------------------





#----------------Toplam canli sayisini hesaplama-----------
CANLI_SAYISI<- function(){
canli_toplam <-0
for(k in 1:length(canlilar))
{
canli_toplam <- canli_toplam + nrow(get(canlilar[k]))

}
print(canli_toplam)
}
#-----------------------------------------------------------






#---------------HAREKET ETTIRME FONKSIYONU--------------------------
#Ilk olarak tüm canlilar icinden random olarak bir canli seciyoruz. Bu canlinin
#hareketi icin kac adim atmasi gerektigini belirliyoruz. Bundan sonra her bir
#adim icin random olarak x veya y koordinatini seciyoruz. Sonra da x yada y koor-
#dinatini random olarak +1 yada -1 degistiriyoruz. Bir yandan da toplam adim
#sayisini kayit ediyoruz.

HAREKET_ETTIR <- function(){
z <- sample(1:11,1)
temp <- get(canlilar[z])
j <- sample(1:nrow(temp),1)

adim <- 0
if(canlilar[z] %in% c("avci","kus_erkek","kus_disi")){adim <- 1}
if(canlilar[z] %in% c("koyun_erkek","koyun_disi","inek_disi","inek_erkek")){adim <- 2}
if(canlilar[z] %in% c("kurt_erkek","kurt_disi")){adim <- 3}
if(canlilar[z] %in% c("aslan_erkek","aslan_disi")){adim <- 4}

for(c in 1:adim){

x_yada_y <- sample(c(1,2),1)
a <- temp[j,x_yada_y]
a <- a+sample(c(1,-1),1)

while(a<0 & a>500){
x_yada_y <- sample(c(1,2),1)
a <- temp[j,x_yada_y]
a <- a+sample(c(1,-1),1)

}
temp[j,x_yada_y] <- a
assign(canlilar[z],temp,envir = .GlobalEnv)
atilan_toplam_adim <<- atilan_toplam_adim +1
print(atilan_toplam_adim)
if(atilan_toplam_adim==1000){break}
}
}
#------------------------------------------------------------------------


#-------------------DOGUM FONKSIYONU-------------------------
DOGUM<- function(){
#Analitik geometriden bildigimiz iki nokta arasi mesafeyi hesaplama yönteminden
#yararlanarak birbirine 3 birim veya daha yakin iki adet ayni cins canli olup
#olmadigina bakiyoruz. Varsa yukarýdaki tablolardan koordinat bilgilerini kaldiriyoruz.

for(i in 1:5){
erkek <- 2*i-1
disi <- 2*i
temp_erkek <- get(canlilar[erkek])
temp_disi <- get(canlilar[disi])

for(k in 1:nrow(temp_erkek)){
for(j in 1:nrow(temp_disi)){

apsis <- temp_erkek[k,1]-temp_disi[j,1]
ordinat <- temp_erkek[k,2]-temp_disi[j,2]
mesafe <- sqrt((apsis^2)+(ordinat^2))
if(mesafe <= 3){
ind <- sample(c(erkek,disi),1)
temp_erkek_disi <- get(canlilar[ind])
temp_erkek_disi <- rbind(temp_erkek_disi,data.frame(x=sample(1:500,1),y=sample(1:500,1)))
assign(canlilar[ind],temp_erkek_disi,envir = .GlobalEnv)
print(canlilar[ind])
}
}
}
}
}
#---------------------------------------------------------------------

CANLILARI_TANIMLA()
CANLI_SAYISI()
for(ss in 1:5){
AVLANMA()
DOGUM()
HAREKET_ETTIR()
}
CANLI_SAYISI()
#-----------------------ALTERNATIF HAREKET FONKSIYONU------------------
#------------------------------------------------------------------
#-------------------------------------------------------------------
#HAREKET_ETTIR <- function(){
#for(z in 1:length(canlilar)){
#temp <- get(canlilar[z])

#adim <- 0
#if(canlilar[z] %in% c("avci","kus_erkek","kus_disi")){adim <- 1}
#if(canlilar[z] %in% c("koyun_erkek","koyun_disi","inek_disi","inek_erkek")){adim <- 2}
#if(canlilar[z] %in% c("kurt_erkek","kurt_disi")){adim <- 3}
#if(canlilar[z] %in% c("aslan_erkek","aslan_disi")){adim <- 4}


#for(j in 1:nrow(temp)){

#for(c in 1:adim){

#x_yada_y <- sample(c(1,2),1)
#a <- temp[j,x_yada_y]
#a <- a+sample(c(1,-1),1)

#while(a<0 & a>500){
#x_yada_y <- sample(c(1,2),1)
#a <- temp[j,x_yada_y]
#a <- a+sample(c(1,-1),1)

#}
#temp[j,x_yada_y] <- a
#}
#}
#assign(canlilar[z],temp,envir = .GlobalEnv)
#}
#}

