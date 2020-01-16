library(rvest)
library(tidyverse)

URL<-"http://programme-tv.nouvelobs.com/programme-tv/2019-01-01/"
page<-read_html(URL)
GRILLE<-page%>%html_nodes("#contenu")%>%html_nodes(".tab_grille")
Titres<-GRILLE%>%
  html_nodes(".prog")%>%
  html_nodes(".b_d.prog1")%>%html_nodes(".titre")%>%html_text()
Chaines<-page%>%html_nodes("#contenu")%>%html_nodes(".logo_chaine_g")%>%html_nodes(xpath = "./a") %>% html_attr("href")
Longueur<-length(Chaines)
rep(Chaines,each=2)

PrimeTime<-data.frame("Chaines"=rep(Chaines[1:20],each=2),
                      "Programme"=Titres[1:40],
                      "Partie"=rep(1:2,20))
PrimeTime$Date<-"2019-01-01"
dates<-seq(from=as.Date("2019-01-02"), to=as.Date("2019-12-04"),by=1)
for (i in as.character(dates)){
  print(as.character(i))
  URL<-paste0("http://programme-tv.nouvelobs.com/programme-tv/",as.character(i),"/")
  page<-read_html(URL)
  GRILLE<-page%>%html_nodes("#contenu")%>%html_nodes(".tab_grille")
  Titres<-GRILLE%>%
    html_nodes(".prog")%>%
    html_nodes(".b_d.prog1")%>%html_nodes(".titre")%>%html_text()
  Chaines<-page%>%html_nodes("#contenu")%>%html_nodes(".logo_chaine_g")%>%html_nodes(xpath = "./a") %>% html_attr("href")
  Longueur<-length(Chaines)
  tmp<-data.frame("Chaines"=rep(Chaines[1:20],each=2),
                  "Programme"=Titres[1:40],
                  "Partie"=rep(1:2,20))
  tmp$Date<-as.character(i)
  PrimeTime<-rbind(PrimeTime,tmp)
  Sys.sleep(1)
}
 saveRDS(PrimeTime, "PrimeTime.rdata")
