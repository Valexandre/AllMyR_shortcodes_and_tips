install.packages("tidyverse")

library(tidyverse)
library(xml2)

addDpt<-function(x) {
  name<-paste("https://www.interieur.gouv.fr/avotreservice/elections/telechargements/MN2014/candidatureT1/",x,"/C1",x,".xml",sep="")
  c.nodes<-read_xml(name) %>%
    xml_find_all("//Candidat|//CandidatMaj")
  data.frame(
    code_dpt = c.nodes %>%
      xml_find_first(".//ancestor::Departement/CodMinDpt") %>%
      xml_text(),
    code_com = c.nodes %>%
      xml_find_first(".//ancestor::Commune/CodSubCom") %>%
      xml_text(),
    nom_com = c.nodes %>%
      xml_find_first(".//ancestor::Commune/LibSubCom") %>%
      xml_text(),
    pop_com = c.nodes %>%
      xml_find_first(".//ancestor::Commune/PopSubCom") %>%
      xml_text(),
    mode = c.nodes %>%
      xml_find_first(".//ancestor::Commune/ModeScrutin") %>%
      xml_text(),
    no_liste = c.nodes %>%
      xml_find_first(".//ancestor::Liste/NumListe") %>%
      xml_text(),
    nuance = c.nodes %>%
      xml_find_first(".//ancestor::Liste/CodNuaListe") %>%
      xml_text(),
    nom_liste = c.nodes %>%
      xml_find_first(".//ancestor::Liste/NomListe") %>%
      xml_text(),
    prenom_candidat = c.nodes %>%
      xml_find_first("PrePsn") %>%
      xml_text(),
    nom_candidat = c.nodes %>%
      xml_find_first("NomPsn") %>%
      xml_text(),
    sexe_candidat = c.nodes %>%
      xml_find_first("CivilitePsn") %>%
      xml_text(),
    tete_liste =  c.nodes %>%
      xml_find_first("TeteListe") %>%
      xml_text()
  )
}

candidats<-map_df(c(lapply(1:19,function(x) stringr::str_pad(x, 3, pad = "0")),"02A","02B",lapply(21:95,function(x) stringr::str_pad(x, 3, pad = "0")),lapply(971:976,paste),"987","988"),addDpt)

### Petite commune : moins de 3500

candidats %>%
  filter(!(code_dpt %in% c("987","988"))) %>%
  mutate(pop_com = as.numeric(pop_com)) %>%
  mutate(seuil = cut(pop_com,breaks=c("0","500","1000","3500",range(pop_com)[2]),labels = c("0-500","500-1000","1000-3500","plus de 3500"),right=FALSE,include.lowest=TRUE)) %>%
  filter(mode == "Liste") %>%
  group_by(seuil,nom_com) %>%
  summarise(liste_max = max(no_liste)) %>%
  arrange(desc(liste_max)) %>%
  group_by(seuil,liste_max) %>%
  tally() %>%
  ggplot(aes(y=n,x=seuil,fill=liste_max)) +geom_bar(stat="identity")+ geom_text(aes(x=seuil,label=n,y=n),position = position_stack(vjust = 0.5))


candidats %>%
  mutate(code_insee = ifelse(code_dpt %in% c("971","972","973","974","976"),paste("97",code_com,sep = ""),paste(code_dpt,code_com,sep = ""))) %>%
  mutate(pop_com = as.numeric(pop_com)) %>%
  mutate(seuil = cut(pop_com,breaks=c("0","500","1000","3500",range(pop_com)[2]),labels = c("0-500","500-1000","1000-3500","plus de 3500"),right=FALSE,include.lowest=TRUE)) %>%
  group_by(code_insee,sexe_candidat,seuil) %>%
  tally() %>%
  pivot_wider(names_from = sexe_candidat,values_from = n) %>%
  mutate(taux = `Mme`/(`M.`+`Mme`)) %>%
  group_by(seuil) %>%
  summarise(moyenne = mean(taux,na.rm = TRUE)) %>%
  ggplot(aes(y=moyenne,x=seuil)) +geom_bar(position = "dodge",stat="identity")+ geom_text(aes(x=seuil,label=paste0(floor(moyenne*1000)/10,"%"),y=moyenne),position=position_dodge(0.9),vjust=-0.5)


candidats %>%
  filter(tete_liste == "O") %>%
  group_by(sexe_candidat) %>%
  tally
