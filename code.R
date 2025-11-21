#Ajouter les éléments pour la charte
 labs(title=str_wrap("Nombre de patients hospitalisés pour 100 000 habitants",50),
  caption=c("Source : CovidTracking.com","LP/DATA"))+
    theme_LP_1200()+theme(axis.text.x = element_blank(),
                          axis.text.y = element_blank(),
                         plot.caption = element_text(hjust=c(0, 1)))+ 
    inset_element(on_top = F,logo, 0.9, 0.9, 1, 1, align_to = 'full')

CreeMoiUnGradient <- colorRampPalette(c("#CC2828", "white"))
CouleursBornes<-rev(CreeMoiUnGradient(NbBornes)[1:5])


NombreCouleurs<-function(couldep,coularr,nombre){
  
  CreeMoiUnGradient <- colorRampPalette(c(couldep, coularr))
  CreeMoiUnGradient(nombre)
}


# Sélectionner plusieurs éléments

## Joindre des dataframes qui commencent par df
mget(ls(pattern="^df\\.\\d+")) %>%
              bind_rows()

## Mettre des valeurs disparates dans un df
tibble::enframe(mget(ls(pattern = "^Nombre")))

## Regrouper les csv en un seul df en créant une colonne avec le nom
 AppendMe <- function(dfNames) {
  do.call(rbind, lapply(dfNames, function(x) {
    cbind(get(x), departement = x)
  }))
}                
 TauxBruts<-AppendMe(c("D75_tb", "D77_tb","D78_tb",
                      "D91_tb", "D92_tb","D93_tb",
                      "D94_tb", "D95_tb","D60_tb","idf_tb","france_tb"))
             
## Sélectionner des colonnes dont le nom est dans une lsite.
desired_columns<-c("coucou","bonjour")
extract_columns <- function(data) {
    extracted_data <- data %>%
        select_(.dots = desired_columns)
    return(extracted_data)
}
extract_columns(jeudedonneesaveclescolonnescoucouetbonjouretautrechose)
extract_columns(df)

## Modifier une chaine de caractère peu import où elle est
DF%>%mutate_all(funs(str_replace(., "\\{DansDep\\}", DansDep)))

# Split et prendre le premier mot d'une variable
Donnees$Marque<-vapply(strsplit(Donnees$`Marque Modele`,"-"), `[`, 1, FUN.VALUE=character(1))
amendements$TitrePrenomNomTmp2<-sapply(strsplit(amendements$TitrePrenomNomTmp," et "), `[`, 1)

# Côté cartographie

## Extraire une geometry
aaa_coords <- do.call(rbind, st_geometry(aaa)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))

## Crop polygones selon coord
st_intersection(subdivisions, st_set_crs(st_as_sf(as(raster::extent(-20, 40,20, 70), "SpatialPolygons")), st_crs(subdivisions)))
               
## From geometry to lat long
st_x = function(x) st_coordinates(x)[,2]
st_y = function(x) st_coordinates(x)[,1]
SF%>%mutate(centre=st_centroid(geometry),
                            latitude=st_x(centre),
                            longitude=st_y(centre))

## Cropping on Mapshaper
mapshaper -clip bbox=1.4473,48.1205,3.5557,49.2374 
mapshaper -clip bbox=583350,6770000,744555,6957000

## Ne garder que l'exlusion entre deux polygones
CommunessansParcs<-st_difference(Communes,st_union(PasParis))

## Delaunay triangle
vtess=deldir(Geo_2018_centroids$longitude,Geo_2018_centroids$latitude)
tl = triang.list(vtess)
polys = SpatialPolygons(
  lapply(1:length(tl),
         function(i){
           Polygons(
             list(
               Polygon(tl[[i]][c(1:3,1),c("x","y")])
             ),ID=i)
         }
  )
)

ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}

polys_sf<-st_as_sf(polys)
polys_sf%>%ggplot()+geom_sf(colour="red")  
  
# Côté graphiques

#Fonction jpeg
sortunjpeg<-function(x,y,z){
  time<-str_c(unlist(str_extract_all(Sys.time(),"[:alnum:]")),collapse="")
  jpeg(filename = paste0(time,".jpg"), width=y, height = z, quality=100, units = "px",type="cairo")
  plot(x)
  dev.off()
}

devoutsvg::svgout(filename = "Titre.svg", width=10, height = 10)
Graph
invisible(dev.off())

## Reorder ggplot bar
group_by(Pays)%>%
  mutate(TotalVente=sum(KgParMillionDHectares,na.rm=T))%>%
  ungroup()%>%
  mutate(Pays=fct_reorder(Pays,TotalVente))%>%
  ...

## Limiter la largeur des textes dans une facet
facet_wrap(~ grp, labeller = label_wrap_gen(width=10))
                
## Légende sur deux lignes
guides(fill=guide_legend(nrow=2,byrow=TRUE))
                
## Légende plus large
guides(fill = guide_colourbar(barwidth = 20, barheight = 3))

## Plus de clés dans la légende
  scale_fill_viridis_c(option = "inferno",    breaks = c(0, 1, 3, 6,20,25,30),    guide = guide_legend()  ) 

# Divers

## changer le mode scientifique de l'écriture
options(scipen=999)

## Créer Jpeg
jpeg(filename = "GNAGNA.jpg", width=624, height = 450, quality=100, units = "px",type="cairo")
dev.off()
          
## Calculer un âge entre deux périodes
library(lubridate)
get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  age
}

# Manipulation de données

## Faire la somme des colonnes qui commencent par XXX
Paris$A2014 <- Paris %>% 
  select(starts_with("2014")) %>% 
  rowSums()

## dédoublonner
verfi1<-verifdedoublon[!duplicated(verifdedoublon), ]


## Compter selon conditions
coucou%>%
mutate(count4 = length(value[week<=2]),
       sum2 = sum(value[week<=2]))

## Compter les NA dans chaque colonne
Data%>%
purrr::map_df(~.sum(is.na(.)))

## Transformer en NA les ""
Data%>%
na_if("")%>%
count(Variable)

## Créer un json
jsoncars<-TextesPremierEtSecondTour%>%jsonlite::toJSON(pretty = TRUE)
fileConn<-file("T1T2MUN2014-VillesPlus1000hab.json")
writeLines(jsoncars,fileConn)
close(fileConn)
fileConn<-file("T1T2MUN2014-VillesPlus1000hab.txt")
writeLines(jsoncars,fileConn)
close(fileConn)

## Examiner les colonnes numériques
data%>%
select_if(is.numeric)%>%
skimr::slengkim()

## Toutchanger 
mtcars %>%
  mutate_all(as.character)                 

## Si plusieurs éléments par ligne, pour spliter, compter et trier par nombre
data%>%
mutate(nouvelleentite=str_split(entiteavecvirgule,","))%>%
select(nouvelleentite)%>%
unnest()%>%
count(nouvelleentite)%>%
mutate(nouvelleentite = forcats::fct_reorder(nouvelleentite,n))

## multiplier le nombre de lignes
# Créer le dataframe avec une ligne par page
df_pages <- df %>%
  rowwise() %>%
  mutate(page = list(num_page_debut:num_page_fin)) %>%
  unnest(page) %>%
  select(marque, an, page, chapitre)


## Reshape
aql <- reshape2::melt(airquality, id.vars = c("month", "day"),   variable.name = "climate_variable",    value.name = "climate_value") 

## Etat 4001

purrr::map_df(readxl::excel_sheets("./tableaux-4001-ts.xlsx"),function(x){
  readxl::read_excel("./tableaux-4001-ts.xlsx",sheet = x) %>%
    pivot_longer(-c("Index","libellé index"),names_to = "date",values_to = "valeur") %>%
    separate(date,c("annee","mois"),sep = "_") %>%
    add_column(dpt = x)
})

## taux devol
evol <- function(x) {(last(x) - first(x)) / first(x) * 100 }
Pourevolution%>%
  group_by(insee_comm,variable)%>%
  filter(!is.na(valeur))%>%
  filter(!is.na(insee_comm))%>%
  arrange(insee_comm,variable,annee) %>%
  summarise_at(.vars = "valeur", .funs = evol)


 ## Creation dataframe avec dimesions fixes 
data.frame(matrix(NA, nrow = 2, ncol = 3))

## Not in
`%!in%` = function(x,y) !(x %in% y)
Majuscule = function(x) (paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
         
## Transformer les facteurs en caractères
mutate_if(is.factor, as.character)
        
mtcars %>%
  mutate_all(as.character)
                
##Remplacer les trous

a<-c("1000",NA,"1000",NA)
b<-c("mille","millions","mille","sabords")
coalesce(a,b)
ifelse(is.na(a), b,a)

      
## Sortie en csv avec probleme d'encode list
df <- apply(tempfile,2,as.character)
write.csv(df,"file.csv",fileEncoding = "UTF-8",row.names = F)

## Créeer une bloucle for qui ne s'interrompt pas à chaque erreur
for (i in 1:10) {
  tryCatch({
    print(i)
    if (i==7) stop("Urgh, the iphone is in the blender !")
  }, error=function(e){})
}

# Remplacer les lettres en majuscules par une valeur en nom propre
mutate(nouveau=str_replace(ancien,"([[:upper:]]){2,}",str_to_title))
     
# extraire les noms en majuscules meême si plusieurs
ResultatsRegT1G%>%rowwise()%>%mutate(nom_candidat=str_c(unlist(str_extract_all(nom,pattern = "[[:upper:]]{2,}")),collapse=" "))


## Changer le nom de colonnes selon un autre tableau
names(df) <- name$NomComplet[match(names(df), name$Short)]

## Changer le nombre de caractères tronqués dans la console.
glue_collapse(DF$LongString, width = 1000)

for (i in 1:nrow(FiltresCommunesCentre)){
  FiltresCommunesCentre$distancemin_a_20k[i]<-as.numeric(st_distance(FiltresCommunesCentre[i,],CommunesP20K_U))
  # Print a dot at every iteration
  cat(".")
  
  # Check if the iteration number is divisible by 100
  if (i %% 100 == 0) {
    cat(" Iteration ", i)
    # Print a newline character at the end to separate the dots from further output
cat("\n")
  }

}



## sortr un svg pa jour
for (i in listJours){
  print(i)
  tmp<-i
  svglite::svglite(file = paste0("Australie",tmp,".svg"),width = 10,height=8,standalone = T)
  plot(DonneesInfog%>%
         filter(ACQ_DATE==tmp)%>%
         mutate(centre=st_centroid(geometry),
                latitude=st_x(centre),
                longitude=st_y(centre))%>%
         ggplot()+
         labs(title=tmp,
              subtitle=paste0("Départs de feux : ",
                              DenombrementParJour$DepartsDeFeux[DenombrementParJour$ACQ_DATE==tmp]))+
         geom_sf(data=AUSTRALIA_sf,colour="gray",alpha=0.2,fill=NA)+
         geom_hex(aes(x=longitude,y=latitude),binwidth = c(.2, .2), alpha=0.7) +
         scale_fill_gradientn("Densité",colours =  myPalette(5), limits=c(0,1500)
         )+
         coord_sf(datum=NA)+
         theme_void())
  dev.off()
}

#################
#création gif

library(readr)
frontaltair <- read_delim("repo_projetsR/scripts_r/frontaltair.csv", 
                            ";", escape_double = FALSE, col_types = cols(Date = col_character()), 
                            trim_ws = TRUE)

library(tidyverse)
library(gganimate)
library(rnaturalearth)

sp::plot(ne_coastline(scale = "large"))

Cotes <- ne_coastline(scale = 'large', returnclass = 'sf')
Cotes<-st_transform(Cotes, crs=4326)
Cotes2<-st_crop(Cotes,xmin=55,ymin=24,
                xmax=59,ymax=27)

DateAnim<-data_frame(Date=rep("2019-06-12",3*60),
           Hr=c(rep(21,60),rep(22,60),rep(23,60)),
                "Min"=c(rep(0:59,3)))

library(lubridate)

frontaltair$Time<-as.POSIXct(paste0(frontaltair$Date, " ",
                                    frontaltair$Hr, ":",
                                    frontaltair$Min, ":00"))


frontaltair%>%
  ggplot()+
  geom_point(aes(Lon,Lat))+
  geom_sf(data=Cotes2,colour="#0F82BE")+theme_void()+
  coord_sf(datum=NA)+
  transition_reveal(Time)+
  shadow_mark(alpha = 0.2)+
  ease_aes('linear')+shadow_wake(wake_length = 0.1, alpha = FALSE)+
  labs(title =  paste0(frontaltair$Date, " ", frontaltair$Hr,"h", frontaltair$Min))
  

animfront<-frontaltair%>%
  ggplot()+
  geom_point(aes(Lon,Lat), size=6, alpha=0.8, colour="#0F82BE")+
  geom_sf(data=Cotes2,colour="#0F82BE")+theme_void()+
  coord_sf(datum=NA)+
  transition_time(Time)+
  shadow_mark(alpha = 0.2)+
  ease_aes('linear')+shadow_wake(wake_length = 0.1, alpha = FALSE)+
  labs(title =  'Position du Front Altair {frame_time}')
anim_save(filename = "Animation_Front-Altair.gif",
          animation=animate(animfront, fps=10, 
                            duration=10,width=800,
                            height=800))
############

TouteAnimIDF<-MEGADF%>%filter(substr(code_post,1,2)%in%c(75,77,78,91:95))%>%
  ggplot()+
  #geom_sf(data=France,colour="black",fill="white",alpha=0.4)+
  geom_sf(data=ContoursDep%>%filter(code%in%c(75,77,78,91:95)),colour="gray",fill=NA,alpha=0.4)+
  geom_point(aes(longitude,latitude,size=TotalCumParHab, group=seq_along(ordre)),colour="#0F82BE",alpha=0.6)+
  labs(caption="Source : Change.org  
Carte : Le Parisien Data  ")+
  theme_void()+
  theme(text=element_text(family="Graphik", size=16),
        legend.position="none")+
  coord_sf(datum=NA)+
  transition_states(
    ordre,
    transition_length = 1,
    state_length = 1
  ) +
  labs(title = '{closest_state}') +
  ease_aes('linear')+shadow_mark(past = T)

### MAP + files

add_rrps<-function(name) {
  rpps<-read.csv(name,skip=4)
  rpps<-rpps[-1,]
  annee<-paste("20",substr(name,nchar(name)-5,nchar(name)-4),sep="")
  rpps<-rpps %>% tidyr::pivot_longer(-c(X,AGE)) %>% rename(territoire = X,specialite = AGE,tranche = name,nombre = value) %>% separate(territoire,c("code","territoire"),"(?<=([0-9]|2A|2B)) - (?=[A-Z])",extra = "merge",fill = "left") %>% mutate(annee = annee)
  rpps$nombre<-as.numeric(rpps$nombre)
  rpps
}

rpps<-c("rpps-medecins13.csv","rpps-medecins14.csv","rpps-medecins15.csv","rpps-medecins16.csv","rpps-medecins17.csv","rpps-medecins18.csv") %>%
  map_dfr(add_rrps)

rpps %>%
  filter(territoire == "FRANCE ENTIERE") %>%
  inner_join(.,cat_age) %>%
  group_by(annee,categorie) %>%
  filter(specialite == "Ensemble des spécialités d'exercice") %>%
  summarize(somme = sum(nombre,na.rm=TRUE)) %>%
  pivot_wider(names_from = categorie,values_from = somme) %>%
  mutate(taux = plus55 / ensemble * 100)

rpps %>%
  filter(!is.na(code)) %>% 
  inner_join(.,cat_age) %>% 
  group_by(code,annee,territoire,categorie) %>% 
  filter(specialite == "Médecine générale") %>%
  filter(categorie != "autres") %>% 
  summarize(somme = sum(nombre,na.rm=TRUE)) %>% 
  pivot_wider(names_from = categorie,values_from = somme) %>% 
  mutate(taux = plus55 / ensemble) %>%
  select(-plus55,-ensemble) %>%
  pivot_wider(names_from = annee,values_from = taux) %>%
  arrange(desc(`2018`)) %>% 
  head(9) %>%
  pivot_longer(-c(code,territoire),names_to = "annee",values_to = "taux") %T>% print() %>%
  ungroup() %>%
  mutate_at(vars(territoire), funs(factor(., levels=unique(.)))) %>%
  ggplot(aes(x=annee,y=taux)) + 
    facet_wrap(~territoire,ncol=3) + 
    geom_col(fill="#82f5cf") + labs(x="",y="") + 
    geom_text(aes(label = ifelse(annee=="2018"|annee=="2013",paste(floor(taux*100),"%",sep=""),NA)),vjust=1.5,size=4) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    theme_minimal() + 
    theme(legend.position = "none",text = element_text(family = "Roboto", color = "black",size=12))


# télécharger des csv.gz

# Liste des URLs
urls <- c(
  "https://exemple.com/fichier1.csv.gz",
  "https://exemple.com/fichier2.csv.gz",
  "https://exemple.com/fichier3.csv.gz"
)

# Fonction pour télécharger et lire un fichier
download_and_read <- function(url) {
  temp_file <- tempfile(fileext = ".csv.gz")
  download.file(url, temp_file, mode = "wb")
  df <- read_csv(temp_file)
  unlink(temp_file)
  return(df)
}

# Télécharger et combiner tous les fichiers
df_final <- urls %>%
  map_dfr(download_and_read)


#### isochrones ign
points <- list("1.46405,43.58329", "1.48,43.57")


response <- purrr::map_dfr(points, ~GET("https://wxs.ign.fr/calcul/isochrone/isochrone.json",
                                        query = list(
                                          location=.x,
                                          method="Time",
                                          graphName="Pieton",
                                          exclusions="",
                                          time=600,
                                          holes="false",
                                          smoothing="true")) %>%
                             content(as = "text", encoding = "UTF-8") %>%
                             fromJSON() %>%
                             unlist() %>%
                             tibble::enframe() %>%
                             tidyr::pivot_wider() %>%
                             sf::st_as_sf(wkt = "wktGeometry", crs = 4326))


###########
# netcdf
nc<-nc_open(ncfname)

#  Get the variable values
rr_values <- ncvar_get(nc, "rr")
lat_values <- ncvar_get(nc, "latitude")
lon_values <- ncvar_get(nc, "longitude")
time_values <- ncvar_get(nc, "time")

# Convert time values to Date format
time_values <- as.Date(time_values, origin = "1950-01-01")

# Define the indices for the desired latitudes, longitudes, and time range
lat_indices <- which(lat_values >= 40 & lat_values <= 55)
lon_indices <- which(lon_values >= -10 & lon_values <= 12)
time_indices <- which(time_values >= as.Date("2019-01-01"))

# Subset the variable values based on the indices
lat_subset <- lat_values[lat_indices]
lon_subset <- lon_values[lon_indices]
time_subset <- time_values[time_indices]
rr_subset <- rr_values[ lon_indices, lat_indices,time_indices]

# Create a dataframe with the subsetted values
df <- expand.grid(latitude = lat_subset,
                  longitude = lon_subset,
                  time = time_subset)
df$rr <- as.vector(rr_subset)
