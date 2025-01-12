#--------------------------------------------------------------------------------------------------#
## Script destiné à produire des statistiques et des graphiques pour étudier les conditions
## météorologiques lors des signalements de piqûres dans le cadre de la collecte du programme de 
## sciences participatives "Citique" (https://www.citique.fr/) à l'aide de l'application  
## "Signalement tiques" (https://www.citique.fr/signalement-tique/).
##-------------------------------------------------------------------------------------------------#
## Date : 25/08/2021
## Authors : Khaldoune Hilami, Vincent Godard
##--------------------------------------------------------------------------------------------------#
##--------------------------------------------------------------------------------------------------#
## Code des scripts produisant les différents résultats de l'article.
##
## Code pour Tableau n°3 – Pour la France entière, selon le 1er décile, la moyenne et le 9ème décile:
## Paramètres météorologiques associés aux 14 657 lieux et dates de signalements comparés à ceux des 
## mêmes dates mais pour un semis  aléatoires (France, January 2017 – April 2020; soit 995 jours).
## 
## Code pour Tableau n°4–Pour la France entière, selon le 1er quantile, le 2ème quantile (la médiane)
## et le 3ème quantile: Paramètres météorologiques associés aux 14657 lieux et dates de signalements
## comparés à ceux des mêmes dates mais pour un semis de lieux aléatoires (France, January 2017 –
## April 2020; soit 995 jours).
##
## Code Tableau n°5 – En Île-de-France, selon le 1er décile, la moyenne et le 9ème décile :
## Paramètres météorologiques associés aux 17 46 lieux et dates de signalements comparés à ceux des
## mêmes dates mais pour un semis de lieux aléatoires (January 2017 - April 2020; 995 jours).
## 
## Code Tableau n°6 – En Alsace-Lorraine, selon le 1er décile, la moyenne et le 9ème décile :
## Paramètres météorologiques associés aux 2 761 lieux et dates de signalements comparés à ceux des 
## mêmes dates mais pour un semis de lieux aléatoires (January 2017 - April 2020; 995 jours).
## 
## Code Tableau n°7 – En Rhône-Alpes, selon le 1er décile, la moyenne et le 9ème décile :
## Paramètres météorologiques associés aux 1 607 lieux et dates de signalements comparés à ceux des 
## mêmes dates mais pour un semis de lieux aléatoires (January 2017 - April 2020; 995 jours).
## 
## Code Tableau n°8 – Caractéristiques hivernales (octobre à mars), pour la France entière, selon le
## 1er décile, la moyenne et le 9ème décile des paramètres météorologiques associés aux 1 095 lieux
## et dates de signalements comparés à ceux des mêmes dates mais pour un semis de lieux aléatoires
## (France, January 2017 – April 2020, soit 995 jours).
##
## Code Figure n°6 – Profils temporels des variables météorologiques associés aux 14 657 lieux
## et dates de signalements comparés à ceux des mêmes dates, mais pour un semis de lieux aléatoires 
## (France, April 1st 2017 – April 5th 2020, soit 1100 jours).
##
## Code Figure n°7 – Profils météorologiques associés aux 14 657 lieux et dates de signalements
## comparés à ceux des mêmes dates, mais pour un semis de lieux aléatoires 
## (France, July 15th 2017 – April 5th 2020, soit 995 jours).
##
##
##-------------------------------------------------------------------------------------------------#
############################## Preparation de la donnee  ###########################################

start = Sys.time()

#### 1. Mise en place de l’environnement de travail
setwd('./')
getwd()

### Appel des librairies requises
# le script ne se lancera pas correctement sans
# l’invocation prelalable de ces librairies
require(RSQLite)
require(ggplot2)
require(cowplot)
require(DT)
require(utils)
############################## Connexion a la base SQLite###########################################
### 2.3 Methode d’importation depuis la BDD geographique SQLite/SPatialite
## /!\ Ne pas commenter ni supprimer /!\

## La base de donnees ayant une taille relativement consequente
# elle est stoquee sous le format d’un fichier attache au depot Git
# centrale, sans toutefois de versionnement pris en charge par celui-ci
# le telechargement prealable est une etape obligatoire suite au clonage
# du projet sur la machine locale. Une connexion a internet est egalement requise
# ces opération sont effectuées lors du téléchargement (clonage) du projet

datapath='../../data/'
target='citique.zip'
target=paste0(datapath,target)
#unzip(target, exdir=datapath)
print('Génération des objets à partir de la base. Veuillez patienter ...')

# Etablissement de la connexion avec la base SQLite
sqlitedrv <- RSQLite::SQLite()
sqlitedb <- dbConnect(sqlitedrv, '../../data/citique.db')
# Recurperation de la liste des tables utiles
tablist <- dbListTables(sqlitedb)

# Requete groupee des tables de donnee
# c’est cette partie qui recupere la donnee de la base SQLite
for (tab in tablist){
  
  query <- paste('SELECT * FROM ',tab,';')
  curs <- dbSendQuery(sqlitedb, query)
  df <- dbFetch(curs)
  assign(tab, df)
  dbClearResult(curs)
  rm(query)
  rm(df)
  rm(curs)
  
}

print('Fin de la génération des objets')

# La BDD SQLite sauvegarde le dates sous format text
# avant d’utiliser les jeux de donnees on converti 
# les colonnes date au format approprie
MFdata$date_iso <- as.Date(MFdata$date_iso)
DSKdata$date_releve <- as.Date(DSKdata$date_releve)
DSKdata_42avg$date_releve <- as.Date(DSKdata_42avg$date_releve) 
animdata$date_piqure_saisie <- as.Date(animdata$date_piqure_saisie)
DSKdata_700avg$date_releve <- as.Date(DSKdata_700avg$date_releve) 
DSKdata_700avg_al$date_releve <- as.Date(DSKdata_700avg_al$date_releve) 
DSKdata_700avg_ra$date_releve <- as.Date(DSKdata_700avg_ra$date_releve) 
DSKdata_700avg_idf$date_releve <- as.Date(DSKdata_700avg_idf$date_releve) 

## Uniformisation des parametres en % de MF - donnee moyennee
# humidite
MFdata$humidite_floor <- floor(MFdata$humidite)
MFdata$humidite_ceiling <- ceiling(MFdata$humidite)
# nebulosite
MFdata$nebulosite_floor <- floor(MFdata$nebulosite)
MFdata$nebulosite_ceiling <- ceiling(MFdata$nebulosite)

### 2.4 Création des dataframes par région comparées dans l'article

## 2.4.1 Création du subset pour l'IDF:
# signalements
animdata_idf <- animdata[animdata$departement_code %in% c("75","77","78",91:95),]
# semi meteo
DSKdata_idf <- DSKdata[DSKdata$departement_code %in% c("75","77","78",91:95),]

## 2.4.2 Création du subset pour l'AL:
# signalements
animdata_al <- animdata[ animdata$departement_code %in% c("54","55","57","88","67","68"),]
# semi meteo
DSKdata_al <- DSKdata[ DSKdata$departement_code %in% c("54","55","57","88","67","68"),]

### 2.4.3 Création du subset pour RA:
# signalements
animdata_ra <- animdata[ animdata$departement_code %in% c("01","07","26","38","42","69","73","74"),]
# semi meteo
DSKdata_ra <- DSKdata[ DSKdata$departement_code %in% c("01","07","26","38","42","69","73","74"),]

### 2.4.3. Sélection de la période hivernale "longue" (6 mois)
# signalements
animdata_winter17_long <- animdata[animdata$date_piqure_saisie >= "2017-10-01" & animdata$date_piqure_saisie <= "2018-03-31",] 
animdata_winter18_long <- animdata[animdata$date_piqure_saisie >= "2018-10-01" & animdata$date_piqure_saisie <= "2019-03-31",] 
animdata_winter19_long <- animdata[animdata$date_piqure_saisie >= "2019-10-01" & animdata$date_piqure_saisie <= "2020-03-31",] 
animdata_winter_long <-rbind(animdata_winter17_long, animdata_winter18_long, animdata_winter19_long)
# semi meteo
DSKdata_winter17_long <- DSKdata[DSKdata$date_releve >= "2017-10-01" & DSKdata$date_releve <= "2018-03-31",]
DSKdata_winter18_long <- DSKdata[DSKdata$date_releve >= "2018-10-01" & DSKdata$date_releve <= "2019-03-31",]
DSKdata_winter19_long <- DSKdata[DSKdata$date_releve >= "2019-10-01" & DSKdata$date_releve <= "2020-03-31",]
DSKdata_winter_long <-rbind(DSKdata_winter17_long, DSKdata_winter18_long, DSKdata_winter19_long)

### 2.4.3. Sélection de la période hivernale "courte" (4 mois)
# signalements
animdata_winter17_short <- animdata[animdata$date_piqure_saisie >= "2017-11-01" & animdata$date_piqure_saisie <= "2018-02-28",] 
animdata_winter18_short <- animdata[animdata$date_piqure_saisie >= "2018-11-01" & animdata$date_piqure_saisie <= "2019-02-28",] 
animdata_winter19_short <- animdata[animdata$date_piqure_saisie >= "2019-11-01" & animdata$date_piqure_saisie <= "2020-02-28",] 
animdata_winter_short <-rbind(animdata_winter17_short, animdata_winter18_short, animdata_winter19_short)
# semi meteo
DSKdata_winter17_short <- DSKdata[DSKdata$date_releve >= "2017-11-01" & DSKdata$date_releve <= "2018-02-28",]
DSKdata_winter18_short <- DSKdata[DSKdata$date_releve >= "2018-11-01" & DSKdata$date_releve <= "2019-02-28",]
DSKdata_winter19_short <- DSKdata[DSKdata$date_releve >= "2019-11-01" & DSKdata$date_releve <= "2020-02-28",]
DSKdata_winter_short <-rbind(DSKdata_winter17_short, DSKdata_winter18_short, DSKdata_winter19_short)

## Nettoyage des variables inutiles
rm(list = c(paste('animdata_winter', 17:19, '_long', sep=''),
                paste('animdata_winter', 17:19, '_short', sep=''),
                        paste('DSKdata_winter', 17:19, '_long', sep=''),
                                paste('DSKdata_winter', 17:19, '_short', sep='') ) )

print('Fin de la preparation des dataframes uniformises')

################################### Definitions des fonctions  ####################################

# fonction de benchmark et calcul des duree de traitement
benchmark <- function (start, end){
  
  duration = end-start
  duration <- round(duration, 1)
  unit <- units(duration)
  cat('Le temps ecoulé est: ',duration, ' ', unit, '\n')
  
  return(duration)
  
}

### 5. Boostrap pour stabiliser indicateurs et intervalles de confiance du t.test d'une moyenne
# Cf. médiane/quantile Poinsot, 2005, R pour les statophobes, pp.13-1510

## Fonction de Calcul d'un IC en utilisant le Bootstrap pour des petits échantillons
#  ou avec une absence de normalité avérée (cf. section 3.4 de Poinsot, p.14)
ic_calculator <- function(param, calcul){
        
  ## création d'une table 1000x3 index vides
  ## pour initialiser les dimensions et la valeurs des cellules du dataframe qui suit
  ## création d'un DF initialisé avec les valeurs de c1 pour les cellules et le nombre de lignes.
  humDF <- data.frame("lower_quantile"=c(0:999),
                      "middl_quantile"=c(0:999),
                      "upper_quantile"=c(0:999))*NA 
  
  ## boucle qui calcule 1000 fois sur un échantillon de 50 tirages les centiles
  ## 25, 50 et 75 à partir du vecteur index (cf. infra)
  ## /!\ il faut exclure les NA du calcul de la moyenne également /!\
  ## Calcule conditionnel selon l’option de la variable calcul
  # Calcul de quartiles
  if(calcul=='quartile'){
          
    for (i in 1:1000){
          humDF$lower_quantile[i] <- quantile(sample(param, 50, replace=T), 0.25, na.rm = T)
          humDF$middl_quantile[i] <- quantile(sample(param, 50, replace=T), 0.50, na.rm = T)
          humDF$upper_quantile[i] <- quantile(sample(param, 50, replace=T), 0.75, na.rm = T)
    }
  # Calcul de centiles
  }else if (calcul=='decile'){
    
    for (i in 1:1000){
      
      humDF$lower_quantile[i] <- quantile(sample(param, 50, replace=T), 0.1, na.rm = T)
      humDF$middl_quantile[i] <- mean(sample(param, 50, replace=T), na.rm = T)
      humDF$upper_quantile[i] <- quantile(sample(param, 50, replace=T), 0.9, na.rm = T)
      
    }    
    
  # Calcul de centiles
  }else if (calcul=='centile'){
      
    for (i in 1:1000){
      
      # Modif Vg 29 oct 2023
      humDF$lower_quantile[i] <- quantile(sample(param, 50, replace=T), 0.06, na.rm = T)
      humDF$middl_quantile[i] <- mean(sample(param, 50, replace=T), na.rm = T)
      humDF$upper_quantile[i] <- quantile(sample(param, 50, replace=T), 0.94, na.rm = T)          
    }
          
  } else{
          print('Aucune opération valide demandée')
  }
  
  moy_quant1 <- mean(humDF$lower_quantile) ## moyenne des 1000 1er quantiles
  moy_quant2 <- mean(humDF$middl_quantile) ## moyenne des 1000 2eme quantiles
  moy_quant3 <- mean(humDF$upper_quantile) ## moyenne des 1000 3eme quantiles
  
  humDF_C25.sort=sort(humDF$lower_quantile)
  humDF_C50.sort=sort(humDF$middl_quantile)
  humDF_C75.sort=sort(humDF$upper_quantile)
  
  ## recuperation des bornes de l’IC
  quant1_IC_bas <- humDF_C25.sort[25]
  quant1_IC_haut <- humDF_C25.sort[975]
  
  ## recuperation des bornes de l’IC
  quant2_IC_bas <- humDF_C50.sort[25]
  quant2_IC_haut <- humDF_C50.sort[975]
  
  ## recuperation des bornes de l’IC
  quant3_IC_bas <- humDF_C75.sort[25]
  quant3_IC_haut <- humDF_C75.sort[975]
  
  # La fonction retourne ce vecteur numeric contenant les 
  # trois moyennes et leur intervalle de confiance
  ic_vector = c(moy_quant1, quant1_IC_bas, quant1_IC_haut,
                  moy_quant2, quant2_IC_bas, quant2_IC_haut,
                    moy_quant3, quant3_IC_bas, quant3_IC_haut)
  # arrondire a deux decimales
  ic_vector <- round(ic_vector, digits=2)
  
  return(ic_vector)

}

## Fonction de fabrication des tables statistiques d’IC
ic_table_maker <- function(reportingdf, randomdf, paramvector, calcul){
                
  #liste vide a remplir
  ic_table <- list()
  
  # boucle de calcule implementant la fonction quantile
  # avec filtrage par le vecteur vectornames sur le dataframe animdata
  for (name in paramvector ){
  
          # isolation de la colonne cible pour chaque iteration de la boucle
          param_hum <- reportingdf[,name]
          # affectation dans une variable intermediaire
          result_hum <- ic_calculator(param_hum, calcul)
          # isolation de la colonne cible pour chaque iteration de la boucle
          param_dsk <- randomdf[,name]
          # affectation dans une variable intermediaire
          result_dsk <- ic_calculator(param_dsk, calcul)
          # insertion du vecteur numeric resultant dans la liste
          ic_table[[paste(name,'_reporting', sep='')]] <- result_hum
          # insertion du vecteru numeric resultant dans la liste
          ic_table[[paste(name,'_random', sep='')]] <- result_dsk
          rm(list='param_hum','result_hum','param_dsk','result_dsk')
  
  }
  
  # transformation (cast) de la list en data.frame
  ic_table <- as.data.frame(ic_table)
  # transposition du tableau (data.frame)
  ic_table <- as.data.frame(t(ic_table))
  
  # Creation d’un vecteur de nom de colonne pour le tableau transpose
  nom_de_colonne = c('moy_quant1', 'quant1_IC_bas', 'quant1_IC_haut',
                        'moy_quant2', 'quant2_IC_bas', 'quant2_IC_haut',
                          'moy_quant3', 'quant3_IC_bas', 'quant3_IC_haut')
  
  # renommage des colonnes
  colnames(ic_table) <- nom_de_colonne
  
  ## export au format csv (de-commenter en cas de besoin)
  # date <- format(Sys.time(), "%A_%b_%d_%Hh%Mm%Ss_%Y")
  # filename <- paste('ic', calcul, date, '.csv', sep = '_')
  # write.csv(ic_table, filename )
  
  return(ic_table)

}

### 6. Fonctions de fabrication des graphiques comparatifs avec ggplot2
# La fonction retourne un objet de type liste contenant les graphiques 
# des analyses. Le resultat peut etre ensuite utilise avec 
# une librairie d’aggregation de graphiaues comme cowplot
batch_histogram <- function(hist_dataset, dens_dataset, hist_paramnames, dens_paramnames){
  
    if ( length(hist_paramnames) != length(dens_paramnames) ){
      stop('Les vecteurs de parametres ne sont pas de tailles egales.')
    }
  
    # Listes vide pour accueillir les noms de parametres et objets
    paramlist <- list()
    graphlist <- list()
    
    # fonction de fabricatin des graphs individuels
    make_hist <- function(paramdsk, parammf){
      
      if (paramdsk=='humidity' ){
        bwidth = 2
      }else if (paramdsk=='cloudcover'){
        bwidth = 2
      }else if(paramdsk=='pressure'){
        bwidth = 2
      }else{
        bwidth = 1
      }
      
      p <- ggplot(hist_dataset, aes(hist_dataset[,paramdsk]))+
        geom_histogram( binwidth = bwidth, color='green', fill='black', aes(y=..density..), alpha=.55)+
        geom_density(data = dens_dataset,
                     color='blue',
                     aes(dens_dataset[,parammf]),
                     fill='light blue',
                     alpha=.2)
      
      return(p)
    }
    
    # boucle de remplissage de la liste de correspondance
    for ( i in 1:length(hist_paramnames) ){
      # cat(hist_paramnames[i], '|------>', dens_paramnames[i],'\n')
      paramlist[[ hist_paramnames[i] ]] <- c(hist_paramnames[i], dens_paramnames[i])
    }
  
    # boucle de fabrication des graphiques scope local
    make_hist_batch <- function(){
      for (param in paramlist )local({
        param <- param
        paramdsk <- param[1]
        parammf <- param[2]
        # print(paramdsk, parmmf)
        p <- make_hist(paramdsk, parammf)
        p <- p+xlab(label= paramdsk)+plotstyle
        # print(p)
        graphlist[[paramdsk]] <<- p
        rm(p)
        rm(param)
      })
    }
    
    make_hist_batch()
    
    # La fonction retourne un objet de type liste contenant les graphiques produits
    return(graphlist)
}

# fonction du comparatif du test shapiro dsk vs mf fabrication en lot
# Cette fonction retourne un objet de type dataframe contenant les analyses croisees
shapiro_batch <- function (dsk_paramnames, mf_paramnames){
  
  # Liste vide pour accueillir les nom de parametres
  paramlist <- list()
  shapiroList <- list()
  
  # fonction du comparatif du test shapiro dsk vs mf
  shapiro <- function(paramDSK, paramMF){
    
    # La fonction R invisible() supprime la sortie en console, Inutile
    # puisque le resultat est disponible dans le tableau des resulats
    shapiroDSK <- shapiro.test(paramDSK)
    shapiroMF <- shapiro.test(paramMF)
    
    listShapiro <- list()
    listShapiro[['shapiroDSK']] <- c('shapiro_test'=shapiroDSK$statistic[[1]],
                                                'p.value'=shapiroDSK$p.value[[1]] )
    listShapiro[['shapiroMF']] <- c('shapiro_test'=shapiroMF$statistic[[1]],
                                                'p.value'=shapiroMF$p.value[[1]] )
    
    return(listShapiro)
    
  }
  
  # boucle de remplissage de la liste de correspondance
  for (i in 1:11){
    #cat(dsk_paramnames[i], '|------>', mf_paramnames[i],'\n')
    paramlist[[ dsk_paramnames[i] ]] <- c(dsk_paramnames[i], mf_paramnames[i])
  }
  
  # boucle de calcul
  for (param in paramlist ){
    
    paramdsk <- param[1]
    parammf <- param[2]
    result <- shapiro(DSKdata_42avg[,paramdsk], MFdata[,parammf])
    # print(result)
    
    shapiroList[[paramdsk]] <- c(result$shapiroDSK['shapiro_test'], result$shapiroDSK['p.value'])
    shapiroList[[parammf]] <- c(result$shapiroMF['shapiro_test'], result$shapiroMF['p.value'])
    
    
  }
  
  shapiro_table <- as.data.frame(shapiroList)
  shapiro_table <- t(shapiro_table)
  return(shapiro_table)
  
}

# fonction de fabrication en masse des tables comparatives du t.test
# Cette fonction retourne un objet de type dataframe contenant les analyses croisees
t.test_batch <- function (dsk_paramnames, mf_paramnames){
  
  # Liste vide pour accueillir les nom de parametres
  paramlist <- list()
  t.test_List <- list()
  t_matrix <- matrix(nrow=4,ncol = length(dsk_paramnames))
  
  # boucle de remplissage de la liste de correspondance
  for (i in 1:11){
    # cat(dsk_paramnames[i], '|------>', mf_paramnames[i],'\n')
    paramlist[[ dsk_paramnames[i] ]] <- c(dsk_paramnames[i], mf_paramnames[i])
  }
  
  i=1
  # boucle de calcul
  for (param in paramlist ){
    
    paramdsk <- param[1]
    parammf <- param[2]
    test_result <- t.test(DSKdata_42avg[,paramdsk], MFdata[,parammf])
    # cat(param[1], '|------>', param[2],'\n')
    test_result_vector <- c(test_result$statistic[[1]], test_result$p.value[[1]],
                            test_result$parameter[[1]], test_result$conf.int[[1]])
    t_matrix[,i] <- round(test_result_vector, digits = 10)
    i=i+1
    
  }
  
  t_table <- as.data.frame(t_matrix)
  names(t_table) <- dsk_paramnames
  rownames(t_table) <- c('t.test_vs_MF', 'p.value_vs_MF', 'ddl_vs_MF', 'IC_vs_MF')
  return(t_table)
}

# fonction parametrique permettant de fabriquer les tables comparatives
# du test cible wilcoxon ou kruskal-wallis
# Cette fonction retourne un objet de type dataframe contenant les analyses croisees
kwcox_table <- function (dsk_paramnames, mf_paramnames, test='wilcox'){
  
  # Liste vide pour accueillir les nom de parametres
  paramlist <- list()
  # matrice vide pour le stockage des resultats des calculs
  kwcox_matrix <- matrix(nrow=2, ncol = length(dsk_paramnames))
  
  # boucle de remplissage de la liste de correspondance
  for (i in 1:11){
    # cat(dsk_paramnames[i], '|------>', mf_paramnames[i],'\n')
    paramlist[[ dsk_paramnames[i] ]] <- c(dsk_paramnames[i], mf_paramnames[i])
  }
  
  # i est l’indice matriciel permettant de remplir la matrice par colonne
  i=1
  # boucle de calcul iteratif des tests
  cat("Fabrication des données pour les tests et remplissage de la matrice de calcul\n")
  for (param in paramlist ){
    
    paramdsk <- param[1]
    parammf <- param[2]
    
    # Preparation de la donnee des tests
    # cat("__ Fabrication des données pour le test KW __\n")
    dskdf <- data.frame(param=DSKdata_42avg[,paramdsk], data_provider='dsk')
    mfdf <- data.frame(param=MFdata[,parammf], data_provider='mf')
    kwcoxdata <- rbind(dskdf,mfdf)
    # cat("__ Objet de donnée pour KW Fabriqué__", paramdsk, "|------>", parammf, "__\n\n")
    
    if(test=='wilcox'){
      test_result <- wilcox.test(param ~ data_provider, data = kwcoxdata)
      rnames <- c('wilcox_vs_MF', 'p.value_vs_MF')
    } else if (test=='kruskal'){
      test_result <- kruskal.test(param ~ data_provider, data = kwcoxdata)
      rnames <- c('kruskal_vs_MF', 'p.value_vs_MF')
    }else{
      print('Aucun nom de test valide fourni. tapez wilcox ou kruskal.')
    }
    
    test_result_vector <- c(test_result$statistic[[1]],
                            test_result$p.value[[1]])
    kwcox_matrix[,i] <- test_result_vector
    i=i+1
    
  }
  
  kwcox_table <- as.data.frame(kwcox_matrix)
  names(kwcox_table) <- dsk_paramnames
  rownames(kwcox_table) <- rnames
  return(kwcox_table)
  
}

## Defining plotsaving Function Template
plotsave <- function(plot, plotname, extension='png', format='landscape', plotpath=NULL){
                
                if(format=='portrait'){
                  height=16.54  #(8.27/11.69 A4)
                  width=11.69
                }else if (format=='landscape'){
                  width=16.54 # papier A3 
                  height=11.69
                }
                  
                ggsave2(filename = plotname,
                       plot = plot,
                       path = plotpath,
                       device= extension,
                       width=width, # format A4
                       height=height,
                       units = 'in',
                       # dpi = 92,
                       limitsize=TRUE)
}

## Definition d’un theme general pour les graphs du module
plotstyle <-  theme(plot.title = element_text(hjust = .5, face = 'bold', size = 8.5))+
              theme(axis.title = element_text(face = 'bold', size = 8.5))+
              theme(axis.text.x  = element_text( size = 8.5))+
              theme(axis.text.y  = element_text( size = 8.5))

# Fonction de fabrication des grilles des series temporelles
# elle recupere une chaine de caracteres du parmetre a analyser
# et retourne un objet de type liste contenant les graphiques
# des analyse. Le resultat peut etre ensuite utilise avec 
# une librairie d’aggregation de graphiaues comme cowplot
# Le nombre de parametres fut reduit de 12 à 5 suite à la recommendation des relecteurs
# modif du 2023-10-28 Khi.
weatherPlotGrid <- function(param, mode){
    
    paramlist <- list("temperature"='Temperature (°C)',
                      # "temperaturehigh"='Day temperature (°C)',
                      # "temperaturelow"='Night temperature',
                      'humidity'='Humidity (%)',
                      "dewpoint"='Dewpoint (°C)',
                      "pressure"='Atmospheric Pressure (hPa)',
                      "windspeed"='Windspeed (m/s)'
                      # "visibility"='Visibility (km)',
                      # "cloudcover"='Cloud cover (%)',
                      # 'precipintensity'='Precipitation Intensity mm/h',
                      # "windgust"='Wind Gust (m/s)',
                      # 'uvindex'='UV Index (scale 1 to 10)'
                      )
    
    datalist <- list('france'=list('name'='France', 'report'=animdata, 'witness'=DSKdata_700avg ),
                     'idf'=list('name'='île-de-France', 'report'=animdata_idf, 'witness'=DSKdata_700avg_idf ),
                     'al'=list('name'='Alsace', 'report'=animdata_al,'witness'=DSKdata_700avg_al ),
                     'ra'=list('name'='Rhône-Alpes', 'report'=animdata_ra, 'witness'=DSKdata_700avg_ra )
    )
    
    graphlist <- list()
    
    weatherPlot <- function(reportdata, witnessdata, region, param){
      
      paramname <- paramlist[[param]]
      
      ## General theme
      legende <- c('Signalements'='#0000ff',
                   'Signalements Modèle'='#000000',
                   'Semis Régulier'='#00ff00',
                   'Semis Régulier Modèle'='#ff0000',
                   'Equinox'='orange',
                   'Solstice'='grey50')
      # Explication détaillée du graphique
      # la date de la piqûre est en abscisse
      # la donnée principale est tirée du dataframe animdata qui représente la table de donnée 
      # de signalements citik_humains_clean_weather_strict.csv
      # dans mes script je travaille directememnt sur la base de donnée géographique postgis
      # la donnée météo témoin DSKdata_700avg provient de la table darksky_maille_700_avg
      # le grahique est établi à partir de la colonne temperature représentat
      # la température moyenne obtenue en moyennant temphigh et templow
      ggplot(reportdata, aes(x=date_piqure_saisie))+
        # c'est la ligne qui affiche les poctuels des signalements
        geom_jitter(aes(y=reportdata[,param], color='Signalements'), size=.1, alpha=.6)+
        # Cette ligne établit la courbe lisse noire des poinctuels
        # par défaut elle utilise la méthode GAM ou general additive method si le nombre de points est
        # supérieur à 1000, en utilisant en arrière plan la méthode method="gam", formula = y ~ s(x)
        # comme paramètre, donc la fonction s(x) du packet R mgcv
        # pour plus de détail consultez les références d'explication de la méthode additive 
        geom_smooth(method = 'gam', formula=y~s(x, bs = "cs"), aes(y=reportdata[,param], color='Signalements Modèle'), size=.5)+
        #cette lingne de code établit la ligne verte de la température témoin
        geom_line(data = witnessdata,
                  aes(date_releve, witnessdata[,param], color='Semis Régulier'),
                  size=.4,
                  alpha=.7)+
        #Meme chose que ci-dessus mais courebe lisse rouge de la donnée météo, donc température témoin
        geom_smooth(method = 'gam', formula=y~s(x, bs = "cs"), data=witnessdata, aes(date_releve, witnessdata[,param], color='Semis Régulier Modèle'), size=.3)+
        geom_line(y=0, colour='black', linetype='dotted', alpha=.7, size=.5)+
        # equinox du printemps
        geom_vline(xintercept=as.Date(c( '2017-03-21', '2018-03-21', '2019-03-21','2020-03-21' )),
                   colour='orange',
                   linetype='twodash',
                   alpha=.8,
                   size=.5)+
        # solstice d’hiver
        geom_vline(xintercept=as.Date(c( '2017-12-21', '2018-12-21', '2019-12-21')),
                   colour='grey50',
                   linetype='twodash',
                   alpha=.8,
                   size=.5)+
        # solstice d’ete
        geom_vline(xintercept=as.Date(c( '2017-06-21', '2018-06-21', '2019-06-21')),
                   colour='grey50',
                   linetype='twodash',
                   alpha=.8,
                   size=.5)+
        #e quinox d’automne
        geom_vline(xintercept=as.Date(c( '2017-09-21', '2018-09-21', '2019-09-21')),
                   colour='orange',
                   linetype='twodash',
                   alpha=.8,
                   size=.5)+
        #titre du graph
        ggtitle(paste('Profil temporel de "',paramname,'" associée aux ',nrow(animdata),' lieux et dates de signalements comparés 
                               à ceux des mêmes dates mais pour un semis régulier de lieux: ',region,' de 2017-03-31 à 2020-04-01'))+
        xlab(label = 'Date')+
        ylab(label=paramname)+
        labs(color='Légende: ')+
        # les thèmes et labels des axes
        theme(axis.text.x = element_text(angle = 35, color='grey20', size = 9, vjust = 1, hjust = 1))+
        # theme(axis.text.y = element_text(color='grey20', size = 6) )+
        theme(legend.position = 'top', legend.text = (element_text(size = 8)))+
        # Les elements de la legende en une seule rangee
        guides(col=guide_legend(nrow = 1))+
        #Cette ligne est facultative, elle sert uniquement au cas où on a besoin
        #de zoom sur une période de l'année ou pour restreindre le champ temporel
        scale_y_continuous( breaks = seq(floor(min(animdata[,param], na.rm = T)),
                                         ceiling(max(animdata[,param], na.rm = T)),
                                         by=4),
                            limits = c(floor( min(animdata[,param], na.rm = T)),
                                       ceiling(max(animdata[,param], na.rm = T)) ) )+
        scale_x_date( expand = c(0,0),
                      limits=as.Date( c('2017-03-31','2020-04-01')),
                      date_labels = '%b %Y',
                      date_breaks = '2 month')+
        scale_color_manual(values=legende)+plotstyle
      
    }
    
    getGrid_by_param <- function(param){
      
      graphlist[['france']]<-weatherPlot(datalist$france$report, datalist$france$witness, datalist$france$name, param)
      graphlist[['idf']]<-weatherPlot(datalist$idf$report, datalist$idf$witness, datalist$idf$name, param)
      graphlist[['al']]<-weatherPlot(datalist$al$report, datalist$al$witness, datalist$al$name, param)
      graphlist[['ra']]<-weatherPlot(datalist$ra$report, datalist$ra$witness, datalist$ra$name, param)
      
      # Cette fonction retourne un objet de type liste contenant 
      # les graphiques generes contenant les analyse. Le resultat peut  
      # etre ensuite aggrege avec une librairie d’aggregation de graphiaues comme cowplot
      plotgrid <- plot_grid(plotlist=graphlist, labels = 'AUTO', ncol=2, nrow=2, align = 'hv')
        
        return(plotgrid)
    }
    
    getGrid_by_region <- function(param){
      
      for (paramname in names(paramlist)){
        
        graphlist[[paramname]] <- weatherPlot(datalist[[param]]$report,
                                              datalist[[param]]$witness,
                                              datalist[[param]]$name,
                                              paramname)
        
      }
    
      # Cette fonction retourne un objet de type liste contenant
      # les graphiques generes contenant les analyse. Le resultat peut
      # etre ensuite aggrege avec une librairie d’aggregation de graphiaues comme cowplot
      plotgrid <- plot_grid(plotlist=graphlist, labels = 'AUTO', ncol=2, nrow=3, align = 'hv')
      
      return(plotgrid)
      
    }
    
    if (mode=='param'){
      
      plotgrid <- getGrid_by_param(param)
      return(plotgrid)
      
    }else if (mode=='region'){
      
      plotgrid <- getGrid_by_region(param)
      return(plotgrid)
      
    }else {
      stop('Aucun mode de mosaicage fourni: france, idf, al, ra')
    }
    
}

export_csv_table <- function(table_obj){
  
  table_name <- deparse(substitute(table_obj))
  table_name <- paste0(table_name,".csv")
  write.csv(table_obj, table_name, quote = F)
  
}

end_prep = Sys.time()
print('Fin de la preparation des fonctions de calcul')
benchmark(start, end_prep)

######################################### Programme principal ######################################

print('Debut du program principal')

### Vecteur de caracteres contenant les parametres meteo a traiter
vectornames <- c("temperature",  "temperaturelow", "temperaturehigh", "humidity",
                      "dewpoint",   "pressure",      "windspeed",
                         "visibility", "cloudcover",   "windgust", "uvindex")

### Calcule et generation rapide et automatique des tables statistiques

## Periode annuelle sur la France entier ou les trois regions d’etude
# France entiere
france_quartile <- ic_table_maker(animdata, DSKdata, vectornames, calcul='quartile')
datatable(france_quartile)
export_csv_table(france_quartile)

france_decile <- ic_table_maker(animdata, DSKdata, vectornames, calcul='decile')
datatable(france_decile)
export_csv_table(france_decile)

france_centile <- ic_table_maker(animdata, DSKdata, vectornames, calcul='centile')
datatable(france_centile)
export_csv_table(france_centile)

print('Fin des calculs des tableaux d’intervals de confiance')

## Vecteurs de caracteres contenant les parametres meteo a comparer un a un
dsk_paramnames <- c("temperature", "temperaturelow", "temperaturehigh",
                          "humidity", "dewpoint", "pressure", "windspeed",
                            "visibility", "cloudcover", "windgust", 'precipintensity', 'uvindex')
mf_paramnames <- c('temperature', 'temperature_nocturne', 'temperature_diurne',
                          'humidite_floor', 'point_rose', 'press_mer', 'vvent',
                              'visibilite', 'nebulosite_floor','rafale_10min', 'precip_24h')

# Ces lignes calculent puis affichent le tableau des tests de Shapiro de normalite 
shapiro_df <- shapiro_batch(dsk_paramnames[-12], mf_paramnames)
datatable(shapiro_df)
export_csv_table(shapiro_df)

# Calcule de la table DSK vs MF au test t.test
t <- t.test_batch(dsk_paramnames[-12], mf_paramnames)
t <- t(t)
datatable(t)
export_csv_table(t)

# Calcule de la table du test DSK vs MF pour le test de Wilcoxon
w <- kwcox_table(dsk_paramnames[-12], mf_paramnames, test='wilcox')
w <- t(w)
datatable(w)
export_csv_table(w)

# Calcule de la table du test DSK vs MF pour le test de Kruskal-Wallis
k <- kwcox_table(dsk_paramnames[-12], mf_paramnames, test='kruskal')
k <- t(k)
datatable(k)
export_csv_table(k)

print('Fin des tests statistiques')

print('Début de fabrication des graphiques')

### Fabrication rapide et automatique des graphiques DSK moyennes vs MF moyennes
# ces lignes fabriquent automatiquement tous les graphs de l’article
# la fonction batch_histogram renvoi une liste contenant les graphs fabriques

g <- batch_histogram(DSKdata_42avg, MFdata, dsk_paramnames[-12], mf_paramnames)
# commande courte mais sans possibilite d’arrangement des positions
weather_gridplot_g <- plot_grid(plotlist=g, labels = "AUTO", ncol=3, nrow = 4 , align = 'hv')
title_text_g <- paste('Profils comparés de 11 paramètres climatiques pour 42 stations synoptiques de Météo-France et leur équivalent Dark Sky (France, Janvier 2017 – Avril 2020), ',nrow(DSKdata_700avg),' jours')
bkg <- ggplot()
title_g <- ggdraw(bkg) + draw_label(title_text_g, fontface='bold', size = 12, lineheight = 0.3)
# rel_heights values control title margins
weather_gridplot_g <- plot_grid(title_g, weather_gridplot_g, ncol=1, rel_heights=c(.05, 1), align = 'hv') 
plotsave(weather_gridplot_g, 'fig_3_profils_compares_11_param_clim_42_stations_synoptiques_Meteo-France_vs_dsk_print_fr.png', format='landscape', extension='png')

## Vecteurs de caracteres contenant les parametres meteo a comparer un a un  (Modifiés suite à relecture Cybergeo)
dsk_paramnames <- c("temperature", "humidity", "dewpoint", "pressure", "windspeed")

# Production automatique des grilles des graphiques des séries temporelles
f <- weatherPlotGrid('france', mode='region')
plotsave(f, 'fig_4_profils_temporels_variables_meteo_associes_aux_lieux_dates_signalements_compares_aux_semis_reguliers_print_fr.png', format='landscape', extension='png')

### Fabrication rapide et automatique des graphiques human data vs DSK moyennes semi 700 pts
h <- batch_histogram(animdata, DSKdata_700avg, dsk_paramnames, dsk_paramnames)
weather_gridplot_h <- plot_grid(plotlist=h, labels = "AUTO", ncol=2, nrow = 3 , align = 'hv')
title_text_h <- paste('Profils météorologiques associés aux ',nrow(animdata),' lieux et dates de signalements comparés à ceux des mêmes dates mais pour un semis régulier de lieux (France, Janvier 2017 – 5 Avril 2020), ',nrow(DSKdata_700avg),' jours')
title_h <- ggdraw(bkg) + draw_label(title_text_h, fontface='bold', size = 12, lineheight = 0.3)
weather_gridplot_h <- plot_grid(title_h, weather_gridplot_h, ncol=1, rel_heights=c(.05, 1), align = 'hv') 
plotsave(weather_gridplot_h, 'fig_5_profils_meteorologiques_associes_aux_lieux_dates_signalements_compares_aux_semis_regulier_des_lieux_print_fr.png', format='landscape', extension='png')



end_program = Sys.time()
print('Fin du programme')
benchmark(start, end_program)



