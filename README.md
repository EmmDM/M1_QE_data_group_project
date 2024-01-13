---
title: "Final Part"
author: "Victoire Truchon-Bartès & Emmanuel de MOREL"
format: html
echo : false
editor: 
  markdown: 
    wrap: 72
---

```{r, echo=FALSE}
# Sélection des librairies
suppressWarnings({suppressMessages(library(here))
suppressMessages(library(dplyr))
library(tidyr)
library(knitr)
library(ggplot2)
library(readr)
library(stringr)
library(readxl)})
```

```{r}
#On lit les bases de données OCDE_pays et non_OCDE_pays
donnees_partage <- readRDS("donnees_partage.RDS")
OCDE_pays <- donnees_partage$OCDE_pays
non_OCDE_pays <- donnees_partage$non_OCDE_pays
```

```{r}
#Pour les GES
#Tous les pays de l'OCDE et les trois régions principales (Europe, Amérique du Nord et Pacifique)
#Variables : PIB par habitant / Emission de GES
decoupling_CO2_OCDE <-  OCDE_pays %>% filter(Variable %in% c("PIB réel","Émissions totales de GES (kt d’équivalent CO2)","Population, total")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

#Création d'un index 
CO2_total_OCDE_index <- decoupling_CO2_OCDE %>% filter(Pays%in%unique(decoupling_CO2_OCDE$Pays)) %>% group_by(Année) %>% summarise(PIB_tot = sum(`PIB réel`, na.rm = TRUE)*1000000, Hab_tot = sum(`Population, total`,na.rm = TRUE), GES_tot = sum(`Émissions totales de GES (kt d’équivalent CO2)`, na.rm = TRUE)) %>% mutate(PIB_hab = `PIB_tot`/`Hab_tot`)

CO2_total_OCDE <- CO2_total_OCDE_index %>% group_by(Année) %>% summarise(Ratio = `GES_tot`/`PIB_hab`)
CO2_total_OCDE <- CO2_total_OCDE %>% mutate(Ratio_base100 = Ratio / Ratio[which(CO2_total_OCDE$Année == 1990)]*100)


#Tous les pays NON membres de l'OCDE 
#Variables : PIB par habitant / Emission de GES
decoupling_CO2_non_OCDE <-  non_OCDE_pays %>% filter(Variable %in% c("PIB réel","Émissions totales de GES (kt d’équivalent CO2)", "Population, total")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

#On crée un indice
CO2_total_non_OCDE_index <- decoupling_CO2_non_OCDE %>% filter(Pays%in%unique(decoupling_CO2_non_OCDE$Pays)) %>% group_by(Année) %>% summarise(PIB_tot = sum(`PIB réel`, na.rm = TRUE)*1000000, Hab_tot = sum(`Population, total`,na.rm = TRUE), GES_tot = sum(`Émissions totales de GES (kt d’équivalent CO2)`, na.rm = TRUE)) %>% mutate(PIB_hab = `PIB_tot`/`Hab_tot`)

CO2_total_non_OCDE <- CO2_total_non_OCDE_index %>% group_by(Année) %>% summarise(Ratio = `GES_tot`/`PIB_hab`) 

CO2_total_non_OCDE <- CO2_total_non_OCDE %>% mutate(Ratio_base100 = Ratio / Ratio[which(CO2_total_non_OCDE$Année == 1990)]*100)

```

# I. Constat sur la transition écologique

```{r}
#On va faire des graphs en considérant la croissance du PIB et la croissance des émissions
#On s'intéresse aux pays membres de l'OCDE
#On calcule les taux de croissance par an du PIB réel par habitant et des Émissions totales de GES (kt d’équivalent CO2)

#OCDE
decoupling_CO2_OCDE_growth <- CO2_total_OCDE_index %>% mutate(taux_PIB = ((`PIB_hab` - lag( `PIB_hab`)) *100/lag( `PIB_hab`)), taux_CO2 = ((`GES_tot` - lag( `GES_tot`))*100 /lag( `GES_tot`)))

decoupling_CO2_OCDE_growth_convert <- decoupling_CO2_OCDE_growth %>% pivot_longer(cols = c(taux_PIB,taux_CO2), names_to = "Variable", values_to = "Taux") 

ggplot(decoupling_CO2_OCDE_growth_convert, aes(x = Année, y = Taux, color = Variable, linetype = Variable, group = Variable)) + geom_point(na.rm = TRUE)  + geom_line(na.rm = TRUE) + labs(title = "Taux de croissance du PIB par habitant  \n et taux de croissance des émissions de CO2 des pays membres de l'OCDE", x = "Année", y = "Taux de croissance (%)", color = "Variable") +  theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5))

#Non OCDE
decoupling_CO2_non_OCDE_growth <- CO2_total_non_OCDE_index %>% mutate(taux_PIB = ((`PIB_hab` - lag( `PIB_hab`)) *100/lag( `PIB_hab`)), taux_CO2 = ((`GES_tot` - lag( `GES_tot`))*100 /lag( `GES_tot`)))

decoupling_CO2_non_OCDE_growth_convert <- decoupling_CO2_non_OCDE_growth %>% pivot_longer(cols = c(taux_PIB,taux_CO2), names_to = "Variable", values_to = "Taux")

# Représentation des graphiques considérant la croissance du PIB et la croissance des émissions
#On s'intéresse aux pays non membres de l'OCDE
ggplot(decoupling_CO2_non_OCDE_growth_convert, aes(x = Année, y = Taux, color = Variable, linetype = Variable, group = Variable)) + geom_point(na.rm = TRUE)  + geom_line(na.rm = TRUE) + labs(title = "Taux de croissance du PIB par habitant \n et taux de croissance des émissions de CO2 des pays non membres de l'OCDE", x = "Année", y = "Taux de croissance (%)", color = "Variable") +  theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5))  
```

```{r}
reg_CO2_PIB_OCDE=lm('taux_CO2 ~ taux_PIB', data = decoupling_CO2_OCDE_growth)
summary(reg_CO2_PIB_OCDE)
ggplot(reg_CO2_PIB_OCDE, aes(x = taux_PIB,y = taux_CO2)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm', formula = 'y ~ x', se=FALSE, color='red', na.rm = TRUE) + theme_minimal() + labs(title = "Taux de croissance du PIB par habitant et taux de croissance des émissions de CO2 des pays membres de l'OCDE", x = "Taux de croissance du PIB/hab (%)", y = "Taux de croissance des émissions de GES (%)")

reg_CO2_PIB_non_OCDE=lm('taux_CO2 ~ taux_PIB', data = decoupling_CO2_non_OCDE_growth)
summary(reg_CO2_PIB_non_OCDE)
ggplot(reg_CO2_PIB_non_OCDE, aes(x = taux_PIB,y = taux_CO2)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm',formula = 'y ~ x', se=FALSE, na.rm = TRUE) +  labs(title = "Taux de croissance du PIB par habitant et taux de croissance des émissions de CO2 des pays non membres de l'OCDE", x = "Taux de croissance du PIB/hab (%)", y = "Taux de croissance des émissions de GES (%)") + theme_minimal() 

```

## *A) Découplage*

Le découplage se produit lorsque le taux de croissance d'une pression
environnementale est inférieur à celui de sa force motrice économique
(par exemple le PIB) sur une période donnée. Le découplage peut être
absolu ou relatif. On parle de découplage absolu lorsque la variable
pertinente pour l'environnement est stable ou décroissante alors que la
force motrice économique augmente. Le découplage est dit relatif lorsque
le taux de croissance de la variable pertinente pour l'environnement est
positif, mais inférieur au taux de croissance de la variable économique.
Il existe un nombre varié d'indicateur de découplage, dans cette
présentation nous focaliserons sur les emissions totales de gaz à effet
de serre (GES) par unité de PIB et par personne. La manière la plus
directe d'afficher le découplage entre une pression environnementale et
une force économique consiste à tracer deux séries temporelles indexées
(par exemple 1980 = 100) sur le même graphique. A partir d'un tel
graphique, il est immédiatement clair si une force économique croit ou
décroit, si le découplage -absolu ou relatif- s'opère et quand il
commence et s'il continue.

Pour comparer le découplage parmi les pays, le ratio de la valeur du
découplage à la fin et au début d'une période de temps donnée est
définie comme :
$$ Ratio = {(PE/FM)_{findepériode} / (PE/FM)_{débutdepériode}} $$

avec : PE = Pression environnementale et FM = Force motrice

```{r}
#Representation graphique de l'indice de découplage entre les émissions de GES et le PIB/hab pour l'ensemble des pays membres de l'OCDE
ggplot(CO2_total_OCDE, aes(x = Année, y = Ratio_base100))  + geom_point() + labs(title = "Emissions par unité de PIB et par personne en base 100 (1990) pour le total de l'OCDE", x = "Année", y = "Ratio (Base 100 en 1990)") + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) 

#Representation graphique de l'indice de découplage entre les émissions de GES et le PIB/hab pour l'ensemble des pays NON membres de l'OCDE
ggplot(CO2_total_non_OCDE, aes(x = Année, y = Ratio_base100))  + geom_point() + labs(title = "Emissions par unité de PIB et par personne en base 100 (1990) pour le total des pays non membres de l'OCDE", x = "Année", y = "Ratio (Base 100 en 1990)") + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) 


#Fusion des deux graphiques
CO2_fusion <- bind_rows(mutate(CO2_total_OCDE, Group = "OCDE"), mutate(CO2_total_non_OCDE, Group = "Non-OCDE"))

ggplot(CO2_fusion, aes(x = Année, y = Ratio_base100, color = Group)) + geom_point() +  labs(title = "Emissions par unité de PIB et par personne en base 100 (1990)", x = "Année", y = "Ratio (Base 100 en 1990)") + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) 

```

```{r}
#Ici le but est de réaliser un diagramme en nuage de point avec en abscisse le taux de croissance du PIB en pourcentage en 2020 depuis 1990 et en ordonnées le taux de croisssance des émissions de GES en pourcentage en 2020 depuis 1990. Cela nous permettra de distinguer les pays qui ont on effectué un découplage et de de quel type de découplage il s'agit


#OCDE
decoupling_final <- decoupling_CO2_OCDE %>% group_by(Année, Pays) %>% summarise(PIB_tot = sum(`PIB réel`, na.rm = TRUE)*1000000, Hab_tot = sum(`Population, total`,na.rm = TRUE), GES_tot = sum(`Émissions totales de GES (kt d’équivalent CO2)`, na.rm = TRUE), .groups = 'drop') %>% mutate(PIB_hab = `PIB_tot`/`Hab_tot`)

decoupling_final_convert <- decoupling_final %>% filter(Pays%in%unique(decoupling_final$Pays)) %>% group_by(Pays) %>% summarise(taux_GES_90_20 = (GES_tot[Année == 2020] - GES_tot[Année == 1990])*100/ GES_tot[Année == 1990], taux_PIB_90_20 = (PIB_hab[Année == 2020] - PIB_hab[Année == 1990])*100/ PIB_hab[Année == 1990],  .groups = 'drop')

decoupling_final_convert  <- decoupling_final_convert  %>% filter(!is.infinite(taux_PIB_90_20) & !is.infinite(taux_GES_90_20))
  
graph_decoupling <- ggplot(decoupling_final_convert, aes(x = taux_PIB_90_20,y = taux_GES_90_20)) + geom_point( size=1, na.rm = TRUE)  +  labs(title = "Découplage au sein de l'OCDE", x = "Taux de croissance du PIB/hab en 2020 par rapport à 1990 (%)", y = "Taux de croissance des émissions de GES en 2020 par rapport à 1990 (%)") + theme_minimal() 

graph_decoupling + geom_abline(intercept = 0, slope = 1, color = "black") + geom_abline(intercept = 0, slope = 0, color = "black") + geom_abline(intercept = 0, slope = 0, color = "black") + geom_vline(xintercept = 0, color = "black")

#Non - OCDE
decoupling_final_non_OCDE <- decoupling_CO2_non_OCDE %>% group_by(Année, Pays) %>% summarise(PIB_tot = sum(`PIB réel`, na.rm = TRUE)*1000000, Hab_tot = sum(`Population, total`,na.rm = TRUE), GES_tot = sum(`Émissions totales de GES (kt d’équivalent CO2)`, na.rm = TRUE),  .groups = 'drop') %>% mutate(PIB_hab = `PIB_tot`/`Hab_tot`)

decoupling_final_convert_non_OCDE <- decoupling_final_non_OCDE %>% filter(Pays%in%unique(decoupling_final_non_OCDE$Pays)) %>% group_by(Pays) %>% summarise(taux_GES_90_20 = (GES_tot[Année == 2020] - GES_tot[Année == 1990])*100/ GES_tot[Année == 1990], taux_PIB_90_20 = (PIB_hab[Année == 2020] - PIB_hab[Année == 1990])*100/ PIB_hab[Année == 1990],.groups = 'drop')

decoupling_final_convert_non_OCDE  <- decoupling_final_convert_non_OCDE  %>% filter(!is.infinite(taux_PIB_90_20) & !is.infinite(taux_GES_90_20))

x_limits <- c(-70,1000)
y_limits <- c(-75,1000)
graph_decoupling_non_OCDE <- ggplot(decoupling_final_convert_non_OCDE, aes(x = taux_PIB_90_20,y = taux_GES_90_20)) + geom_point( size=1, na.rm = TRUE)  +  labs(title = "Découplage des pays non membres de l'OCDE", x = "Taux de croissance du PIB/hab en 2020 par rapport à 1990 (%)", y = "Taux de croissance des émissions de GES en 2020 par rapport à 1990 (%)") + theme_minimal() + coord_cartesian(xlim = x_limits, ylim = y_limits) 

graph_decoupling_non_OCDE + geom_abline(intercept = 0, slope = 1, color = "black") + geom_abline(intercept = 0, slope = 0, color = "black") + geom_abline(intercept = 0, slope = 0, color = "black") + geom_vline(xintercept = 0, color = "black")


```

Les pays dont le point se trouve à gauche de l'axe des ordonnées ou au
dessus de la droite d'équation y = x sont des pays où le découplage n'a
pas eu lieu. Les pays dont le point se trouve en dessous de la droite
d'équation y = x mais au dessus de l'axe des abscisse, sont des pays
ayant effectués un découplage relatif, c'est à dire dont les émissions
de GES ont cru plus lentement que le PIB Les pays dont le point est à
droite de l'axe des ordonnées et en dessous de l'axe des abscisse sont
des pays ayant effectuées un découplage absolue, c'est à dire les pays
dont les émissions de GES ont diminué tandis que le PIB a cru

```{r}
decoupling_table_OCDE <- decoupling_final_convert %>% mutate(decoupling_type = case_when(taux_GES_90_20 > taux_PIB_90_20 ~ "No decoupling", taux_GES_90_20 <= taux_PIB_90_20 & taux_GES_90_20 >= 0 ~"Relative decoupling", taux_GES_90_20 < 0 & taux_PIB_90_20 >= 0 ~ "Absolute decoupling", TRUE ~ "Other"))

decoupling_summary_OCDE <- decoupling_table_OCDE %>% group_by(decoupling_type) %>% summarise(percentage = n()/nrow(decoupling_table_OCDE)*100)

decoupling_table_non_OCDE <- decoupling_final_convert_non_OCDE %>% mutate(decoupling_type = case_when(taux_GES_90_20 > taux_PIB_90_20 ~ "No decoupling", taux_GES_90_20 <= taux_PIB_90_20 & taux_GES_90_20 >= 0 ~"Relative decoupling", taux_GES_90_20 < 0 & taux_PIB_90_20 >= 0 ~ "Absolute decoupling", TRUE ~ "Other"))

decoupling_summary_non_OCDE <- decoupling_table_non_OCDE %>% group_by(decoupling_type) %>% summarise(percentage = n()/nrow(decoupling_table_non_OCDE)*100)


knitr :: kable(decoupling_summary_OCDE, caption = "Part des pays membres de l'OCDE d'après le découplage effectué")

knitr :: kable(decoupling_summary_non_OCDE, caption = "Part des pays non membres de l'OCDE d'après le découplage effectué")
```

I.2 Nous analyserons aussi les tendances en matière d'intensité carbone
et d'efficacité énergétique.

A. Courbe environnementale de Kuznets "Selon l'économiste Éloi Laurent,
spécialiste de la croissance, la source de l'idée de découplage repose
sur la courbe environnementale de Kuznets, théorie construite dans les
années 1990 selon laquelle les dommages environnementaux augmentent dans
les premières phases du développement d'une société, puis reculent alors
que la croissance économique se poursuit" (Wikipedia, découplage
(écologie))

La courbe de Kuznets, dont la forme est U inversé, représente
initiallement l'inégalité économique dans un pays en fonction de son
niveau de développement. C'est selon Grossman et Krueger (1994), que
cette forme de courbe peut être observée dans le domaine de
l'environnement.Le modèle de courbe environnementale de Kuznets (EKC)
postule une relation déterministe entre développement économique et
qualité de l'environnement. Dans la première étape de
l'industrialisation, la pollution augmente rapidement car les individus
sont principalement interessées par leur travail et leur revenus que par
la qualité de l'air et de l'eau. A ce stade, la société est trop pauvre
pour réduire ses émissions, de fait la réglementations environnementales
est trop failbe. Le retournement s'effectue à mesure que les revenus
augmentent. Dès lors, les principaux secteurs industriels deviennent
plus propres, les individus valorisent l'environnement et les
institutions de régulation deviennent plus efficaces. Le long de la
courbe, la pollution se stabilise dans la tranche des revenus moyens,
puis retombe au niveau pré-industriel dans les sociétés développées.

```{r}
#CKE 1990
#En abscisse le PIB par tête et en ordonnées le GES par tête
#PIB_hab, GES_hab
#Fusion OCDE, non_OCDE, group
#ggplot
CKE_1990 <- bind_rows(mutate(decoupling_final, Group = "OCDE"),mutate(decoupling_final_non_OCDE, Group = "Non-OCDE"))
CKE_1990 <- CKE_1990 %>% group_by(Année) %>% mutate(GES_hab = `GES_tot` / `Hab_tot`)
CKE_1990 <- CKE_1990 %>% filter(Pays%in%unique(CKE_1990$Pays)) %>% group_by(Pays, Group) %>% summarise(PIB_hab = PIB_hab[Année == 1990], GES_hab = GES_hab[Année == 1990], .groups = 'drop')

ggplot(CKE_1990, aes(x = PIB_hab, y = GES_hab, color = Group)) + geom_point(na.rm = TRUE) + geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE, color = 'darkorange') + labs(title = "Courbe de Kuznets environnementale en 1990", x = "PIB par habitant", y = "Emmission de GES")


#CKE 2020
CKE_2020 <- bind_rows(mutate(decoupling_final, Group = "OCDE"),mutate(decoupling_final_non_OCDE, Group = "Non-OCDE"))
CKE_2020 <- CKE_2020 %>% group_by(Année) %>% mutate(GES_hab = `GES_tot` / `Hab_tot`)
CKE_2020 <- CKE_2020 %>% filter(Pays%in%unique(CKE_2020$Pays)) %>% group_by(Pays, Group) %>% summarise(PIB_hab = PIB_hab[Année == 2020], GES_hab = GES_hab[Année == 2020], .groups = 'drop')

ggplot(CKE_2020, aes(x = PIB_hab, y = GES_hab, color = Group)) + geom_point(na.rm = TRUE) + geom_smooth(method = 'lm', formula = y ~ poly(x, 2), se = FALSE, color = 'darkorange') + labs(title = "Courbe de Kuznets environnementale en 2020", x = "PIB par habitant", y = "Emmission de GES")



```

En utilisant les données des pays pour les emissions de GES et les PIB
par habitant, nous constatons qu'il n'y a pas de courbe en U inversée,
ni en 1990, ni en 2020. En effet, à mesure que le PIB est élevé, les
émissions de GES devraient théoriquement diminuées dans les pays
développées, or ce n'est pas ce qui est observées empiriquement, bien
qu'en 2020 il semble y avoir un seuil aux émissions de GES qui n'étaient
pas constaté en 1990. Toutefois, l'année 2020 est aussi une année
particulière en raison de la pandémie de COVID-19 qui a drastiquement
diminué les émissions de CO2, comme le montre l'un des graphiques
précédent.

I.3 Identité de Kaya

"L'identité de Kaya, relie les émissions anthropique de dioxyde de
carbone à des paramètre d'ordre démographique, économique et
énergétique. \[...\] Selon Kaya, le niveau total d'émission peut
s'exprimer comme le produit de quatres facteurs : la population, le PIB
par habitant, l'intensité énergétique et le contenu en CO2 de l'énergie
consommée. Cette identité se décompose par : CO2 = POP \* (PIB/POP) \*
(E/PIB) \* (CO2/E) Avec : CO2 : Emission de CO2 POP : la population PIB
: Produit interieur brut E : consommation d'énergie PIB/POP : Produit
interieur brut par habitant E/PIB : Intensité énergétique du PIB CO2/E :
Intensité carbone de l'énergie

```{r}
#Identité de Kaya pour l'ensemble des pays membres de l'OCDE
Kaya_OCDE <-  OCDE_pays %>% filter(Variable %in% c("Émissions de CO2 (kt)","Population, total","PIB réel","Approvisionnements totaux en énergie")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

Kaya_OCDE <- Kaya_OCDE %>%  group_by(Année) %>% summarise(PIB_tot = sum(`PIB réel`, na.rm = TRUE)*1000000, Hab_tot = sum(`Population, total`,na.rm = TRUE), CO2_tot = sum(`Émissions de CO2 (kt)`, na.rm = TRUE), E_tot = sum(`Approvisionnements totaux en énergie`, na.rm = TRUE)) %>% mutate(PIB_hab = `PIB_tot`/`Hab_tot`, IEP_tot = `E_tot`/`PIB_tot`, ICE_tot = `CO2_tot`/`E_tot`)

Kaya_OCDE_growth <- Kaya_OCDE %>% mutate(taux_PIB_hab = ((`PIB_hab` - lag( `PIB_hab`))*100/lag( `PIB_hab`)),taux_CO2 = ((`CO2_tot` - lag( `CO2_tot`))*100 /lag( `CO2_tot`)), taux_POP = ((`Hab_tot` - lag(`Hab_tot`))*100 / `Hab_tot`), taux_IEP = ((`IEP_tot` - lag( `IEP_tot`))*100 /lag( `IEP_tot`)), taux_ICE = ((`ICE_tot` - lag( `ICE_tot`))*100 /lag( `ICE_tot`)))

ggplot(Kaya_OCDE_growth, aes(x = Année)) + geom_bar(aes(y = taux_PIB_hab, fill = "Taux PIB par habitant"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_bar(aes(y = taux_POP, fill = "Taux population"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_bar(aes(y = taux_IEP, fill = "Taux de l'intensité énergétique du PIB"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_bar(aes(y = taux_ICE, fill = "Taux de l'intensité carbone de l'énergie"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_point(aes(y = taux_CO2), color = 'black', size = 2, na.rm = TRUE) + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) +  labs(title = "Identité de Kaya des pays membres de l'OCDE", x = "Année", y = "Taux (%)") + geom_abline(intercept = 0, slope = 0, color = "black") 



#Identité de Kaya pour l'ensemble des pays non membres de l'OCDE
Kaya_non_OCDE <-  non_OCDE_pays %>% filter(Variable %in% c("Émissions de CO2 (kt)","Population, total","PIB réel","Approvisionnements totaux en énergie")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

Kaya_non_OCDE <- Kaya_non_OCDE %>%  group_by(Année) %>% summarise(PIB_tot = sum(`PIB réel`, na.rm = TRUE)*1000000, Hab_tot = sum(`Population, total`,na.rm = TRUE), CO2_tot = sum(`Émissions de CO2 (kt)`, na.rm = TRUE), E_tot = sum(`Approvisionnements totaux en énergie`, na.rm = TRUE)) %>% mutate(PIB_hab = `PIB_tot`/`Hab_tot`, IEP_tot = `E_tot`/`PIB_tot`, ICE_tot = `CO2_tot`/`E_tot`)

Kaya_non_OCDE_growth <- Kaya_non_OCDE %>% mutate(taux_PIB_hab = ((`PIB_hab` - lag( `PIB_hab`))*100/lag( `PIB_hab`)),taux_CO2 = ((`CO2_tot` - lag( `CO2_tot`))*100 /lag( `CO2_tot`)), taux_POP = ((`Hab_tot` - lag(`Hab_tot`))*100 / `Hab_tot`), taux_IEP = ((`IEP_tot` - lag( `IEP_tot`))*100 /lag( `IEP_tot`)), taux_ICE = ((`ICE_tot` - lag( `ICE_tot`))*100 /lag( `ICE_tot`)))

ggplot(Kaya_non_OCDE_growth, aes(x = Année)) + geom_bar(aes(y = taux_PIB_hab, fill = "Taux PIB par habitant"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_bar(aes(y = taux_POP, fill = "Taux population"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_bar(aes(y = taux_IEP, fill = "Taux de l'intensité énergétique du PIB"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_bar(aes(y = taux_ICE, fill = "Taux de l'intensité carbone de l'énergie"), stat ="identity", position = "dodge", na.rm = TRUE) + geom_point(aes(y = taux_CO2), color = 'black', size = 2, na.rm = TRUE) + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) +  labs(title = "Identité de Kaya des pays non membres de l'OCDE", x = "Année", y = "Taux (%)") + geom_abline(intercept = 0, slope = 0, color = "black") 


```

```{r}
#Kaya Table
Kaya_table_OCDE <- Kaya_OCDE_growth %>% select(Année,taux_PIB_hab,taux_CO2,taux_POP,taux_ICE,taux_IEP)

Kaya_OCDE_growth <- Kaya_OCDE %>% mutate(taux_PIB_hab = ((`PIB_hab` - lag( `PIB_hab`))*100/lag( `PIB_hab`)),taux_CO2 = ((`CO2_tot` - lag( `CO2_tot`))*100 /lag( `CO2_tot`)), taux_POP = ((`Hab_tot` - lag(`Hab_tot`))*100 / `Hab_tot`), taux_IEP = ((`IEP_tot` - lag( `IEP_tot`))*100 /lag( `IEP_tot`)), taux_ICE = ((`ICE_tot` - lag( `ICE_tot`))*100 /lag( `ICE_tot`)))
```


*II- Politiques*

Dans cette deuxième partie, l'objectif est d'étudier les pressions
environnementales sur les ressources exercées par les composantes du
PIb, puis d'analyser les politiques mises en place pour réduire cela.

**A- Pression environementales** 
*A.1 : Intensité de l'eau*
Il nous paraît interessant dans un premier temps de comparer entre les
pays OCDE et hors OCDE les differentes intensité sur l'eau, et la qualité
de l'air.
Afin d'évaluer l'utilisation durable de l'eau, nous allons examiner
l'intensité de l'eau par habitant et par unité de production. Nos
variables principales utilisés sont : Prélèvements bruts d'eau douce par
habitant, Stress hydrique (total des prélèvements d'eau douce en % des
ressources internes renouvelables),Retraits annuels d'eau douce, total
(% des ressources internes), Retraits annuels d'eau douce, (total
milliards de mètres cubes). Nous les avons choisies car elles permettent
d'avoir un aperçu des impacts et des utilisations intensives sur l'eau,
une ressource qui va venir à manquer. Il va aussi être interessant de se
rendre compte des différents accès à l'eau selon les régions du monde,
car 1 personne sur 3 n'y a pas accès créant un nombre élévé de dèces du
à ce manque important.

```{r}

tab_var<- c("Prélèvements bruts d'eau douce par habitant",
    "Stress hydrique, total des prélèvements d'eau douce en % des ressources internes renouvelables",
    "Retraits annuels d’eau douce, total (% des ressources internes)",
    "Retraits annuels d’eau douce, total (milliards de mètres cubes)")

 

#  On calcule la moyenne et la médiane des variables pour chaques pays membres de l'OCDE
eau_OCDE <- OCDE_pays %>% filter(Variable %in% c("Prélèvements bruts d'eau douce par habitant","Stress hydrique, total des prélèvements d'eau douce en % des ressources internes renouvelables","Retraits annuels d’eau douce, total (% des ressources internes)","Retraits annuels d’eau douce, total (milliards de mètres cubes)")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`)%>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value))

eau_stat_OCDE <- eau_OCDE %>% group_by(Pays,Variable) %>% summarize(
  Valeur_moyenne = mean(`Value`, na.rm = TRUE), 
  valeur_median = median(`Value`, na.rm = TRUE), 
  valeur_min = min(`Value`, na.rm = TRUE), 
  valeur_max = max(`Value`, na.rm = TRUE), 
  .groups = 'drop')
tab_var_10_colonnes <- head(tab_var[1:10])
knitr :: kable(eau_stat_OCDE)



#Non OCDE

tab_var_non_OCDE<- c("Prélèvements bruts d'eau douce par habitant",
    "Stress hydrique, total des prélèvements d'eau douce en % des ressources internes renouvelables",
    "Retraits annuels d’eau douce, total (% des ressources internes)",
    "Retraits annuels d’eau douce, total (milliards de mètres cubes)")

 

#  On calcule la moyenne et la médiane des variables pour chaques pays membres de l'OCDE
eau_non_OCDE <- non_OCDE_pays %>% filter(Variable %in% c("Prélèvements bruts d'eau douce par habitant","Stress hydrique, total des prélèvements d'eau douce en % des ressources internes renouvelables","Retraits annuels d’eau douce, total (% des ressources internes)","Retraits annuels d’eau douce, total (milliards de mètres cubes)")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`)%>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value))

eau_stat_non_OCDE <- eau_non_OCDE %>% group_by(Pays,Variable) %>% summarize(
  Valeur_moyenne = mean(`Value`, na.rm = TRUE), 
  valeur_median = median(`Value`, na.rm = TRUE), 
  valeur_min = min(`Value`, na.rm = TRUE), 
  valeur_max = max(`Value`, na.rm = TRUE), 
  .groups = 'drop')
tab_var_non_OCDE_10_colonnes <- head(tab_var_non_OCDE[1:10])
knitr :: kable(eau_stat_non_OCDE)




```

Voici un tableau récapitulatif pour ces 4 variables qui permettent d'avoir une vue d'ensemble à l'aide d'outils statistics. Les disparités sont énorment entre tous ces pays, en effet prenons par exemple le Brésil pour le retrait annuel d'eau douce par habitant en pourcentage des resssources internes qui à un maximum de 1,32 % contrairement au Japon qui est à plus de 182% .La situation Géographique est aussi à prendre en compte, la surface océanique est plus élévée au sud qu'au Nord du globe. De plus, les précipitations et les variations des rayonnement solaires sont aussi des variables qui sont à l'origine de ces inégalités.Certains pays auront donc plus tendance à exercée une pression plus intense sur leurs ressources en eau.

```{r}
#On analyse les tendances des pays membres de l'OCDE

#On effectue la moyenne par an de tous les pays membres de l'OCDE
eau_tendance_OCDE <- eau_OCDE %>% group_by(Année,Variable) %>% summarise(moyenne_annuelle = mean(`Value`, na.rm =TRUE), .groups = 'drop')

ggplot(eau_tendance_OCDE, aes(x=`Année`, y = moyenne_annuelle)) + geom_jitter() + scale_size_continuous("moyenne_annuelle") +  facet_wrap(~ Variable, scales = "free_y", ncol = 1) + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) 


#On analyse les tendances des pays non membres de l'OCDE
#On effectue la moyenne par an de tous les pays membres de l'OCDE
eau_tendance_non_OCDE <- eau_non_OCDE %>% group_by(Année,Variable) %>% summarise(moyenne_annuelle = mean(`Value`, na.rm =TRUE), .groups = 'drop')

ggplot(eau_tendance_non_OCDE, aes(x=`Année`, y = moyenne_annuelle)) + geom_jitter(na.rm = TRUE) + scale_size_continuous("moyenne_annuelle") +  facet_wrap(~ Variable, scales = "free_y", ncol = 1) + theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) 


```

Nous pouvons observer par ces 4 tableaux, que pour chacune des variables, la moyenne annuelle était plus elevés en 1990 qu'en 2020. Pour la variable : retraits annuel d'eau douce relié au pourcentage des ressources internes , sa croissance est plutot homogene avec celle qui mesure en milliard de mètre cubes, cela est cohérent. Cependant, la demande d'eau ne cesse d'augmenter,le secteur agricole qui est le principal consommateur avec 70% de la demande d'eau douce (OCDE), puis la production d'éléctricité.
D'après le scénario de reference des Perspectives à horizon 2050, 40% de la population mondiale vivront dans des bassins soumis à un stress hydrique elevé qui correspond à plus de 40% de demande d'eau dites bleues ( eau douces des aquiferes, cours d'eau et lacs). 
Ces indicateurs ne refletent pas la réalité complexe. La diminution de la moyenne annuelle transcrit le fait que les pays de l'OCDE developpés ont finis leurs croissance exponentielles et ont souvent recours à la délocalisation. Cependant les BRIICS et les autres pays emergeants devrait voir leurs consommation augmenter fortement pour le secteur manufacturier, la production d'éléctricité et les besoins domestiques. On peut apercevoir cet écart de croissance avec le premier tableau qui montre une décroissance des prélevements bruts d'eau douce par habitant en dessous de 400 contrairement aux pays hors OCDE qui voient leurs prélevement croître depuis 2006. 
La pression excercé sur les ressources en eau n'ont donc pas finis leurs croissance et devraut même encore augmenter selon les régions du monde. 

*A.2 : Qualité de l'air* L'indice de qualité de l'air sera utilisé pour évaluer les niveaux de pollution atmoshpériques.Aujourd'hui, les émissions de CO2 et autres Gazs à effets de serre (HFC, PFC et SF6) sont concentrées dans les millieux surtout urbains et supérieurs aux règles fixées. Cette surexposition entraîne la hausse du nombre de dècés par leurs contenance de produits toxiques qui penetrent dans les poumons. En effet le nombre de décès est passé de près d'un million en 2000 à plus de 3.5 millions d'ici 2050, avec une majorité de perte au sein des BRIICS. 


```{r}
CO2_emission_OCDE <-  OCDE_pays %>% filter(Variable %in% c("Émissions de CO2 (kt)","Population, total","Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)")) %>% select(-COU, -VAR ,- Unit, -PowerCode) %>%pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 
  

#Création d'un index CO2

CO2_emission_OCDE_index <- CO2_emission_OCDE %>% filter(Pays%in%unique(CO2_emission_OCDE$Pays)) %>% group_by(Année) %>%
summarise(CO2_emi_tot = sum(`Émissions de CO2 (kt)`, na.rm = TRUE), Hab_tot = sum(`Population, total`,na.rm = TRUE), Autre_CO2 = sum(`Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)`, na.rm = TRUE)) %>% mutate(CO2_hab = `CO2_emi_tot`/`Hab_tot`)



CO2_emission_habitant_OCDE <- CO2_emission_OCDE_index %>% group_by(Année) %>%summarise(Ratio = `CO2_emi_tot`/`Hab_tot` )
CO2_emission_habitant_OCDE <- CO2_emission_habitant_OCDE  %>% mutate(Ratio_base100 = Ratio / Ratio[which(CO2_emission_habitant_OCDE$Année == 1990)]*100)
  

#Création index GEs


Ges_emission_OCDE_index <- CO2_emission_OCDE %>% filter(Pays%in%unique(CO2_emission_OCDE$Pays)) %>% group_by(Année) %>%
summarise(Ges_emi_tot = sum(`Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)`, na.rm = TRUE), Hab_tot = sum(`Population, total`,na.rm = TRUE), Autre_CO2 = sum(`Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)`, na.rm = TRUE)) %>% mutate(Ges_hab = `Ges_emi_tot`/`Hab_tot`)
         

Ges_emission_habitant_OCDE <-Ges_emission_OCDE_index  %>% group_by(Année) %>%summarise(Ratio = `Ges_emi_tot`/`Hab_tot` )
Ges_emission_habitant_OCDE <- Ges_emission_habitant_OCDE  %>% mutate(Ratio_base100 = Ratio / Ratio[which(Ges_emission_habitant_OCDE$Année == 1990)]*100)
  

# Fusion des deux bases de données
merged_data_Ges_CO2_OCDE <- merge(CO2_emission_habitant_OCDE, Ges_emission_habitant_OCDE, by = "Année", suffixes = c("_Ratio", "_Autres"))



# Création du graphique
ggplot(merged_data_Ges_CO2_OCDE, aes(x = Année)) +
  geom_point(aes(y = Ratio_base100_Ratio, color = "CO2 émission par habitant")) +
  geom_point(aes(y = Ratio_base100_Autres, color = "GES émission par habitant")) +
  labs(
    title = "Emissions CO2 et autres GES par personne en base 100 (1990) \n pour le total des pays membres de l'OCDE",
    x = "Année",
    y = "Ratio (Base 100 en 1990)"
  ) +
  theme(axis.text.x = element_text(angle = 50, size = 8, vjust = 0.5)) +
  scale_color_manual(
    values = c("CO2 émission par habitant" = "blue", "GES émission par habitant" = "red"),
    labels = c("CO2 émission par habitant", "GES émission par habitant")
  ) +
  guides(color = guide_legend(title = NULL))




#Non OCDE


CO2_emission_non_OCDE <-  non_OCDE_pays %>% filter(Variable %in% c("Émissions de CO2 (kt)","Population, total","Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)")) %>% select(-COU, -VAR ,- Unit, -PowerCode) %>%pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 
  

#Création d'un index CO2


Ges_emission_non_OCDE_index <- CO2_emission_non_OCDE %>% filter(Pays%in%unique(CO2_emission_non_OCDE$Pays)) %>% group_by(Année) %>%
summarise(Ges_emi_tot = sum(`Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)`, na.rm = TRUE), Hab_tot = sum(`Population, total`,na.rm = TRUE), Autre_CO2 = sum(`Autres émissions de gaz à effet de serre, HFC, PFC et SF6 (milliers de tonnes métriques d’équivalent CO2)`, na.rm = TRUE)) %>% mutate(Ges_hab = `Ges_emi_tot`/`Hab_tot`)
         

Ges_emission_habitant_non_OCDE <-Ges_emission_non_OCDE_index  %>% group_by(Année) %>%summarise(Ratio = `Ges_emi_tot`/`Hab_tot` )
Ges_emission_habitant_non_OCDE <- Ges_emission_habitant_non_OCDE  %>% mutate(Ratio_base100 = Ratio / Ratio[which(Ges_emission_habitant_non_OCDE$Année == 1990)]*100)
  

# Fusion des deux bases de données


# Création du graphique
ggplot(Ges_emission_habitant_non_OCDE, aes(x = Année, y = Ratio_base100)) + geom_point()+
  labs( title = "Emissions GES ( HFC, PFC et SF6 ) par personne en base 100 (1990)\n  pour le total des pays non membres de l'OCDE",
    x = "Année",
    y = "Ratio (Base 100 en 1990)") +
  theme(axis.text.x = element_text(angle = 50, size = 8, vjust = 0.5))

```

Les Emissions de CO2 sont largement au dessus des autres Gaz à effets de serres depuis 1990 et ne conaissent qu'une timide diminution depuis 2010 pour les pays de l'OCDE. Cette pollution atmoshpérique à de nombreux impacts, tout d'abord sur notre santé et sur le réchauffement climatique. Selon le GIEC si nous voulons limiter le réchauffement à 1,5 degrés ou 2 il faut que les émissions cessent rapidement. Pour cela, tous les secteurs doivent réaliser leurs transition énergétique simultanément et arrêter de soutenir la production et la consommation de combustibles fossiles.
Pour les pays hors de l'OCDE nous avons seulement des informations sur leurs émissions autres que CO2.Comme expliqué plus haut, la diminution des GES pour les pays de l'OCDE provient de la délocalisation des activitées polluantes et surconsommatrices. C'est pourquoi dans les années 1995 une forte augmentation pour ces pays est marqué par une forte diminution pour les pays de l' OCDE. 
En plus de connaître des émissions de GES plus éleves certains pays comme les BRIICS n'ont pas le même cadre sanitaire que les pays qui délocalisent. La pollution atmosphérique est particulièrement répandue dans l'industrialisation des pays en développements.Nous allons maintenant analyser le lien de corrélation entre le taux de mortalité et la croissance de la population urbaine.

```{r}

mortalité_PM25_pop_OCDE <-  OCDE_pays %>% filter(Variable %in% c("Croissance de la population (% annuel)","Mortalité due à l'exposition aux PM2.5","Croissance de la population urbaine (% annuel)")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

mortalité_PM25_pop_OCDE <- mortalité_PM25_pop_OCDE %>% mutate(taux_mortalite_PM25 = (`Mortalité due à l'exposition aux PM2.5`- lag(`Mortalité due à l'exposition aux PM2.5`))*100/lag(`Mortalité due à l'exposition aux PM2.5`))

reg_mortalite_PM25_OCDE=lm('taux_mortalite_PM25 ~ `Croissance de la population urbaine (% annuel)`', data = mortalité_PM25_pop_OCDE)
summary(reg_mortalite_PM25_OCDE)
ggplot(reg_mortalite_PM25_OCDE, aes(x = `Croissance de la population urbaine (% annuel)`,y = taux_mortalite_PM25)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm', formula = 'y ~ x', se=FALSE, color='red', na.rm = TRUE) + theme_minimal() + labs(title = "Correlation entre le taux de mortalité due à l'exposition au PM2.5 \n par rapport au taux de croissance de la population urbaine (%) ", x = "Croissance de la population urbaine (% annuel)", y = "taux de Mortalité due à l'exposition aux PM2.5")




#non OCDE
mortalité_PM25_pop_non_OCDE <-  non_OCDE_pays %>% filter(Variable %in% c("Croissance de la population (% annuel)","Mortalité due à l'exposition aux PM2.5","Croissance de la population urbaine (% annuel)")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

mortalité_PM25_pop_non_OCDE_taux <- mortalité_PM25_pop_non_OCDE %>% mutate(taux_mortalite_PM25_non_OCDE = (`Mortalité due à l'exposition aux PM2.5`- lag(`Mortalité due à l'exposition aux PM2.5`))*100/lag(`Mortalité due à l'exposition aux PM2.5`))

reg_mortalite_PM25_non_OCDE_taux=lm('taux_mortalite_PM25_non_OCDE ~ `Croissance de la population urbaine (% annuel)`', data = mortalité_PM25_pop_non_OCDE_taux)
summary(reg_mortalite_PM25_non_OCDE_taux)
ggplot(reg_mortalite_PM25_non_OCDE_taux, aes(x = `Croissance de la population urbaine (% annuel)`,y = taux_mortalite_PM25_non_OCDE)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm', formula = 'y ~ x', se=FALSE, color='red', na.rm = TRUE) + theme_minimal() + labs(title = "Correlation entre le taux de mortalité due à l'exposition au PM2.5 \n par rapport au taux de croissance de la population urbaine (%) ", x = "Croissance de la population urbaine (% annuel)", y = "taux de Mortalité due à l'exposition aux PM2.5")


```

#Le résultat de Cette régression linéaire pour les pays de l'OCDE nous indique une relation statistiquement signifiante entre nos deux variables. De manière plus concrète, pour une augmentation unitaire du taux de croissance de la population urbaine, le taux de mortalité est attendu d'augmenter de 0,2203 unités. La R-squared de 0,003925 nous informe que cette augmentation attendu du taux de mortalité par la croissance de la population explique seulement une petite proportion de la variabilité de cette dernière. 


Dans une même logique, pour les pays hors OCDE, les deux variables semblent avoir une corrélation statistiquement signicative. 
Pour une augmentation unitaire du taux de croissance, le taux de mortalité induit par le PM2.5 est censé diminuer de 0,04564.
Cependant avec une R-Squared encore plus faible que pour l'OCDE, la croissance de population urbaine n'explique qu'une partie infime du taux de mortalité du au PM2.5. 
Cette différence peut s'expliquer par le fait que les pays pauvres vivent dans des conditions sanitaires dramatiques et l'exposition aux particules n'est qu'un facteur parmis tant d'autres. 
Malgré cela,la pollution de l'air reste la cause première environnementale de mort prématurée. L'exposition à des particules fines favorise le développement de maladies respiratoires et cancers. Cela induit également une augmentation des dépenses médicales. Un vrai fossé existe entre la facilité d'accès aux soins selon les pays.

*B- Politiques Environnementales* Après avoir pu observer les pressions
exercés par les composantes de notre activité économique, nous allons
pouvoir nous interesser aux politiques mises en place pour les réguler.

*B.1* tax 
Une taxe environnementale est caractérisée par son action sur
les coûts des activités ou les prix des produits qui ont un effet
négatif sur l'environnement. C'est une internalisation de l'externalité négative.La taxe carbone est la plus utilisé, elle taxe les émissions de dioxyde de carbone, le gaz à effet de serre qui contribue majoritairement à la deterioration du capital naturel. 


```{r}

library(ggplot2)
library(tidyr)

#pour OCDE
OCDE_pays_Taxes <- OCDE_pays %>%
  filter(Variable == "Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

#Non OCDE
non_OCDE_pays_Taxes <- non_OCDE_pays%>%
  filter(Variable == "Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

#Merged data 

merged_data <- bind_rows(OCDE_pays_Taxes, non_OCDE_pays_Taxes)

ggplot(merged_data, aes(x = as.numeric(Année), y = Value, color = Groupe)) +
  geom_smooth(method = "loess", se = FALSE, na.rm = TRUE)+
  labs(title = "Recettes provenant des taxes liées à l'énergie, % recettes \n des taxes environnementales",
       x = "Année",
       y = "Recettes en %") +
  theme_minimal()
```



#Voici un graph presentant les Recettes provenant des taxes liées à l'énergie pour les pays OCDE et ceux hors OCDE.Nous pouvons observer que le taux des recettes des taxes pour le groupe OCDE est largement supérieur à ceux hors OCDE avec un taux plutot constant autour des 70%. Contrairement les pays hors OCDE ont connu une croissance asymptomatique depuis les années 1990 avec un pic entre 2010 et 2015 à plus de 50%. Les recettes beaucoup plus éléves pour les pays de l'OCDE permettent des mesures plus importantes.
Les taxes representent une mesure efficace à moindre coût, les pays dans l'OCDE souvent plus developpés que la moyenne hors OCDE ont commencés à se doter de cadres d'actions nationaux afin de préserver l'environnement.Ces mesures sont necessaire afin de pénaliser les technologies nocives et d'encourager le developpement de celles plus durable. L'innovation technologique via l'investissement en R&D est un élement essentiel de la transition vers une économie plus sobre.Cependant, de nombreux problèmes bloquent l'innovation, notamment la compétitivité accrue entre les entreprises. 


Il paraît interessant d'analyser un potentiel lien de corrrélation entre le développement des technologies liées à l'environement et les recettes provenent des taxes liées à l'énergie. 

```{r}
Dévelop_recette_OCDE <-  OCDE_pays %>% filter(Variable %in% c("Développement de technologies liées à l'environnement, % inventions dans le monde", "Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 


reg_Dévelop_recette_OCDE <- lm(`Développement de technologies liées à l'environnement, % inventions dans le monde` ~ `Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales`, data = Dévelop_recette_OCDE)

summary(reg_Dévelop_recette_OCDE)


ggplot(reg_Dévelop_recette_OCDE, aes(x = `Développement de technologies liées à l'environnement, % inventions dans le monde`,y = `Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales`)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm', formula = 'y ~ x', se=FALSE, color='red', na.rm = TRUE) + theme_minimal() + labs(title = "Correlation entre le développement de technologies liées à l'environnement \n et les recettes provenant des taxes liées à l'énergie - OCDE", x = "Développement de technologies liées à l'environnement, % inventions dans le monde", y = "Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales")



#Non OCDE


Dévelop_recette_non_OCDE <-  non_OCDE_pays %>% filter(Variable %in% c("Développement de technologies liées à l'environnement, % inventions dans le monde", "Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

reg_Dévelop_recette_non_OCDE <- lm(`Développement de technologies liées à l'environnement, % inventions dans le monde` ~ `Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales`, data = Dévelop_recette_non_OCDE)

summary(reg_Dévelop_recette_non_OCDE)


ggplot(reg_Dévelop_recette_non_OCDE, aes(x = `Développement de technologies liées à l'environnement, % inventions dans le monde`,y = `Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales`)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm', formula = 'y ~ x', se=FALSE, color='red', na.rm = TRUE) + theme_minimal() + labs(title = "Correlation entre le développement de technologies liées à l'environnement \n et les recettes provenant des taxes liées à l'énergie - non OCDE", x = "Développement de technologies liées à l'environnement, % inventions dans le monde", y = "Recettes provenant des taxes liées à l'énergie, % recettes des taxes environnementales")
                                     

```

Pour les deux groupes (OCDE, hors OCDE) la relation n'est pas statistiquement significative et est même négative. Une augmentation unitaire des recettes provenant des taxes conduirais à une diminution du développement des technologies liées à l'environnement. 
Il ne semble pas y avoir de relation linéaire forte entre ces deux variables. 
Les taxes peuvent avoir un impact significatif sur l'incitation à l'innovation indispensable pour réaliser une transition durable. Des travaux de l'OCDE ont montré que le coût de cette transition serais réduit de moitié si des efforts et des moyens conséquent sont mis au service de la R&D. L'investissement public est aussi un facteur essentiel dans ce secteur risqué et la coopération international notamment pour les pays en voie de développement est nécessaire afin de ne pas accentuer l'écart deja bien ancré. 

*B.2* Subventions + prise de décision de protection de l'environnement
#Le dispositif de subvention est une aide aux entreprises afin de les aider dans leurs transitions écologique pour aller vers des énergies plus durables. Cependant certaines subvention au contraire retarde la transition. 
# Nous allons pouvoir observer l'évolution des subventions via plusieurs variables.

```{r}
library(ggplot2)
library(tidyr)

#pour OCDE
OCDE_pays_subvention <- OCDE_pays %>%
  filter(Variable == "Soutien total aux combustibles fossiles, % recettes totales des taxes" ) %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

#Non OCDE
non_OCDE_pays_subvention <- non_OCDE_pays%>%
  filter(Variable == "Soutien total aux combustibles fossiles, % recettes totales des taxes") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

#Merged data 

merged_data_subvention <- bind_rows(OCDE_pays_subvention, non_OCDE_pays_subvention)

ggplot(merged_data_subvention, aes(x = as.numeric(Année), y = Value, color = Groupe)) +
  geom_smooth(method = "loess", se = FALSE, na.rm = TRUE)+
  labs(title = "Soutien total aux combustibles fossiles, % recettes totales des taxes",
       x = "Année",
       y = "Recettes en %") +
  theme_minimal()


# Nouveau graph pour les subvention pour les consommateur de combustibles fossiles


#pour OCDE
OCDE_pays_subvention_consommateur <- OCDE_pays %>%
  filter(Variable == "Soutien aux consommateurs de combustibles fossiles, % recettes totales des taxes") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

#Non OCDE
non_OCDE_pays_subvention_consommateur <- non_OCDE_pays%>%
  filter(Variable == "Soutien aux consommateurs de combustibles fossiles, % recettes totales des taxes") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

#Merged data 

merged_data_subvention_consommateur <- bind_rows(OCDE_pays_subvention_consommateur, non_OCDE_pays_subvention_consommateur)

ggplot(merged_data_subvention_consommateur, aes(x = as.numeric(Année), y = Value, color = Groupe)) +
  geom_smooth(method = "loess", se = FALSE, na.rm = TRUE)+
  labs(title = "Soutien aux consommateurs de combustibles fossiles, % recettes totales des taxes",
       x = "Année",
       y = "Recettes en %") +
  theme_minimal()




#Nouveau graph pour Soutien à l'électricité, % soutien total aux combustibles fossiles


#pour OCDE
OCDE_pays_subvention_éléctricité <- OCDE_pays %>%
  filter(Variable == "Soutien à l'électricité, % soutien total aux combustibles fossiles") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

#Non OCDE
non_OCDE_pays_subvention_éléctricité <- non_OCDE_pays%>%
  filter(Variable == "Soutien à l'électricité, % soutien total aux combustibles fossiles") %>%
  pivot_longer(cols = c(`1990`, `1991`, `1992`, `1993`, `1994`, `1995`, `1996`, `1997`, `1998`, `1999`, `2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),
               names_to = "Année",
               values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

#Merged data 

merged_data_subvention_éléctricité <- bind_rows(OCDE_pays_subvention_éléctricité, non_OCDE_pays_subvention_éléctricité)

ggplot(merged_data_subvention_éléctricité, aes(x = as.numeric(Année), y = Value, color = Groupe)) +
  geom_smooth(method = "loess", se = FALSE, na.rm = TRUE)+
  labs(title = "Soutien à l'électricité, % soutien total aux combustibles fossiles",
       x = "Année",
       y = "Recettes en %") +
  theme_minimal()




```
Les subventions des combustibles fossiles favorisent la surconsommation et rend vain tous les efforts mis en place. Ce type de subventions sont un obstacle à l'émergence d'un avenir plus durable mais restent importantes à cause de la volatilité du prix de l'énergie. Neammoins ont peut observer un effort surtout pour les pays hors OCDE qui ont connu un pic en 2012 avec plus de 5% des recettes totales des taxes suivis d'une baisse. Les pays hors OCDE connaissent une croissance plutot stable et descendante depuis 2010 en dessous de 1%. Avec la guerre de l'Ukraine, le coût de l'énergie a connu une augmentation sans precedent ce qui devrait revoir ces graphiques à la hausse. Cependant cette hausse est ephemere et les prix reviendront petit à petit à la périod pré-guerre. 

Les subventions à l'electricité ont une croissance plutôt ascendante depuis 2017 pour les pays hors OCDE et descendante pour les pays de l'OCDE. Utilisé à 41% par les ménages nous comprennons pourquoi ces subventions sont necessités cependant elle est produite à 47,2% par des combustibles fossiles, elle est donc très polluante. 
Il est difficile de pouvoir réduire ces subventions lorsque les prix de l'énergies sont volatiles et que 760 millions de personnes n'ont pas accès à l'électricité. Il faut trouver des solutions alternatives pour compenser cela telle que l'énergie verte. Leurs suppression est tout de même un objectif pour les politiques climatiques du FMI et de la Banque Mondiale, qui les considèrent comme innéficaces et favorisant les mauvaises cibles.

Pour autant certaines mesures sont prises telles que la protection des zones terrestres et marines.

```{r}

OCDE_zone_protégée_totale <- OCDE_pays %>%
  filter(Variable == "Zones protégées à l'échelle nationale (% du territoire total)") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

non_OCDE_zone_protégée_totale <- non_OCDE_pays %>%
  filter(Variable == "Zones protégées à l'échelle nationale (% du territoire total)") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

# Merged data 
merged_data_zone_protégée <- bind_rows(OCDE_zone_protégée_totale, non_OCDE_zone_protégée_totale)

# Calcul de la moyenne par année
average_data <- merged_data_zone_protégée %>%
  group_by(Année, Groupe) %>%
  summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = 'drop')


ggplot(average_data, aes(x = as.factor(Année), y = Average_Value, fill = Groupe)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, na.rm = TRUE) +
  labs(title = "Moyenne des zones protégées à l'échelle nationale (% du territoire total)",
       x = "Année",
       y = "Pourcentage moyen des zones protégées") +
  theme_minimal()


# pour Zones terrestres et marines protégées (% du territoire total)

OCDE_terrestre_marine_protégée_totale <- OCDE_pays %>%
  filter(Variable == "Zones terrestres et marines protégées (% du territoire total)") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

non_OCDE_terrestre_marine_protégée_totale <- non_OCDE_pays %>%
  filter(Variable == "Zones terrestres et marines protégées (% du territoire total)") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

# Merged data 
merged_terrestre_marine_protégée <- bind_rows(OCDE_terrestre_marine_protégée_totale, non_OCDE_terrestre_marine_protégée_totale)

# Calcul de la moyenne par année
average_terrestre_marine <- merged_terrestre_marine_protégée %>%
  group_by(Année, Groupe) %>%
  summarize(Average_Value = mean(Value, na.rm = TRUE), .groups = 'drop')


ggplot(average_terrestre_marine , aes(x = as.factor(Année), y = Average_Value, fill = Groupe)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7, na.rm = TRUE) +
  labs(title = "Moyenne des zones terrestres et marines protégées à l'échelle nationale \n (% du territoire total)",
       x = "Année",
       y = "Pourcentage moyen des zones terrestres et marines protégées") +
  theme_minimal()

```
Les zones protégées inclut des lieux important pour la biodiversité et ne cessent d'augmenter depuis 2015 dans l'ensemble.Elles sont essentielles pour la conservation des espèces et des moyens de subsistances.
Cela montre une prise de conscience par l'opinion mondiale et la necessité de preserver les ressources naturelles. Nous pouvons observer par ces graphs que la moyenne des zones protégées est plus elevés pour les pays membres de l'OCDE. Selon le pays, les zones sont plus ou moins difficile à protéger.En Amazonie par exemple où le président du Brésil laisse portes ouvertes aux industriels car elle represente un des atouts majeurs pour s'enrichir et développer le pays, il es compliqué de rendre complétement protéger cette mine d'or. 



*III- Vulnérabilités et strategies adaptives*

*A- Variable démographique* La migrations des populations vers les
espaces urbains s'est réalisé de manière continu depuis
l'industrialisation au 19 ième siècle. Depuis 2007, la population urbain est plus nombreuse que la population rurale. D'ici 2030, les urbains representeront 60% de la population.  Nous allons analyser ces migrations dans un graph afin de nous rendre compte depuis 1990 des ces évolutions. La Worldbank à plus de variable sur les indices urbains, tandis que l'OCDE est plus tourné vers les migrations, ce qui
va nous permettre d'avoir un grand calibre de donnée.


```{r}
OCDE_pop_urbaine <- OCDE_pays %>%
  filter(Variable == "Population urbaine (% du total)") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

non_OCDE_pop_urbaine <- non_OCDE_pays %>%
  filter(Variable == "Population urbaine (% du total)") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

merged_data_pop_urbaine <- bind_rows(OCDE_pop_urbaine, non_OCDE_pop_urbaine)

average_pop_urbaine <-merged_data_pop_urbaine %>%
  group_by(Année, Groupe) %>%
  summarize(Average_pop_urbaine = mean(Value, na.rm = TRUE))

OCDE_migration <- OCDE_pays %>%
  filter(Variable == "Migration nette") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")



ggplot(average_pop_urbaine, aes(x = as.factor(Année), y = Average_pop_urbaine, group = Groupe, color = Groupe)) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  labs(title = "Évolution de la population urbaine (% du total) par groupe",
       x = "Année",
       y = "Pourcentage moyen de la population urbaine") +
  theme_minimal() +
  scale_color_manual(values = c("OCDE" = "blue", "non_OCDE" = "green")) +
  theme(legend.position = "top")

```
#A travers ce graphique nous pouvons confirmer l'augmentation de la population urbaine depuis les années 1990 avec un écart de taille entre les Pays de l'OCDE et hors OCDE. En moyenne la population urbaine des pays hors OCDE en 2020 atteint les 80% environ et seulement 58% pour l'autre Groupe. Cette différence s'explique par le fait que certains pays aient un taux d'urbanisation proche, voir à  100% comme le Koweit qui n'est pas dans l'OCDE.



```{r}

OCDE_migration <- OCDE_pays %>%
  filter(Variable == "Migration nette") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "OCDE")

non_OCDE_migration <- non_OCDE_pays %>%
  filter(Variable == "Migration nette") %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Année", values_to = "Value") %>%
  mutate(Value = as.numeric(Value), Groupe = "non_OCDE")

merged_migration <- bind_rows(OCDE_migration, non_OCDE_migration)

ggplot(merged_migration, aes(x = as.factor(Année), y = Value, fill = Groupe)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(title = "Évolution de la migration nette par groupe",
       x = "Année",
       y = "Migration Nette") +
  theme_minimal() +
  scale_fill_manual(values = c("OCDE" = "blue", "non_OCDE" = "green")) +
  facet_wrap(~Groupe, scales = "free_y", ncol = 1) +
  theme(legend.position = "top")

```
L'analyse de l'immigration nette pour 1 million d'habitant peut fournir des données interessante sur la dynamique démographique et migratoire des pays. 
Les tendances sont plus à la hausse pour l'OCDE tandis pour l'autre groupe, la migration atteint parfois les négatifs jusqu'a -5000 dans les années 93-94, 2005-2006 et 2015. Cette immigration négative signifie qu'il y a un excedent de 5000 personnes qui quittent le pays par rapport à celle qui y entrent. Il y a donc plus de sortie dans les pays hors OCDE et plus d'entrée dans les pays de l'OCDE en tendance géneral. La hausse de l'immigration de travail est l'un des facteurs à l'origine de cette tendance, étant donné que les travailleurs étrangers ont contribué à réduire les pénuries de mains d'oeuvres dans les pays de l'OCDE.Les guerres et les raisons humanitaires sont aussi des facteurs provoquant l'éxode des populations et la recherche de meilleurs conditions. 



```{r}

migration_eau_OCDE <-  OCDE_pays %>% filter(Variable %in% c("Prélèvements bruts d'eau douce par habitant", "Migration nette")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 


reg_migration_eau_OCDE <- lm(`Prélèvements bruts d'eau douce par habitant` ~ `Migration nette`, data = migration_eau_OCDE)

summary(reg_migration_eau_OCDE)


ggplot(reg_migration_eau_OCDE, aes(x = `Prélèvements bruts d'eau douce par habitant`,y = `Migration nette`)) + geom_point(color='gray', size=1, na.rm = TRUE) + geom_smooth(method='lm', formula = 'y ~ x', se=FALSE, color='red', na.rm = TRUE) + theme_minimal() + labs(title = "Correlation entre la migration nette et les prélevements bruts d'eau douce \n par habitant - OCDE", x = "Prélèvements bruts d'eau douce par habitant", y = "Migration nette")



```

D'après le test statistiques entre la variable explicative (migration nette) et la variable réponse (Prélèvements bruts d'eau douce par habitant) une augmentation dans la migration nette d'une unité est associé à une augmentation de 0,3997 unité dans les prélèvements bruts d'eau douce par habitant. Ce test est tout de même à prendre avec précaution car même si le coefficient associé à "Migration nette" est legerement significatif, les faibles valeurs de R-squared et la p-value  suggère que le modèle explique seulement une petite portion de ces variations.En expliquant une partie plus ou moins significative, il y a quand même un lien entre la hausse de la migration et la hausse des pressions environnementales pour les pays concernés. Cependant à cause des conditions climatiques extrêmes cela devient inévitable. D'ici 2050 216 millions de personnes serait contraint de migrer à l'intérieur même de leurs pays. Une réduction des GES pourrait diminuer l'ampleur des migrations climatiques internes de 80%.


*B- vulnérabilité humaine face aux changement climatiques*
L'industrialisation intensives qui a lieu depuis le 19 ieme siècle et la
consommation de masse des 30 glorieuses n'a pas laissé notre planète
indemne. En effet, les catastrophes naturelles se multiplient et nous
sommes totalement vulnérable face à cet nature déchainé.


```{r}
vulnerabilite_OCDE <- OCDE_pays %>% filter(Variable %in% c("Pourcentage de la population exposée aux journées glacées","Pourcentage de la population exposée aux journées chaudes","Pourcentage de la population exposée aux incendies incontrôlés","Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)","Stress hydrique, total des prélèvements d'eau douce en % des ressources internes renouvelables","Population, total")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

vulnarabilite_OCDE_growth <- vulnerabilite_OCDE %>% filter(Pays%in%unique(decoupling_CO2_OCDE$Pays)) %>% group_by(Année, Pays) %>% summarise(population_exposée_aux_journées_glacées = (`Pourcentage de la population exposée aux journées glacées`*`Population, total`),population_exposée_aux_journées_chaudes = (`Pourcentage de la population exposée aux journées chaudes`*`Population, total`), population_exposée_aux_incendies_incontrôlés = (`Pourcentage de la population exposée aux incendies incontrôlés`*`Population, total`), Population = `Population, total`, .groups = 'drop')

vulnarabilite_OCDE_growth <- vulnarabilite_OCDE_growth  %>% group_by(Année) %>% summarise(somme_population_exposée_aux_journées_glacées = sum(`population_exposée_aux_journées_glacées`, na.rm = TRUE),somme_population_exposée_aux_journées_chaudes = sum(`population_exposée_aux_journées_chaudes`, na.rm = TRUE), somme_population_exposée_aux_incendies_incontrôlés = sum(`population_exposée_aux_incendies_incontrôlés`,na.rm = TRUE), somme_Population = sum(`Population`,na.rm = TRUE))

vulnarabilite_OCDE_growth <- vulnarabilite_OCDE_growth %>% mutate(part_population_exposée_aux_journées_glacées = (`somme_population_exposée_aux_journées_glacées`/`somme_Population`), part_population_exposée_aux_journées_chaudes = (`somme_population_exposée_aux_journées_chaudes`/`somme_Population`),part_population_exposée_aux_incendies_incontrôlés = (`somme_population_exposée_aux_incendies_incontrôlés`/`somme_Population`))

vulnarabilite_OCDE_growth<- vulnarabilite_OCDE_growth %>% pivot_longer(cols = c(part_population_exposée_aux_incendies_incontrôlés,part_population_exposée_aux_journées_chaudes,part_population_exposée_aux_journées_glacées), names_to = "Variable", values_to = "Taux") 

ggplot(vulnarabilite_OCDE_growth, aes(x = Année, y = Taux, color = Variable, linetype = Variable, group = Variable)) + geom_point(na.rm = TRUE)  + geom_line(na.rm = TRUE) + labs(title = "Part de la population de l'OCDE exposée aux incendies incontrôlées, \n aux journées chaudes  et aux journées glacées", x = "Année", y = "Taux (%)", color = "Variable") +  theme(axis.text.x=element_text(angle=50, size=8, vjust=0.5)) +   geom_smooth(method = 'loess',formula = 'y ~ x' , alpha = 0.3, linewidth = 0.5, se = FALSE, color = 'black')



```

Au sein des pays membres de l'OCDE nous constatons que la part de la
population exposée aux journées chaudes à particulièrement augmentées.
En effet bien qu'il y ait de fortes variations entre les années, la
tendance montre une nette augmentation entre 1990 et 2020 d'un peu plus
de 25% à un peu moins de 40%. Pour ce qui est de la part de la
populaition exposée aux journées glacées, il n'y a pas eu de fort
changement durant c'est 30 dernières. Enfin, sur la part de la
population exposée aux incendies, il n'y a pas de valeurs avant 2000,
donc on ne peut pas interpréter les valeurs entre 1990 et 2000, de plus
entre 2000 et 2020 on constate une tendance constante. Nous ne possedons
pas de valeurs pour les pays non membres de l'OCDE, toutefois la
variables Sécheresses, inondations, températures extrêmes (en % de la
population, moyenne de 1990 à 2009) nous permet d'effectuer des
comparaisons entre les pays membres et non-membres de l'OCDE

```{r}
#Sécheresses, inondations, températures extrêmes (en % de la population, moyenne de 1990 à 2009)
risque_OCDE <- OCDE_pays %>% filter(Variable %in% c("Sécheresses, inondations, températures extrêmes (en % de la population, moyenne de 1990 à 2009)")) %>% select(-`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

risque_non_OCDE <- non_OCDE_pays %>% filter(Variable %in% c("Sécheresses, inondations, températures extrêmes (en % de la population, moyenne de 1990 à 2009)")) %>% select(-`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 

risque <- bind_rows(mutate(risque_OCDE, Group = "OCDE"),mutate(risque_non_OCDE, Group = "Non-OCDE"))
risque <- risque %>% filter(Pays%in%unique(risque$Pays)) %>% group_by(Pays, Group, COU) %>% summarise(Secheresses_inondations_temperatures_extreme = `Sécheresses, inondations, températures extrêmes (en % de la population, moyenne de 1990 à 2009)`[Année == 2009], .groups = 'drop')



```

```{r}
ggplot(risque, aes(x = Group, y = Secheresses_inondations_temperatures_extreme, fill = Group)) +
  geom_boxplot(na.rm = TRUE, alpha = 0.8)  +  scale_fill_manual(values = c("Non-OCDE" = "skyblue", "OCDE" = "lightcoral"))  + theme_minimal() + labs(title = "% de la population de chaque pays exposée aux Sécheresses,\n inondations, températures extrêmes (1990-2009)", x = "Groupe", y = "Population de chaque pays exposée aux risques (%)") 
  
```

Par ailleurs, en 2011 certains pays du monde ont été évalué note sur les
progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5
étant la meilleure note)

```{r}

note_progres_non_OCDE <- non_OCDE_pays %>% filter(Variable %in% c("Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)")) %>% select(-`COU`, -`VAR` ,- `Unit`, -`PowerCode`) %>% pivot_longer(cols = c(`1990`,`1991`,`1992`,`1993`,`1994`,`1995`,`1996`,`1997`,`1998`,`1999`,`2000`, `2001`, `2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`,`2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`),  names_to = "Année", values_to = "Value") %>% mutate(Value = as.numeric(Value)) %>% pivot_wider(names_from = Variable, values_from = Value) 


vulnerabilite_OCDE_clean_progres <- drop_na(vulnerabilite_OCDE,`Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)`) %>% select(`Pays`,`Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)`) %>% mutate(Group = "OCDE")

note_progres_non_OCDE <- drop_na(note_progres_non_OCDE,`Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)`) %>% select(`Pays`,`Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)`)  %>% mutate(Group = "Non-OCDE")

note_progres_fusion <- bind_rows(vulnerabilite_OCDE_clean_progres, note_progres_non_OCDE)

ggplot(note_progres_fusion, aes(y = reorder(Pays,`Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)`),x = `Note sur les progrès de réduction des risques de catastrophes (échelle de 1 à 5, 5 étant la meilleure note)`,  fill = Group)) + geom_bar(stat ="identity", position = "dodge", na.rm = TRUE, width =  0.7) +  theme(axis.text.y = element_text(size = 5),plot.title = element_text(hjust = 0.1)) + labs(title = "Note sur les progrès de réduction des risques de catastrophes\n (échelle de 1 à 5, 5 étant la meilleure note)", y = "Pays", x = "note sur les progrès de\n réduction des risques de catastrophes") +  scale_fill_manual(values = c("OCDE" = "blue", "Non-OCDE" = "red")) 




```

ii. Examiner les variables en lien avec les zones en-dessous d'une
    certaine altitudes ou à proximité du niveau de la mer et leur
    vulnérabilité sur la hausse des niveaux des océans.

Superficie des terres dont l'altitude est inférieure à 5 mètres (en % de
la superficie totale des terres) Zones urbaines dont l'altitude est \< à
5 m (% de la superficie totale) Population vivant sur des terres dont
l'altitude est inférieure à 5 mètres (en % de la population totale)
Hauteur moyenne des précipitations (mm par an)

```{r}
# Hauteur moyenne des précipitations (mm par an)

```
