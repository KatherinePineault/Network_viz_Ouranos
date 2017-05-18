rm(list=ls())
## AJOUT D'EVAP

# Packages a installer pour arriver a travailler avec network visualization
# install.packages("igraph")
# install.packages("sna")
# install.packages('network') 
# install.packages('ndtv')
# install.packages('visNetwork')

## Libraries to call
library('igraph')
library("RColorBrewer")
## Set working directory
output_path<- "H:/network_vis/output/"
data_path<- "H:/network_vis/data/"


setwd(data_path)
nodes<- read.csv("liens_prog_gestioneau_NODES_avecEVAP.csv", sep=";", header=T, as.is=T)
nodes<- nodes[complete.cases(nodes),] ## ajoute parce qu'il y avait des lignes avec des NA a la fin
links<- read.csv("liens_prog_gestioneau_EDGES_avecEVAP.csv", sep=";", header=T, as.is=T)
links<- subset(links, type!="NA") ## ajoute parce qu'il y avait des lignes avec des NA a la fin

network<- graph_from_data_frame(d=links, vertices=nodes, directed=F)


colrs3<- rainbow(19)
colrs2<- c("red", "tomato4", "darkslategrey", "greenyellow", "mediumblue", "navyblue", "hotpink4", "forestgreen", "gold3", "brown4", "orangered4", "magenta4", "ivory4", "azure2", "cyan", "violet", "plum", "darkkhaki", "maroon1")
colrs <- rainbow(19, alpha=1)
V(network)$color<- colrs3[V(network)$type.collaborateur+1]

colrs_links<- c("darkgrey", "darkgreen", "goldenrod4", "darkmagenta")
E(network)$color<- colrs_links[E(network)$type]
E(network)$width<- 4

V(network)$size<-11+ (V(network)$budget.projet/120000)

V(network)$shape<- ifelse(V(network)$membre==1, "circle", "sphere")

V(network)$frame.color <- "black"

# Remplacer les noms qui vont sur deux lignes
##V(network_nrj)$label[10]<- "Base de données\ninteractive"
## Voir si c'est possible de ne pas le faire à la main



setwd(data_path)
load("GestionEau_coordonnees_2.Rda")
setwd(output_path)

#tk_plot<- tkplot(network)
#l<- tkplot.getcoords(tk_plot)

#save(l, file="GestionEau_coordonnees_2.Rda")

png(file="Visualisation_GESTIONEAU_2.png", width=2000, height=2000)
plot(network, 
     vertex.shape=V(network)$shape,
     #vertex.shape="sphere",
     #vertex.shape="csquare",
     
     vertex.label.cex=2.5,
     vertex.label.color="black", 
     layout=l)#, 
#main="Programme nordique: Liens et collaborations\n 2014-2016")

#legend (x=0, y=-1.1, c("Collaboration", "Contribution en nature ou financière", "Appui", "Suivi"), pch="-",
# col=colrs_links[c(1,3,2,4)], ncol=1, pt.cex=1.5, cex=0.7, bty="n")

#legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités", "Secteur de l'énergie", "Gouvernement", "Organisme subventionnaire", "Consultants", "Autre"), pch=21,
# col="black", pt.bg=colrs[c(1,2,6,4,9,7,10)], ncol=1, pt.cex=1, cex=.5,bty="n")

#text(x=1.1, y=-.4, "Note 1: Les collaborateurs qui sont\n membres sont représentés par des cercles\n et les non-membres, par des sphères.\n\nNote 2: La taille des projets \nreprésente la taille de leur budget total.", cex=0.5)

dev.off()