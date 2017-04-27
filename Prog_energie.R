rm(list=ls())

# Packages a installer pour arriver a travailler avec network visualization
# install.packages("igraph")
# install.packages("sna")
# install.packages('network') 
# install.packages('ndtv')
# install.packages('visNetwork')

## Libraries to call
library('igraph')

## Set working directory
output_path<- "H:/network_vis/output/"
data_path<- "H:/network_vis/data/"

setwd(data_path)

nodes<- read.csv("liens_prog_energie_NODES.csv", sep=";", header=T, as.is=T)
links<- read.csv("liens_prog_energie_moins_EDGES.csv", sep=";", header=T, as.is=T)
load("Coordonnees_prog_NRJ.Rda")

### READ DOCUMENTATION : http://igraph.org/r/doc/layout_with_fr.html

network_nrj<- graph_from_data_frame(d=links, vertices=nodes, directed=F)

colrs <- rainbow(10, alpha=1)
V(network_nrj)$color<- colrs[V(network_nrj)$type.collaborateur+1]

colrs_links<- c("darkgrey", "darkgreen", "goldenrod", "magenta")
E(network_nrj)$color<- colrs_links[E(network_nrj)$type]
E(network_nrj)$width<- 2

V(network_nrj)$size<-15+ (V(network_nrj)$budget.projet/120000)

V(network_nrj)$shape<- ifelse(V(network_nrj)$membre==1, "circle", "sphere")

V(network_nrj)$frame.color <- "black"

# Remplacer les noms qui vont sur deux lignes
V(network_nrj)$label[10]<- "Base de données\ninteractive"

setwd(output_path)

pdf("Visualisation_NRJ_19avril.pdf")

plot(network_nrj, 
     vertex.shape=V(network_nrj)$shape,
     #vertex.shape="sphere",
     #vertex.shape="csquare",
     
     vertex.label.cex=0.6,
     vertex.label.color="black", 
     layout=l, 
     main="Programme énergie: Liens et collaborations\n 2014-2016")

legend (x=0, y=-1.1, c("Collaboration", "Contribution en nature ou financière", "Appui", "Suivi"), pch="-",
        col=colrs_links[c(1,3,2,4)], ncol=1, pt.cex=1.5, cex=0.7, bty="n")

legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités", "Secteur de l'énergie", "Gouvernement", "Organisme subventionnaire", "Consultants", "Autre"), pch=21,
       col="black", pt.bg=colrs[c(1,2,6,4,9,7,10)], ncol=1, pt.cex=1, cex=.5,bty="n")

text(x=1.1, y=-.4, "Note 1: Les collaborateurs qui sont\n membres sont représentés par des cercles\n et les non-membres, par des sphères.\n\nNote 2: La taille des projets \nreprésente la taille de leur budget total.", cex=0.5)

dev.off()