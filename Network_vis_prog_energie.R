rm(list=ls())

# Packages a installer pour arriver a travailler avec network visualization
# install.packages("igraph")
# install.packages("sna")
# install.packages('network') 
# install.packages('ndtv')
# install.packages('visNetwork')

## Set working directory
output_path<- "H:/network_vis/output/"
data_path<- "H:/network_vis/data/"

setwd(data_path)

########################################## POUR UN PROJET SEULEMENT ###################################################################


## Load data pour projet PMP-CMP seulement

nodes<- read.csv("liens_prog_energie_NODES_PMPCMP.csv", sep=";", header=T, as.is=T)
links<- read.csv("liens_prog_energie_EDGES_PMPCMP.csv", sep=";", header=T, as.is=T)


library('igraph')

network_proj1 <- graph_from_data_frame(d=links, vertices=nodes, directed=F)
network_proj1

colrs<- c("steel blue", "orange", "red", "purple", "grey", "green", "blue", "yellow")
V(network_proj1)$color<- colrs[V(network_proj1)$type.collaborateur+1]

colrs_links<- c("darkgrey", "darkgreen", "goldenrod")
E(network_proj1)$color<- colrs_links[E(network_proj1)$type]
E(network_proj1)$width<- 1.5

setwd(output_path)
pdf("Projet_PMP-CMP.pdf")

l<- layout_nicely(network_proj1)#, center=V(network_proj1)[8])

plot(network_proj1,
     layout=l,
     vertex.shape="rectangle",
     # vertex.size=(strwidth(V(network_proj1)$label) + strwidth("oo")) * 50,
     # vertex.size2=strheight("I") * 2 * 50,
     vertex.label=V(network_proj1)$label,
     main="Liens entre les différents collaborateurs - Projet PMP-CMP" )
legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial"), pch=21,
       col="black", pt.bg=colrs[c(1,2,6,3)], ncol=1, pt.cex=1, cex=.5,bty="n")



plot(network_proj1,
     layout=l,
     vertex.shape="sphere",
     vertex.label=V(network_proj1)$label,
        vertex.label.cex=0.6,
        vertex.label.color="black",
     #edge.lty=2, 
     main="Liens entre les différents collaborateurs - Projet PMP-CMP")
legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial"), pch=21,
            col="black", pt.bg=colrs[c(1,2,6,3)], ncol=1, pt.cex=1, cex=.5,bty="n")

net_proj1_collab <- network_proj1 - E(network_proj1)[E(network_proj1)$type==2] 
net_proj1_collab<- net_proj1_collab - E(net_proj1_collab)[E(net_proj1_collab)$type==3] 
net_proj1_fin_appui <- network_proj1 - E(network_proj1)[E(network_proj1)$type==1] 

# Plot the two links separately:

par(mfrow=c(1,2))
plot(net_proj1_collab,  main="Liens de collaboration", layout=l,
     vertex.shape="sphere",
     vertex.label=V(net_proj1_collab)$label,
     vertex.label.cex=0.6,
     vertex.label.color="black")
legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial"), pch=21,
       col="black", pt.bg=colrs[c(1,2,6,3)], ncol=1, pt.cex=1, cex=.5,bty="n")


E(net_proj1_fin_appui)$width <- 1+E(net_proj1_fin_appui)$weight/20000

plot(net_proj1_fin_appui , main="Liens d'appui et de financement", layout=l, 
     vertex.shape="sphere",
     vertex.label=V(net_proj1_collab)$label,
     vertex.label.cex=0.6,
     
     vertex.label.color="black")


legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial"), pch=21,
       col="black", pt.bg=colrs[c(1,2,6,3)], ncol=1, pt.cex=1, cex=.5,bty="n")

dev.off()


rm(net_proj1_fin_appui, net_proj1_collab, network_proj1, links, nodes, l)


########################################## POUR PLUS D'UN PROJET ###################################################################

setwd(data_path)

nodes<- read.csv("liens_prog_energie_NODES.csv", sep=";", header=T, as.is=T)
links<- read.csv("liens_prog_energie_EDGES.csv", sep=";", header=T, as.is=T)


network_nrj<- graph_from_data_frame(d=links, vertices=nodes, directed=F)

#colrs<- c("steel blue", "orange", "red", "purple", "grey", "green", "blue", "yellow", "magenta")
colrs <- rainbow(9, alpha=.5)
V(network_nrj)$color<- colrs[V(network_nrj)$type.collaborateur+1]

colrs_links<- c("darkgrey", "darkgreen", "goldenrod", "magenta")
E(network_nrj)$color<- colrs_links[E(network_nrj)$type]
E(network_nrj)$width<- 2
#V(network_nrj)$size<- 5

V(network_nrj)$size<-8+ (V(network_nrj)$budget.projet/80000)

setwd(output_path)
pdf("Network_nrj.pdf")

l<- layout_nicely(network_nrj)

plot(network_nrj, 
     vertex.shape="sphere",
     vertex.label.cex=0.6,
     vertex.label.color="black", 
     layout=l,
     main="Programme énergie")

legend (x=0, y=-1.1, c("Collaboration", "Contribution en nature ou financière", "Appui", "Suivi"), pch="-",
        col=colrs_links[c(1,3,2,4)], ncol=1, pt.cex=1.5, cex=0.8, bty="n")

legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial", "Organisme subventionnaire", "Consultants"), pch=21,
       col="black", pt.bg=colrs[c(1,2,6,3,9,7)], ncol=1, pt.cex=1, cex=.5,bty="n")

l<- layout_with_dh(network_nrj)

plot(network_nrj, 
     vertex.shape="sphere",
     vertex.label.cex=0.6,
     vertex.label.color="black", 
     layout=l, 
     main="Programme énergie")
legend (x=0, y=-1.1, c("Collaboration", "Contribution en nature ou financière", "Appui", "Suivi"), pch="-",
        col=colrs_links[c(1,3,2,4)], ncol=1, pt.cex=1.5, cex=0.8, bty="n")

legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial", "Organisme subventionnaire", "Consultants"), pch=21,
       col="black", pt.bg=colrs[c(1,2,6,3,9,7)], ncol=1, pt.cex=1, cex=.5,bty="n")


dev.off()

#### Tests de layout

pdf("Tests_de_layout_PROGNRJ_3Projets.Pdf")

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

for (layout in layouts) {
        print(layout)
        25
        l <- do.call(layout, list(network_nrj))
        plot(network_nrj, edge.arrow.mode=0,
             vertex.shape="sphere",
             vertex.label.cex=0.6,
             vertex.label.color="black", layout=l, main=layout)
        legend (x=0, y=-1.1, c("Collaboration", "Contribution en nature ou financière", "Appui", "Suivi"), pch="-",
                col=colrs_links[c(1,3,2,4)], ncol=1, pt.cex=1.5, cex=0.8, bty="n")
        
        legend(x=-1.2, y=-1.1, c("Projets Ouranos", "Universités",  "Secteur de l'énergie", "Gouvernement provincial", "Organisme subventionnaire", "Consultants"), pch=21,
               col="black", pt.bg=colrs[c(1,2,6,3,9,7)], ncol=1, pt.cex=1, cex=.5,bty="n")
}

dev.off()