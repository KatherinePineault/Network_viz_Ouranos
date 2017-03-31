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

## Load data

nodes<- read.csv("liens_reseau_Ouranos_NODES.csv", sep=";", header=T, as.is=T)
links<- read.csv("liens_reseau_Ouranos_EDGES.csv", sep=";", header=T, row.names=1) ## Rownames=1 veut dire que la premiere colonne contient les rownames


## Transformation dans un objet de type network - one mode
library(igraph)

        links<- as.matrix(links)
        #network <- graph_from_incidence_matrix(links)

library(bipartite)        
        links2<- as.one.mode(links)
        g<- graph.adjacency(links2, weighted=TRUE)
        df<- get.data.frame(g)
        
        # garder seulement les relations pour lesquelles le point de départ est un programme - le "from" commence par P
        df2<- subset(df, grepl("^[P]", df$from))
        network2<- graph_from_data_frame(d=df2, vertices=nodes, directed=F)

        
        
## vertex.labels
        n<- 71 
        
        radian.rescale <- function(x, start=0, direction=1) {
                c.rotate <- function(x) (x + start) %% (2 * pi) * direction
                c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
        }
        
        lab.locs <- radian.rescale(x=1:n, direction=-1, start=0)     
        
        
        
          
## Tests de graphiques
      # Set attributes

        # Couleurs selon le type de nodes
        colrs<- c("steel blue", "orange", "red", "purple", "grey", "green", "blue", "yellow")
        V(network2)$color<- colrs[V(network2)$type+1]
      
      
       
        l<- layout_on_sphere(network2)
        plot(network2, vertex.label=NA, layout=l)
        
setwd(output_path)    
pdf("Reseau_Ouranos.pdf")

        l<- layout.fruchterman.reingold(network2)
        
        E(network2)$color<- "slategray2"
        E(network2)$width<- 1+E(network2)$weight/10
        plot(network2, 
             vertex.label.cex=.4, 
             layout=l, 
             main="Appartenance des chercheurs et étudiants dans le réseau d'Ouranos")
        legend(x=-1.2, y=-1.1, c("Programmes Ouranos", "Universités", "Ministères provinciaux QC", "Ministères fédéraux CAN", "Autres gouvernements", "Secteur de l'énergie", "Consultants", "Autre partenaires"), pch=21,
               col="black", pt.bg=colrs, ncol=1, pt.cex=.8, cex=.5, bty="n")

dev.off()


pdf("Reseau_Ouranos_cercle.pdf")
        l<- layout_in_circle(network2)
        E(network2)$color<- "grey"
        V(network2)$size<- 5
        plot(network2, 
             #vertex.label=NA, 
             layout=l,
             vertex.label.degree=lab.locs, 
             vertex.label.dist=0.8, 
             vertex.label.cex=.6, 
             main="Liens des programmes d'Ouranos avec les collaborateurs", #edge.curved=.1, 
             #edge.label=E(network2)$weight, 
             edge.label.cex=.6)
        legend(x=-1.2, y=-1.1, c("Programmes Ouranos", "Universités", "Ministères provinciaux QC", "Ministères fédéraux CAN", "Autres gouvernements", "Secteur de l'énergie", "Consultants", "Autre partenaires"), pch=21,
               col="black", pt.bg=colrs, ncol=1, pt.cex=1, cex=.5, bty="n")
        

dev.off()                
        
        l<- layout_in_circle(network_energie)
        V(network_energie)$color<- colrs[V(network_energie)$type+1]
        E(network_energie)$width<-1
        plot(network_energie, vertex.label=NA, layout=l)
        
        
## Pour le network_energie
        df_energie<- df2[which( df2$from=="P01"),] # Garder seulement les relations qui partent du programme energie
        network_energie<- graph_from_data_frame(d=df_energie, vertices=nodes, directed=F)
        
        # Graphique qui donne le reseau Ouranos en cercle avec les edges qui partent d'energie seulement
        l<- layout_in_circle(network_energie)
        colrs<- c("steel blue", "orange", "red", "purple", "grey", "green", "blue", "yellow")
        V(network_energie)$color<- colrs[V(network_energie)$type+1]
        V(network_energie)$size<- 5
        E(network_energie)$color<- "black"
        
        E(network_energie)$width<- E(network_energie)$weight/5
        
               
        
setwd(output_path)

pdf("Premier_graph_energie.pdf")
                
        plot(network_energie, layout=l, vertex.label.degree=lab.locs, vertex.label.dist=0.8, vertex.label.cex=.6, main="Liens du programme énergie dans le réseau Ouranos", #edge.curved=.1, 
             edge.label=E(network_energie)$weight, edge.label.cex=.6)
        legend(x=-1.2, y=-1.1, c("Programmes Ouranos", "Universités", "Ministères provinciaux QC", "Ministères fédéraux CAN", "Autres gouvernements", "Secteur de l'énergie", "Consultants", "Autre partenaires"), pch=21,
               col="black", pt.bg=colrs, ncol=1, pt.cex=1, cex=.5, bty="n")

dev.off()                
        
pdf("Deuxieme_graph_energie.pdf")

l<- layout.fruchterman.reingold(network_energie)

E(network_energie)$color<- "slategray2"
E(network_energie)$width<- 1+E(network_energie)$weight/10
plot(network_energie, 
     vertex.label.cex=.4, 
     layout=l, 
     main="Appartenance des chercheurs et étudiants reliés au programme Énergie")
legend(x=-1.2, y=-1.1, c("Programmes Ouranos", "Universités", "Ministères provinciaux QC", "Ministères fédéraux CAN", "Autres gouvernements", "Secteur de l'énergie", "Consultants", "Autre partenaires"), pch=21,
       col="black", pt.bg=colrs, ncol=1, pt.cex=.8, cex=.5, bty="n")

dev.off()


        # Si on ne garde que les nodes qui sont dans le programme energie
        df_energie<- df2[which( df2$from=="P01"),] # Garder seulement les relations qui partent du programme energie
        nodes_energie<- nodes[which(nodes$id %in% df_energie$to | nodes$id %in% df_energie$from),] # Garder seulement les nodes qui sont en lien
        network_energie<- graph_from_data_frame(d=df_energie, vertices=nodes_energie, directed=F)
         
         # Edge width based on weight
        E(network_energie)$width<-E(network_energie)$weight/3
        
          #Edge color based on weight
        heat_pal<- heat.colors(22, alpha=1)
        E(network_energie)$color<- heat_pal[E(network_energie)$weight]

          # Nodes color based on type
        colrs<- c("steel blue", "orange", "red", "purple", "grey", "green", "blue", "yellow")
        V(network_energie)$color<- colrs[V(network_energie)$type+1]
                
        
        l<-layout_as_star(network_energie)
        plot(network_energie, layout=l)
        
        
pdf("Troisieme_graph_energie.pdf")
        
        # E(network_energie)$weight2<- E(network_energie)$weight
        # x<- 1/E(network_energie)$weight
        # E(network_energie)$weigth<- x
        
        l<- layout_nicely(network_energie)
        
        E(network_energie)$color<- "slategray2"
        E(network_energie)$width<- 1+E(network_energie)$weight/3

        plot(network_energie, 
             vertex.label.cex=.4, 
             layout=l, 
             edge.label=E(network_energie)$weight, 
             edge.label.cex=.6,
             main="Appartenance des chercheurs et étudiants reliés,programme Énergie")
        legend(x=-1.2, y=-1.1, c("Programmes Ouranos", "Universités", "Ministères provinciaux QC", "Ministères fédéraux CAN", "Autres gouvernements", "Secteur de l'énergie", "Consultants", "Autre partenaires"), pch=21,
               col="black", pt.bg=colrs, ncol=1, pt.cex=.8, cex=.5, bty="n")
        
dev.off()