### Workshop sur le network visualization 
rm(list=ls())

# Packages à installer pour arriver à travailler avec network visualization
        # install.packages("igraph")
        # install.packages("sna")
        # install.packages('network') 
        # install.packages('ndtv')
        # install.packages('visNetwork')

# Colors
        plot(x=1:10, y=rep(5,10), pch=19, cex=2, col="dark red") ## Couleur de type "named colors"
        points(x=1:10, y=rep(6, 10), pch=19, cex=3, col="557799") ## Couleur de type "HEX" 
        points(x=1:10, y=rep(4, 10), pch=19, cex=3, col=rgb(.25, .5, .3)) # Couleur de type "RGB"
                # NOTES: pch sert à définir la forme, cex est la grosseur, col peut être determine de trois façons differentes. 

# Background color: 
        par(bg="white")
        # peut être changé par n'importe quelle couleur de type "named colors"
        
# Opacity 
                # Pour des couleurs RGB:
        plot(x=1:5, y=rep(5,5), pch=19, cex=12, col=rgb(.25, .5, .3, alpha=.5), xlim=c(0,6))
                # Pour des couleurs HEX 
        par(bg="gray40") #background gris
        col.tr <- grDevices::adjustcolor("557799", alpha=0.7) # c'est avec la fonction adjustcolor
        plot(x=1:5, y=rep(5,5), pch=19, cex=12, col=col.tr, xlim=c(0,6))
                
        
                #Pour les 'named colors'
        colors() #sert à faire la liste de toutes les couleurs existantes

# Section 3.1 sur les datasets - edgelist

setwd("/Users/katherinepineault/Documents/Etudes/Network_visualization/polnet2016/Data files/")

        nodes<- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
        links<- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
        
        links<- aggregate(links[,3], links[,-3], sum)
        links <- links[order(links$from, links$to),] 
        colnames(links)[4] <- "weight"
        rownames(links) <- NULL

# Section 3.2 sur les datasets 2 - matrix

        nodes2<- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T)
        links2<- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1) ## Rownames=1 veut dire que la première colonne contient les rownames
        
        links2<- as.matrix(links2)

# Section 4: Plotting networks with igraph
# Section 4.1: Turning networks into igraph objects
        # Dataset 1 : Edgelist
library(igraph)

        net<- graph_from_data_frame(d=links, vertices=nodes, directed=T)

        net<- simplify(net, remove.multiple = T, remove.loops = T)
        plot(net, edge.arrow.size=0.5,vertex.label=NA)

        # Dataset 2 : matrix
        
        net2 <- graph_from_incidence_matrix(links2) 
        
# Section 4.2 : Plotting parameters
        ## Deux façons de changer les paramètres des représentations visuelles: 
        #1 - Ajouter de l'information dans la déclaration du graphique
        #2 - Ajouter des attributs à l'objet igraph
        
        #1:
        
        # Plot with curved edges: 
        plot(net, edge.arrow.size=.4, edge.curved=.2, vertex.label=NA)
        
        #Plot
        plot(net, edge.arrow.size=.2, edge.color="blue",
             vertex.color="blue", vertex.frame.color="red", 
             vertex.label=V(net)$media, vertex.label.color="black")
        
                ## Question: Pourquoi la forme du graphique change à toutes les fois que je le génère?!
                ## Comment éviter cela? 
        
        #2:
        # Generate colors based on media type:
        colrs <- c("gray50", "tomato", "gold") 
        V(net)$color <- colrs[V(net)$media.type]
        
        # Compute node degrees (#links) and use that to set node size:
        deg <- degree(net, mode="all")
        V(net)$size <- deg*3
        
        # We could also use the audience size value:
        V(net)$size <- V(net)$audience.size*0.6
        
        # The labels are currently node IDs.
        # Setting them to NA will render no labels:
        V(net)$label <- NA
        
        # Set edge width based on weight:
        E(net)$width <- E(net)$weight/6
        
        #change arrow size and edge color:
        E(net)$arrow.size <- .2 
        E(net)$edge.color <- "gray80"
        E(net)$width <- 1+E(net)$weight/12 
        
        plot(net)
        
        
# Section 4.3        
      
        