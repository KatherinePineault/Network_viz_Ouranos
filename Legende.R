rm(list=ls())

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
nodes<- read.csv("liens_legende_NODES.csv", sep=";", header=T, as.is=T)
links<- read.csv("liens_legende_EDGES.csv", sep=";", header=T, as.is=T)

network<- graph_from_data_frame(d=links, vertices=nodes, directed=F)

colrs3<- rainbow(21)
colrs2<- c("red", "tomato4", "darkslategrey", "greenyellow", "mediumblue", "navyblue", "hotpink4", "forestgreen", "gold3", "brown4", "orangered4", "magenta4", "ivory4", "azure2", "cyan", "violet", "plum", "darkkhaki", "maroon1")
colrs <- rainbow(19, alpha=1)
V(network)$color<- colrs3[V(network)$type.collaborateur+1]


#colrs_links<- c("darkgrey", "darkgreen", "goldenrod4", "darkmagenta")
colrs_links<- c("darkgrey", "darkgreen", "white", "darkmagenta")

E(network)$color<- colrs_links[E(network)$type]

V(network)$shape<- ifelse(V(network)$membre==1, "circle", "sphere")
E(network)$width<- 4
V(network)$size<-11
#V(network)$frame.color <- "black"

#V(network)$label[4]<-"Tested Decision\n making protocol"

setwd(output_path)

#setwd(data_path)
load("LEGENGE2_coordonnees.Rda")


#tk_plot<- tkplot(network)
#l<- tkplot.getcoords(tk_plot)

#save(l, file="LEGENGE2_coordonnees.Rda")

png(file="Visualisation_LEGENDE2.png", width=2000, height=2000)
plot(network, 
     vertex.shape=V(network)$shape,
     #vertex.shape="sphere",
     #vertex.shape="csquare",
     
     vertex.label.cex=5,
     vertex.label.color="black", 
     layout=l)#, 
dev.off()