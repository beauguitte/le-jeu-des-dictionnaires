##LB août 2026

library(igraph)

lien <- read.delim("DATA/Damon_Paquot2021_liens.txt")
sommet <- read.delim("DATA/Damon_Paquot2021_sommets.txt")

# jointure
lien <- merge(lien, sommet, by.x="ORIGINE", by.y="NUMERO", all.x=TRUE, all.y=FALSE)
lien <- merge(lien, sommet, by.x="DESTINATION", by.y="NUMERO", all.x=TRUE, all.y=FALSE)

# réseau
g <- graph_from_data_frame(lien[,c(3,5)], directed =TRUE)
g
plot(g)

# degré entrant et sortant
V(g)$degin <- degree(g, mode="in")
V(g)$degou <- degree(g, mode="out")

# degré entrant nul
g0 <- induced_subgraph(g, v = which(V(g)$degin == 0))
V(g0)$name

# tableau des degrés entrants et sortants
d <- as.data.frame(list(Vertex=V(g), deg_in=V(g)$degin, deg_ou=V(g)$degou))

# ego-network
# ego-network 
EgoNet <- make_ego_graph(g,
                               nodes = V(g)[name=='Arbre'], 
                               order = 1,  # voisins d'ordre 1
                               mode = c("all"))
plot(EgoNet[[1]], 
     edge.arrow.size = 0.5,
     vertex.color="yellow")
