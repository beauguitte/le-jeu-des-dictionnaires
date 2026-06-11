#LB, 2026, Le jeu des dictionnaires

library(igraph)

d <- read.delim("Cosseron2007_liens.txt")

# Nombre de notices
length(unique(d$ENTREE))

# Notices sans le moindre renvoi
sum(is.na(d$DESTINATION))

# Distribution des degrés entrants et sortants
# Composantes et réciprocité des liens
# Réseau des liens mutuels

g <- graph_from_data_frame(d[,c(3,5)], directed= TRUE)

# supprimer sommet NA
g <- induced_subgraph(g, v = which(V(g)$name != "NA"))
is_simple(g)

# supprimer boucles et liens multiples
g <- simplify(g, remove.loops = TRUE, remove.multiple = TRUE)
is_simple(g)

# degrés entrants et sortants
V(g)$degin <- degree(g, mode = "in")
V(g)$degou <- degree(g, mode = "out")
summary(V(g)$degin)
summary(V(g)$degou)

# connexité
comp <- components(g)
# nombre d'isolés
table(comp$csize)
# proportion de sommets dans la composante principale
100* max(comp$csize) / vcount(g)

# suppression des isolés
g <- delete_vertices(g, V(g)[degree(g, mode="all") < 1])

# extraire la plus grande composante 
comp <- decompose(g)

pgcom <- comp[[1]]
plot(pgcom, 
     vertex.label=NA, 
     vertex.size = 6, 
     vertex.color="yellow",
     edge.arrow.size = 0.1)

# proportion de liens mutuels
dc <- dyad_census(pgcom)
100*dc$mut / dc$asym

# degré entrant
V(pgcom)$degin <- degree(pgcom, mode="in")
summary(V(pgcom)$degin)

# proportion de sommets avec degré entrant nul
100*table(V(pgcom)$degin) / vcount(pgcom)

# explorer degrés entrants

metrics <- data.frame(
  nom = V(pgcom)$name,
  degin = V(pgcom)$degin
)

# distribution des degrés entrants (composante principale)
plot(degree_distribution(pgcom, mode="in"), type="b")

# ego-network antifascisme
EgoNet_ville <- make_ego_graph(pgcom,
                               nodes = V(pgcom)[name=='antifascisme'], 
                               order = 2,  # voisins d'ordre 2
                               mode = c("all"))
plot(EgoNet_ville[[1]], 
     edge.arrow.size = 0.3,
     vertex.color="yellow")

# ego-network écologie
EgoNet_ville <- make_ego_graph(pgcom,
                               nodes = V(pgcom)[name=='écologie'], 
                               order = 2,  # voisins d'ordre 2
                               mode = c("all"))
plot(EgoNet_ville[[1]], 
     edge.arrow.size = 0.3,
     vertex.color="yellow")

# sélection du réseau des liens mutuels
mutg <- which_mutual(pgcom)
gmg <- delete_edges(pgcom, E(pgcom)[mutg == FALSE])

# suppression des isolés
gmg <- delete_vertices(gmg, V(gmg)[degree(gmg) < 1])

plot(gmg, 
     vertex.label.cex=1, 
     vertex.color="yellow",
     vertex.size = 6, 
     edge.arrow.size = 0)

# extraire la plus grande composante connexe
comp <- decompose(gmg)

plot(comp[[6]], 
     vertex.label.cex=1, 
     vertex.color="yellow",
     vertex.size = 6, 
     edge.arrow.size = 0)

rm(list=ls())
