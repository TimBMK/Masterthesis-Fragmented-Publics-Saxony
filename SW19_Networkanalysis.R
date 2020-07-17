  ### Sachsenwahl 2019 ###
 ########################
 ### Network Analysis ###
#########################

setwd("E:\\Nextcloud\\R\\Masterarbeit")
load("sachsenwahl_tweets.RDa")
load("sachsenwahl_botscores.RDa")

library(dplyr)
library(igraph)
library(graphTweets)
library(data.table)
library(scales)
library(rgexf)
library(ggraph)
library(circlize)
library(xlsx)


# botscores as numeric
sachsenwahl_botscores <- as.data.frame(sachsenwahl_botscores)
sachsenwahl_botscores[, 1:18] <- sapply(sachsenwahl_botscores[, 1:18], as.numeric)
# users whose bot detection produced an error have no screen_name attached, thus can't be classified (50 of 1093 cases)
sachsenwahl_botscores <- sachsenwahl_botscores[!is.na(sachsenwahl_botscores$scores.universal),]
# remove stray duplicates (1)
sachsenwahl_botscores <- sachsenwahl_botscores[!duplicated(sachsenwahl_botscores),]
# add "yes"/"no" classification for botscores > 0.5 [for quick overviews only!]
sachsenwahl_botscores$bot <- NA
sachsenwahl_botscores[sachsenwahl_botscores$scores.universal >= 0.5, "bot"] <- "yes"
sachsenwahl_botscores[sachsenwahl_botscores$scores.universal < 0.5, "bot"] <- "no"




### Make Graphs ----------

# Make mentions network
sachsenwahl_tweets %>%
  gt_edges(screen_name, mentions_screen_name, status_id, is_retweet, is_quote, verified) %>%      # additional edge attributes
  gt_nodes(meta = F) %>%
  gt_graph() -> mentions_graph

# 'n' in mentions graph = number of mentions in a single tweet, not number of interactions between two nodes


## Community Detection for Retweet Network

# Make retweet network for community detection

retweet_graph <- subgraph.edges(mentions_graph, which(E(mentions_graph)$is_retweet == TRUE))

# Community Detection 
louvain <- cluster_louvain(as.undirected(retweet_graph, mode = "each"))
modularity(louvain)
V(retweet_graph)$membership <- louvain$membership
communities_RT <- as.data.table(sizes(louvain))
colnames(communities_RT) <- c("community", "size")

# crossing
E(retweet_graph)$crossing <- crossing(louvain, retweet_graph)

# Top Communities
communities_RT$share <- (communities_RT$size / sum(communities_RT$size))
communities_RT_top <- communities_RT[communities_RT$share>=0.01,]
sum(communities_RT_top$share) # 96.8% of retweet network classified in communities, 3.2% "Rest"
communities_RT_top$share <- percent(communities_RT_top$share)
# 15 Communitites with more than 1% of the total share of members in the network

# PageRank and indegree within top communities
for (i in communities_RT_top$community){
  community <- induced_subgraph(retweet_graph, which(V(retweet_graph)$membership == i), impl = "auto")
  pagerank_community <- page_rank(community)
  V(retweet_graph)$pagerank_community[V(retweet_graph)$membership==i] <- pagerank_community$vector
  V(retweet_graph)$indegree_community[V(retweet_graph)$membership==i] <- igraph::degree(community, mode = "in", loops = F)
}



## Additional Measures for Mentions (full) Networkn ------

# indegree (loops ommitted to ignore self-retweets/-replies)
V(mentions_graph)$indegree <- igraph::degree(mentions_graph, v=V(mentions_graph), mode = "in", loops = F)

# PageRank
pagerank <- page_rank(mentions_graph)
V(mentions_graph)$pagerank <- pagerank$vector

# merge botscore
scores <- as.data.table(V(mentions_graph)$name)
names(scores) <- "user.screen_name"
# Attention!: names in graph are all lowercase, therefore names of botscores must be tolower() to not lose data!
sachsenwahl_botscores$user.screen_name <- tolower(sachsenwahl_botscores$user.screen_name)
scores <- left_join(scores, sachsenwahl_botscores) %>% select(user.screen_name, scores.universal, cap.universal, bot)
V(mentions_graph)$botscore <- scores$scores.universal
V(mentions_graph)$cap <- scores$cap.universal
V(mentions_graph)$bot <- scores$bot


# Followers Count
followers <- as.data.table(V(mentions_graph)$name)
names(followers) <- "screen_name"
sachsenwahl_followers <- data.table(screen_name = tolower(sachsenwahl_tweets$screen_name),
                                   followers_count = sachsenwahl_tweets$followers_count)
sachsenwahl_followers <- unique(sachsenwahl_followers[order(sachsenwahl_followers$followers_count, decreasing = T),], by = "screen_name") #keeps higher value of followers
followers <- left_join(followers, sachsenwahl_followers)
# additional followers from quotes
qu <- sachsenwahl_tweets[sachsenwahl_tweets$is_quote ==T] %>% 
  select(quoted_screen_name, quoted_followers_count)
names(qu) <- c("screen_name", "followers_count")
qu$screen_name <- tolower(qu$screen_name)
qu <- unique(qu[order(qu$followers_count, decreasing = T),], by = "screen_name")
followers <- left_join(followers, qu, by = "screen_name") %>%
  mutate(followers_count = ifelse(is.na(followers_count.x), followers_count.y, followers_count.x)) %>% 
  select(-followers_count.y, -followers_count.x)
# additional followers from retweets
rt <- sachsenwahl_tweets[sachsenwahl_tweets$is_retweet == T] %>% 
  select(retweet_screen_name, retweet_followers_count)
names(rt) <- c("screen_name", "followers_count")
rt$screen_name <- tolower(rt$screen_name)
rt <- unique(rt[order(rt$followers_count, decreasing = T),], by = "screen_name")
followers <- left_join(followers, rt, by = "screen_name") %>%
  mutate(followers_count = ifelse(is.na(followers_count.x), followers_count.y, followers_count.x)) %>% 
  select(-followers_count.y, -followers_count.x)

V(mentions_graph)$followers <- followers$followers_count


# Actual Name
full_names <- as.data.table(V(mentions_graph)$name)
names(full_names) <- "screen_name"
sachsenwahl_names <- data.table(screen_name = tolower(sachsenwahl_tweets$screen_name),
                                    full_name = sachsenwahl_tweets$name)
sachsenwahl_names <- unique(sachsenwahl_names, by = "screen_name") 
full_names <- left_join(full_names, sachsenwahl_names)
# additional names from quotes
qu <- sachsenwahl_tweets[sachsenwahl_tweets$is_quote ==T] %>% 
  select(quoted_screen_name, quoted_name)
names(qu) <- c("screen_name", "full_name")
qu$screen_name <- tolower(qu$screen_name)
qu <- unique(qu, by = "screen_name") 
full_names <- left_join(full_names, qu, by = "screen_name") %>%
  mutate(full_name = ifelse(is.na(full_name.x), full_name.y, full_name.x)) %>% 
  select(-full_name.y, -full_name.x)
# additional names from retweets
rt <- sachsenwahl_tweets[sachsenwahl_tweets$is_retweet == T] %>% 
  select(retweet_screen_name, retweet_name)
names(rt) <- c("screen_name", "full_name")
rt$screen_name <- tolower(rt$screen_name)
rt <- unique(rt, by = "screen_name") 
full_names <- left_join(full_names, rt, by = "screen_name") %>%
  mutate(full_name = ifelse(is.na(full_name.x), full_name.y, full_name.x)) %>% 
  select(-full_name.y, -full_name.x)

V(mentions_graph)$full_name <- full_names$full_name


# Classify Replies and genuine Mentions
mentions <- data.table(status_id = E(mentions_graph)$status_id)

mentions <- left_join(mentions, sachsenwahl_tweets, by = "status_id") %>% 
  select(status_id, is_retweet, is_quote, reply_to_status_id, mentions_screen_name)

mentions$is_reply <- !is.na(mentions$reply_to_status_id)

  # Quote AND Reply: A quoted Tweet as a reply to another tweet. 
  #   However, these quotes are not recognised as edges by graphtweets
  #   => only keep as reply, adjust values in Graph Edge attribute
mentions$is_quote <- !(mentions$is_quote & mentions$is_reply) & !mentions$is_retweet & mentions$is_quote

mentions$is_mention <- !(mentions$is_retweet | 
                          mentions$is_quote |
                          mentions$is_reply)
          # not an exact assessment, as additional mentions within replies / quotes are possible

E(mentions_graph)$is_reply <- mentions$is_reply
E(mentions_graph)$is_mention <- mentions$is_mention
E(mentions_graph)$is_quote <- mentions$is_quote


## Save 
save(mentions_graph, file = "SW19_mentions_graph.RDa")


### Merge Retweet and Mentions Graph ---------

# dataframe format for easier analysis

## nodes
nodes_df <- data.frame(ID = c(1:vcount(mentions_graph)), 
                       NAME = V(mentions_graph)$name)
nodes_df$indegree <- V(mentions_graph)$indegree
nodes_df$pagerank <- V(mentions_graph)$pagerank
nodes_df$botscore <- V(mentions_graph)$botscore
nodes_df$CompleteAutomationProbability <- V(mentions_graph)$cap
nodes_df$bot<- V(mentions_graph)$bot
nodes_df$followers <- V(mentions_graph)$followers
nodes_df$full_name <- V(mentions_graph)$full_name


retweet_nodes <- data.frame(ID = c(1:vcount(retweet_graph)), 
                            NAME = V(retweet_graph)$name,
                            community = V(retweet_graph)$membership, 
                            pagerank_community = V(retweet_graph)$pagerank_community,
                            indegree_community = V(retweet_graph)$indegree_community)
nodes_df <- left_join(nodes_df, retweet_nodes, by = "NAME")
sum(is.na(nodes_df$community))
sum(is.na(nodes_df$community)) / nrow(nodes_df)
# 2915 users without community classification
# = 7.9% of users
# Set 0 as dummy for missing and non-top community classification
nodes_df$community[which(!(nodes_df$community %in% communities_RT_top$community))] <- NA
nodes_df$community[is.na(nodes_df$community)] <- 0


## edges
edges_df <- as.data.frame(get.edges(mentions_graph, c(1:ecount(mentions_graph))))
edges_df$status_id <- E(mentions_graph)$status_id
edges_df$is_retweet <- E(mentions_graph)$is_retweet
edges_df$is_quote <- E(mentions_graph)$is_quote
edges_df$is_reply <- E(mentions_graph)$is_reply
edges_df$is_mention <- E(mentions_graph)$is_mention

# Crossings between communities
  # PROBLEM: can't use standard crossing() function due to different subgraph
  # Solution: Detect crossings from scratch. Reduce to top communities. Value 0 if not classified there

edges_df$V1_membership <- nodes_df[match(edges_df$V1, nodes_df$ID.x),"community"]
edges_df$V1_membership[which(!(edges_df$V1_membership %in% communities_RT_top$community))] <- NA
edges_df$V1_membership[is.na(edges_df$V1_membership)] <- 0

edges_df$V2_membership <- nodes_df[match(edges_df$V2, nodes_df$ID.x),"community"]
edges_df$V2_membership[which(!(edges_df$V2_membership %in% communities_RT_top$community))] <- NA
edges_df$V2_membership[is.na(edges_df$V2_membership)] <- 0

edges_df$crossing <- !(edges_df$V1_membership == edges_df$V2_membership)

sum(is.na(edges_df$crossing))
# Crossings
apply(edges_df[edges_df$crossing==T,4:7],2,sum)
# Not crossing
apply(edges_df[edges_df$crossing==F,4:7],2,sum)


## Inspect community content by hand (descending order of top communities) ----------

# Top 5 Users by pagerank per community
# PageRank is as a better measure for central users in the network since:
#   1) It functions as a proxy for the (potential) networks of users (more visibility if retweetet by higher ranking user)
#   2) The weights minimize the influence of bot(-like) retweeting of single users 
#   (see indegree ranking for comparison)
top_users <- subset(nodes_df, subset=(nodes_df$community %in% communities_RT_top$community)) %>% group_by(community) %>% top_n(5, pagerank_community)
top_users_in <- subset(nodes_df, subset=(nodes_df$community %in% communities_RT_top$community)) %>% group_by(community) %>% top_n(5, indegree_community)

## Classify by most influential users in communities, add colours

# Community 244   (20.444%): Left-leaning Mainstream Public: Linke Sachsen; tagesspiegel correspondent MatthiasMeisner; right-wing critical sociologist Matthias_Quent; DLFNachrichten; MDR Sachsen
nodes_df$community_name[nodes_df$community == 244] <- "Left-leaning Mainstream"
nodes_df$color[nodes_df$community == 244] <- "#1F78B4"

# Community 193   (20.268%): AfD: AfD, party members and supporters. Interesting: right-wing CDU member Markus Roscher-Meinel (@lawyerberlin)
nodes_df$community_name[nodes_df$community == 193] <- "AfD"
nodes_df$color[nodes_df$community == 193] <- "#B15928"

# Community 275   (12.087%): Left-wing satire and bloggers: extra3, heuteshow, _nasir_ahmad_, Die PARTEI Sachsen and Chemnitz
nodes_df$community_name[nodes_df$community == 275] <- "Left-wing Satire and bloggers"
nodes_df$color[nodes_df$community == 275] <- "#FB8072"

# Community 221   (11.729%): AfD-critical/anti-fascist civil society: union watch, hoecke watch, volksverpetzer, lovebeatshb; AfD critic LetKiser
nodes_df$community_name[nodes_df$community == 221] <- "Anti-fascist Civil Society"
nodes_df$color[nodes_df$community == 221] <- "#E7298A"

# Community 208   (5.376%): Artists & Climate Activists: Grönemeyer; climate activists; comedian nnamrreherdna (now:@antrehherrmann)
nodes_df$community_name[nodes_df$community == 208] <- "Artists and Climate Activists"
nodes_df$color[nodes_df$community == 208] <- "#CCEBC5" 
  
# Community 230   (4.508%): CDU: regional and federal party accounts, AKK & Kretschmer; "wahl_beobachter" Martin Fuchs (slightly CDU sympathetic)
nodes_df$community_name[nodes_df$community == 230] <- "CDU"
nodes_df$color[nodes_df$community == 230] <- "#1A1A1A"

# Community 216   (4.046%): National News Outlets: SZ, Spiegelonline, Tagesschau, ZDF; also: right-wing media newscompact
nodes_df$community_name[nodes_df$community == 216] <- "Supra-Regional News Outlets"
nodes_df$color[nodes_df$community == 216] <- "#FF7F00"
  
# Community 52    (3.939%): Green Party: state and federal party acoounts, candidates
nodes_df$community_name[nodes_df$community == 52] <- "Green Party"
nodes_df$color[nodes_df$community == 52] <- "#33A02C"

# Community 202   (3.472%): SPD: party accounts and candidates
nodes_df$community_name[nodes_df$community == 202] <- "SPD"
nodes_df$color[nodes_df$community == 202] <- "#E31A1C"

# Community 255   (3.011%): Polls and Election Observers: DW Editor; Political Scientist Julia Schulte-Cloos; wahlrecht_de, pollofpolls_EU...
nodes_df$community_name[nodes_df$community == 255] <- "Election Observers"
nodes_df$color[nodes_df$community == 255] <- "#8DD3C7"

# Community 212   (2.122%): International Nationalists: french right-wing accounts (incl. Marie Le Pen); Junge Alternative; spanish news outlet puntual24h reporting on the election; dutch alternative media / right-wing journalist sbergsma 
    # SW19_tweets_comm[SW19_tweets_comm$community=="212" & !is.na(SW19_tweets_comm$community),] %>% View()  
    # -> mostly non-german (esp. french) tweets
nodes_df$community_name[nodes_df$community == 212] <- "International Nationalists"
nodes_df$color[nodes_df$community == 212] <- "#999999"

# Community 232   (1.511%): ironic commentators: satire (ddroffiziell) & ironic accounts; strictly speaking apolitical
nodes_df$community_name[nodes_df$community == 232] <- "Ironic Commentators"
nodes_df$color[nodes_df$community == 232] <- "#FCCDE5"
  
# Community 256   (1.419%): Humanists and Pirates: Humanist Party (+ saxony chapter and their chairman); Pirate Party Saxony; European Centre for Press and Media Freedom
nodes_df$community_name[nodes_df$community == 256] <- "Humanists and Pirates"
nodes_df$color[nodes_df$community == 256] <- "#80B1D3"

# Community 277   (1.327%): ZEIT online and its authors
nodes_df$community_name[nodes_df$community == 277] <- "ZEIT online"
nodes_df$color[nodes_df$community == 277] <- "#FDB462"

# Community 264   (1.084%): Liberals: FDP and candidates; Creative Agency CROMATICS with their mural on the elctions; VW show factory Gläserner Manufaktur in Dresden
nodes_df$community_name[nodes_df$community == 264] <- "Liberals"
nodes_df$color[nodes_df$community == 264] <- "#FFFF33"

# Dummy: "None"
nodes_df$community_name[nodes_df$community == 0] <- "None"
nodes_df$color[nodes_df$community == 0] <-"#D9D9D9"

## Add Community Data to Mentions Graph 
V(mentions_graph)$community <- nodes_df$community
V(mentions_graph)$community_name <- nodes_df$community_name
V(mentions_graph)$color <- nodes_df$color



### Gephi Export ---------

## add party name as node attribtue for easier visualization
parties <- tibble(party = c("afd",
                            "spdde",
                            "spdsachsen",
                            "cdu",
                            "cdusachsen",
                            "die_gruenen",
                            "gruenesachsen",
                            "dielinke",
                            "linke_sachsen",
                            "csu",
                            "piratenpartei",
                            "piraten_sn",
                            "fdp",
                            "fdpsachsen",
                            "npdde",
                            "diepartei",
                            "partei_sachsen"
))

nodes_df$party <- ifelse(V(mentions_graph)$name%in%parties$party, V(mentions_graph)$name, NA)


## Display labels, depending on PageRank
  # display of PageRank at least 0.005 (of 1). 19 Labels to display
nodes_df$label <- ifelse(nodes_df$pagerank>=0.005, nodes_df$full_name, NA)
  # remove unicodes in labels, remove &, ?, / (problems for xml writing)
nodes_df$label[nodes_df$NAME=="afd"] <- "Alternative fuer Deutschland"
nodes_df$label[nodes_df$NAME=="die_gruenen"] <- "BUENDNIS90/DIE GRUENEN"
nodes_df$label[nodes_df$NAME=="gruene_sachsen"] <- "B'90/GRUENE Sachsen"
nodes_df$label[nodes_df$NAME=="joerg_urbanafd"] <- "Joerg Urban"
nodes_df$label[nodes_df$NAME=="groenemeyer"] <- "Herbert Groenemeyer"
nodes_df$label[nodes_df$NAME=="unteilbar_"] <- "unteilbar Solidaritaet statt Ausgrenzung"
nodes_df$label <- iconv(nodes_df$label, to="utf-8")

save(nodes_df, edges_df, file="SW19_Mentions_NodesEdges.RDa")


## Export Mentions Graph 
write.gexf(nodes = nodes_df[1:2], 
           edges = edges_df[1:2], 
           nodesAtt = select(nodes_df, -starts_with("ID"), -NAME, -full_name), 
           edgesAtt = edges_df[3:length(edges_df)], 
           defaultedgetype = "directed", 
           output = "SW19_mentions_graph.gexf")



## Merge community information to twitter data  --------------------
SW19_tweets_comm <- sachsenwahl_tweets

SW19_tweets_comm$screen_name <- tolower(SW19_tweets_comm$screen_name)
SW19_tweets_comm <- left_join(SW19_tweets_comm, nodes_df[,c("NAME","community", "community_name")], by = c("screen_name" = "NAME") )
SW19_tweets_comm <- as.data.table(SW19_tweets_comm)
    # Users that do not reply / retweet have no community information. 
    # Users that are not in the sample but appear as nodes in graph (due to being retweetet etc.) are not added to the twitter data

SW19_tweets_comm$community %>% is.na() %>% sum()    # 2371 tweets without community classification (1.5%)
# SW19_tweets_comm[is.na(SW19_tweets_comm$community),] %>% View()
    # tweets with low favorite / retweet counts OR
    # tweets by banned accounts ("account has been withheld in Germany based on local law(s).")

# Dummy variable 0/"None" for Tweets w/o community classification / not in top communities
SW19_tweets_comm$community[is.na(SW19_tweets_comm$community)] <- 0
SW19_tweets_comm$community_name[is.na(SW19_tweets_comm$community_name)] <- "None"


# Crossing?
SW19_tweets_comm <- left_join(SW19_tweets_comm, 
                              select(unique(as.data.table(edges_df[order(edges_df$crossing, decreasing = T),]), by="status_id"), 
                                     status_id, crossing)) # complicated query since unique() only works for data.tables
  # -> for multiple interactions in one tweet (e.g. replies, mentioning of several users) treat as crossing when in doubt

SW19_tweets_comm <- as.data.table(SW19_tweets_comm)

save(SW19_tweets_comm, file="SW19_tweets_comm.RDa")



### Visualize collapsed community graph  --------------

# Collapse communities

communities <- nodes_df %>% group_by(community, community_name, color) %>% summarise(size = n())

# Sum retweets, quotes, replies and mentions between communities
community_edges <- edges_df %>% group_by(V1_membership, 
                                         V2_membership, 
                                         is_retweet,
                                         is_quote,
                                         is_reply,
                                         is_mention) %>% 
                                summarise(n = n())

community_edges$interaction <- ifelse(community_edges$is_retweet, "Information Diffusion",
                                      ifelse(community_edges$is_reply |community_edges$is_quote | 
                                               community_edges$is_mention, "Deliberation", NA))

community_edges <- community_edges %>% group_by(V1_membership,
                                                V2_membership,
                                                interaction) %>% 
                                        summarise(n = sum(n))


## Chord Diagram

# adjust edges frame
community_edges <- left_join(community_edges, communities, by=c("V1_membership" = "community"))
community_edges <- left_join(community_edges, communities, by=c("V2_membership" = "community"))

community_edges_2 <- data.frame(V1_community = community_edges$community_name.x, 
                                V2_community = community_edges$community_name.y,
                                n = community_edges$n,
                                interaction = community_edges$interaction)


# intra-community and interaction in communities object
intra <- community_edges_2[(community_edges_2$V1_community==community_edges_2$V2_community),]

communities <- left_join(communities, intra[intra$interaction=="Deliberation", c(1,3)],
                         by = c("community_name" = "V1_community"))
names(communities)[names(communities)=="n"] <- "intra_deliberation"

communities <- left_join(communities, intra[intra$interaction=="Information Diffusion", c(1,3)],
                         by = c("community_name" = "V1_community"))
names(communities)[names(communities)=="n"] <- "intra_diffusion"



### visualize with circos


## Deliberation
deliberation <- community_edges_2[community_edges_2$interaction=="Deliberation",]

transparency <- ifelse(deliberation[,1]==deliberation[,2],1,0.5) # transparency: makes self-links invisible


circos.clear()

chordDiagram(deliberation[c(1:3)], 
             annotationTrack = NULL, # c("grid", "axis"),
             preAllocateTracks = list(track.height = 0.075), 
             grid.col = communities$color,
             transparency = transparency, directional = 1, 
             self.link = 1, link.sort = T, scale = F,  # self.link = 2: self-link counts out- and incoming; 1: counts only once (changes maximum!)
             direction.type = c("arrows"),
             link.arr.type = "big.arrow")

circos.track(track.index = 1, panel.fun = function(x, y) {
  i = get.cell.meta.data("sector.numeric.index")
  xlim = get.cell.meta.data("xlim")  # all in- and outgoing for sector (i.e. community) 
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  xplot = get.cell.meta.data("xplot")
  
  #label
  circos.text(CELL_META$xcenter, CELL_META$ylim[2]+0.5, CELL_META$sector.index, 
              facing = "clockwise", niceFacing = T, adj = c(0, 0),
              cex = 1.5)
  
  # border plots
  circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2],
              col = communities$color[i], border = communities$color[i])
  # white bits (share of intra-community interaction)
  circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[1]+communities$intra_deliberation[i], ytop=ylim[1]+0.5, 
              col = "white", border = communities$color[i])   #!! Figure out how to represent share of intra- / inter interaction !! 
  
  #white line all the way around
  circos.rect(xleft=xlim[1], ybottom=0.5, xright=xlim[2], ytop=0.55, col = "white", border = "white")
  
  #plot axis
  breaks = seq(from=0,to=(xlim[2]+1000), by=1000)
  circos.axis(labels.cex=0.8, labels.facing = "out",
              major.at=breaks, 
              labels = c("",paste0("",breaks[-1]/1000, "k")),
              minor.ticks=4, labels.away.percentage = 0.5,
              labels.pos.adjust = F)
  
}, bg.border = NA)



## Information Diffusion

diffusion <- community_edges_2[community_edges_2$interaction=="Information Diffusion",]

transparency <- ifelse(diffusion[,1]==diffusion[,2],1,0.5) # transparency: makes self-links invisible


circos.clear()

chordDiagram(diffusion[c(1:3)], 
             annotationTrack = NULL, # c("grid", "axis"),
             preAllocateTracks = list(track.height = 0.075), 
             grid.col = communities$color,
             transparency = transparency, directional = 1, 
             self.link = 1, link.sort = T, scale = F,  # self.link = 2: self-link counts out- and incoming; 1: counts only once (changes maximum!)
             direction.type = c("arrows"),
             link.arr.type = "big.arrow")

circos.track(track.index = 1, panel.fun = function(x, y) {
  i = get.cell.meta.data("sector.numeric.index")
  xlim = get.cell.meta.data("xlim")  # all in- and outgoing for sector (i.e. community) 
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  xplot = get.cell.meta.data("xplot")
  
  #label
  circos.text(CELL_META$xcenter, CELL_META$ylim[2]+0.5, CELL_META$sector.index, 
              facing = "clockwise", niceFacing = T, adj = c(0, 0),
              cex = 1.5)
  
  # border plots
  circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2],
              col = communities$color[i], border = communities$color[i])
  # white bits (share of intra-community interaction)
  circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[1]+communities$intra_diffusion[i], ytop=ylim[1]+0.5, 
              col = "white", border = communities$color[i])   #!! Figure out how to represent share of intra- / inter interaction !! 
  
  #white line all the way around
  circos.rect(xleft=xlim[1], ybottom=0.5, xright=xlim[2], ytop=0.55, col = "white", border = "white")
  
  #plot axis
  breaks = seq(from=0,to=(xlim[2]+5000), by=5000)
  circos.axis(labels.cex=0.8, labels.facing = "out",
              major.at=breaks, 
              labels = c("",paste0("",breaks[-1]/1000, "k")),
              minor.ticks=4, labels.away.percentage = 0.15,
              labels.pos.adjust = F)
  
}, bg.border = NA)





### Additional Tables ---------

## Influential users in communities
influential_users <- subset(nodes_df, subset=(nodes_df$community %in% communities_RT_top$community)) %>% 
                     group_by(community) %>% top_n(5, pagerank_community) %>% select(full_name, NAME, followers, pagerank_community, pagerank, community_name)
write.xlsx(as.data.frame(influential_users), "influential_users.xlsx")


## Legend for the Mentions Graph
legend <- communities[2:4]
legend$percent <- percent(legend$size / sum(legend$size))
#legend$label <- paste(legend$percent, legend$community_name)
legend <- legend[order(legend$size, decreasing = T),]
write.xlsx(legend, "mentions_graph_legend.xlsx")


## Interaction between communities

interaction <- deliberation[1:3]
names(interaction)[names(interaction)=="n"] <- "Deliberation"
interaction <- left_join(interaction, diffusion[1:3])
names(interaction)[names(interaction)=="n"] <- "Information Diffusion"
names(interaction)[1:2] <- c("From", "To")
write.xlsx(interaction, "community_interaction_table.xlsx")


