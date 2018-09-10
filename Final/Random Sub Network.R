weights <- NA
c0 <- NA
c1 <- NA
c2 <- NA
c3 <- NA
c4 <- NA
c5 <- NA

##define the whole network.
data <- transform(na.omit(read.csv("C:/Users/Jaewoo/Desktop/Edges_Final.csv")), score = positive - negative)
e20 <- read.csv("C:/Users/Jaewoo/Desktop/edgefilter20.csv")
g20 <- graph_from_data_frame(e20, directed = F)

##from cluster 1 to 5, define subnetworks.
cl20 = cluster_louvain(g20, weights=NA)
head(membership(cl20))
V(g20)$cl = membership(cl20)
cl1<- induced_subgraph(g20, V(g20)[cl == 1])
cl2<- induced_subgraph(g20, V(g20)[cl == 2])
cl3<- induced_subgraph(g20, V(g20)[cl == 3])
cl4<- induced_subgraph(g20, V(g20)[cl == 4])
cl5<- induced_subgraph(g20, V(g20)[cl == 5])

##random subnetwork scoring system
randsubnet.score = function(data, net)
{
  nodes = sample(V(net)$name, size = sample(2:vcount(net), size = 1))
  com.nodes = cbind(t(combn(nodes, 2)))
  for(i in 1:nrow(com.nodes))
  {
    temp <- data[which(data$Source==com.nodes[i][1]),]
    if (length(which(temp$Target==com.nodes[i,2]))==0)
    {
      temp <- data[which(data$Target==com.nodes[i][1]),]
      if (length(which(temp$Source==com.nodes[i,2]))==0)
      {
        temp2 <- NA
      }
      else
        temp2 <- temp[which(temp$Source == com.nodes[i,2]),8]
    }
    else
      temp2 <- temp[which(temp$Target==com.nodes[i,2]),8]
    weights[i] <- sample(temp2, size=1)
  }
  return(mean(as.numeric(na.omit(weights))))
}

for (i in 1:100)
{
  c0[i] <- randsubnet.score(data,g20)
  c1[i] <- randsubnet.score(data,cl1)
  c2[i] <- randsubnet.score(data,cl2)
  c3[i] <- randsubnet.score(data,cl3)
  c4[i] <- randsubnet.score(data,cl4)
  c5[i] <- randsubnet.score(data,cl5)
}