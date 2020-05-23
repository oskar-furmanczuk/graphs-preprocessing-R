# Function to parse text into vectors

readInGraph <- function(path){
  con <- file(path, open='r')
  text <- readLines(con, warn = FALSE)
  outter_list <- list()
  for (i in 1:length(text)){
    line <- text[[i]]
    without_br <-  gsub('(\\{|\\})','', line)
    vec_char <- strsplit(without_br, ", ")
    vec_double <- as.numeric(unlist(vec_char))
    node <-  vec_double[[1]]
    connections <-  vec_double[-1] 
    inner_list <- list(node = node, connections = connections)
    outter_list[[node]] <- inner_list
  }
  return(outter_list)
}

# Example
l_graph <- readInGraph("graph_small.dat")

df <- data.frame( node = 1:length(l_graph),
                  "number_of_connections" = unlist( lapply( X = l_graph,
                                                            FUN = function( s){
                                                              length(s$connections)}
                  )))
head(df)

# Simple Pie Chart
lbls <- paste("node_number:", as.character(unlist(as.character(unlist((df$node))))))
pie(df$number_of_connections, main="Pie Chart of Number of Connections", labels=lbls)
