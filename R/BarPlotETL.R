
#' Get Top Twenty Artists Ranked by Popularity
#'
#' This function returns an interactive bar plot of the top twenty most popular
#' artists in the network.
#'
#' @param artist_id Character string of the search artist's Spotify Id
#'
#' @return bar plot
#' @export
#'
#' @examples
#' artists_popularity(artist_id = "3fMbdgg4jU18AjLCKBhRSm")

artists_popularity <- function(artist_id){

  suppressPackageStartupMessages(library(spotifyr,quietly = T))
  suppressPackageStartupMessages(library(dplyr,quietly = T))
  suppressPackageStartupMessages(library(plotly,quietly = T))
  suppressPackageStartupMessages(library(ggplot2,quietly = T))
  suppressPackageStartupMessages(library(viridis,quietly = T))


  # get related artists
  related_artists <- get_related_artists(id = artist_id,
                                         include_meta_info = TRUE)

  # get other artists that are related to the
  # artists that are related to the main artist
  other_related <- c()
  for(i in 1:nrow(related_artists$artists)){
    result <- get_related_artists(id = related_artists$artists[["id"]][i],
                                  include_meta_info = TRUE)
    other_related <- append(other_related,result)
  }

  # create nodes data.frame
  nodes <- data.frame(
    id = c(1:400),
    node = c(other_related[[1]]$name,
             other_related[[2]]$name,
             other_related[[3]]$name,
             other_related[[4]]$name,
             other_related[[5]]$name,
             other_related[[6]]$name,
             other_related[[7]]$name,
             other_related[[8]]$name,
             other_related[[9]]$name,
             other_related[[10]]$name,
             other_related[[11]]$name,
             other_related[[12]]$name,
             other_related[[13]]$name,
             other_related[[14]]$name,
             other_related[[15]]$name,
             other_related[[16]]$name,
             other_related[[17]]$name,
             other_related[[18]]$name,
             other_related[[19]]$name,
             other_related[[20]]$name),

    label = c(other_related[[1]]$name,
              other_related[[2]]$name,
              other_related[[3]]$name,
              other_related[[4]]$name,
              other_related[[5]]$name,
              other_related[[6]]$name,
              other_related[[7]]$name,
              other_related[[8]]$name,
              other_related[[9]]$name,
              other_related[[10]]$name,
              other_related[[11]]$name,
              other_related[[12]]$name,
              other_related[[13]]$name,
              other_related[[14]]$name,
              other_related[[15]]$name,
              other_related[[16]]$name,
              other_related[[17]]$name,
              other_related[[18]]$name,
              other_related[[19]]$name,
              other_related[[20]]$name),

    popularity = c(c(other_related[[1]]$popularity,
                     other_related[[2]]$popularity,
                     other_related[[3]]$popularity,
                     other_related[[4]]$popularity,
                     other_related[[5]]$popularity,
                     other_related[[6]]$popularity,
                     other_related[[7]]$popularity,
                     other_related[[8]]$popularity,
                     other_related[[9]]$popularity,
                     other_related[[10]]$popularity,
                     other_related[[11]]$popularity,
                     other_related[[12]]$popularity,
                     other_related[[13]]$popularity,
                     other_related[[14]]$popularity,
                     other_related[[15]]$popularity,
                     other_related[[16]]$popularity,
                     other_related[[17]]$popularity,
                     other_related[[18]]$popularity,
                     other_related[[19]]$popularity,
                     other_related[[20]]$popularity)),

    followers = c(c(other_related[[1]]$followers.total,
                    other_related[[2]]$followers.total,
                    other_related[[3]]$followers.total,
                    other_related[[4]]$followers.total,
                    other_related[[5]]$followers.total,
                    other_related[[6]]$followers.total,
                    other_related[[7]]$followers.total,
                    other_related[[8]]$followers.total,
                    other_related[[9]]$followers.total,
                    other_related[[10]]$followers.total,
                    other_related[[11]]$followers.total,
                    other_related[[12]]$followers.total,
                    other_related[[13]]$followers.total,
                    other_related[[14]]$followers.total,
                    other_related[[15]]$followers.total,
                    other_related[[16]]$followers.total,
                    other_related[[17]]$followers.total,
                    other_related[[18]]$followers.total,
                    other_related[[19]]$followers.total,
                    other_related[[20]]$followers.total))
  )

  # remove duplicate nodes and labels in data frame

  nodes <- distinct(nodes,node,label,popularity,followers,.keep_all = T)

  # create new data.frame for bar plot tooltip
  nodes_df <- data.frame(
    Artist = reorder(nodes$node,+nodes$popularity),
    Popularity = nodes$popularity
  )

  # select top twenty most popular nodes
  nodes_df <- head(nodes_df,20)

  # create random color fill options
  fill <- c("A","B","C","D","E","F","G","H")
  rand_fill <- sample(fill,size = 1)

  # plot top twenty most popular nodes
  ggnodes <- nodes_df |>
    ggplot()+aes(x = Artist,y = Popularity,fill = Artist)+
    geom_bar(stat = "identity")+coord_flip()+
    labs(title = "Top Twenty Most Popular Artists In Network",
         x = "Artists")+scale_fill_viridis(discrete = T,option = rand_fill)+
    scale_y_continuous(expand = c(0,0))+
    theme(axis.ticks = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 10,face = "bold"),
          plot.title = element_text(size = 15,face = "bold"),
          axis.title.x = element_text(size = 12,face = "bold"),
          axis.title.y = element_text(hjust = 0.8,size = 12,face = "bold"))

  g <- ggnodes |> ggplotly(tooltip = c("x","y")) # show only x and y aesthetic values


  return(g)

}



