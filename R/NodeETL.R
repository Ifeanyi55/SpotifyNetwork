
#' Get Network Nodes Data
#'
#' This function returns a data frame of the artists in the
#' network and their features.
#'
#' @param artist_id Character string of the search artist's Spotify Id
#'
#'
#' @return table
#' @export
#'
#' @examples
#' related_artists_nodes(artist_id = "3fMbdgg4jU18AjLCKBhRSm")

related_artists_nodes <- function(artist_id){

  suppressPackageStartupMessages(library(spotifyr,quietly = T))
  suppressPackageStartupMessages(library(dplyr,quietly = T))
  suppressPackageStartupMessages(library(reactable,quietly = T))



  # get artists related to main artist
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

  # get images
  images <- c()
  for(i in other_related){ # this loops through the list
    for(k in 1:nrow(i)){ # this loops through each table in list
      image_urls <- i$images[[k]]$url[2] # the third image is collected per row in each table
      images <- append(images,image_urls)
    }
  }

  # get genres
  genre <- c()
  for(i in (other_related)){ # this loops through each list
    for(j in 1:nrow(i)){ # this loops through each table in list
      result <- i$genres[[j]][2] # this collects the 2nd item in the vector of genres
      genre <- append(genre,result)
    }
  }

  nodes <- data.frame(
    Id = c(1:400),

    name = c(other_related[[1]]$name,
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
                    other_related[[20]]$followers.total)),

    profile = c(c(other_related[[1]]$external_urls.spotify,
                  other_related[[2]]$external_urls.spotify,
                  other_related[[3]]$external_urls.spotify,
                  other_related[[4]]$external_urls.spotify,
                  other_related[[5]]$external_urls.spotify,
                  other_related[[6]]$external_urls.spotify,
                  other_related[[7]]$external_urls.spotify,
                  other_related[[8]]$external_urls.spotify,
                  other_related[[9]]$external_urls.spotify,
                  other_related[[10]]$external_urls.spotify,
                  other_related[[11]]$external_urls.spotify,
                  other_related[[12]]$external_urls.spotify,
                  other_related[[13]]$external_urls.spotify,
                  other_related[[14]]$external_urls.spotify,
                  other_related[[15]]$external_urls.spotify,
                  other_related[[16]]$external_urls.spotify,
                  other_related[[17]]$external_urls.spotify,
                  other_related[[18]]$external_urls.spotify,
                  other_related[[19]]$external_urls.spotify,
                  other_related[[20]]$external_urls.spotify)),

    images = images,
    genre = genre
  )

  ## Remove duplicate nodes and labels in data frame

  nodes_df <- distinct(nodes,name,popularity,profile,
                       images,genre,followers,.keep_all = T)


  return(nodes_df)


}


