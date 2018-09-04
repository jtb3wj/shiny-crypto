# Author: Jacob Bailey

# get whether or not we have certain packages
package.list <- list("tidyverse", "rvest", "lubridate",
                     "shiny", "ggthemes", "jsonlite",
                     "httr", "dygraphs")

# see if the packages are installed
is.installed <- sapply(package.list, require, character.only = TRUE)

# install and load and ones that aren't
sapply(package.list[!is.installed], install.packages, character.only = TRUE)
sapply(package.list[!is.installed], library, character.only = TRUE)



# Get listing of all coins
coin.listing <- 
  "https://api.coinmarketcap.com/v2/listings/" %>% 
  parse_url() %>% 
  build_url() %>% 
  fromJSON(flatten = TRUE) %>% 
  .[[1]] %>% 
  arrange(name)

coin.listing.input <- coin.listing$website_slug
names(coin.listing.input) <- coin.listing$name

