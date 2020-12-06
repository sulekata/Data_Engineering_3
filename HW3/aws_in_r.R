
# Motivation --------------------------------------------------------------

# Christmas around the world
# Which are the most common Christmas traditions around the world?


# Initializing packages ---------------------------------------------------

library( tidyverse )
library( rvest )
library( data.table )
library( aws.comprehend )

# Scraping whychristmas.com -----------------------------------------------

# first scraping one page
url <- 'https://www.whychristmas.com/cultures/angola.shtml'
t <- read_html( url )

# then extracting the relative links to all the country specific pages
relative_links <- t %>% html_nodes('a') %>% html_attr( 'href' )
relative_links <- relative_links[ 11:100 ]

# creating the full links
links <- NULL 
for ( i in seq_along( relative_links ) ){
  links[ i ] <- paste0( 'https://www.whychristmas.com', relative_links[ i ])
}

# creating a function which scrapes the name of the country and the text from a given page
# and saves these into a named list
get_one_page <- function( url ) {
  tlist <- list()
  
  page <- read_html( url )
  
  country <- 
    page %>% 
    html_nodes('article h1')%>%
    html_text()
  tlist[[ 'country' ]] <- strsplit( country, 'Christmas in ', fixed = T )[[ 1 ]][ 2 ]
  
  text_list <- 
    page %>% 
    html_nodes('article p')%>%
    html_text()
  
  text <- NULL
  for ( i in text_list ){
    text <- paste( text, i )
  }
  
  tlist[[ 'text' ]] <- text
  
  return( tlist )
}

# scraping the pages of every country with the function
content <- lapply( links, get_one_page )

# writing all the scraped content into a data frame
df <- rbindlist( content, fill = T )

# cleaning the text from \n and ""
lapply( df, function(x){
  gsub("\n", "", df, fixed = TRUE)
})

# Setting up Amazon Comprehend --------------------------------------------

# latest stable version
# install.packages( "aws.comprehend", repos = c( cloudyr = "http://cloudyr.github.io/drat",
#                  getOption( "repos" ) ) )

# setting up the R - AWS connection
keyTable <- read.csv( "accessKeys.csv", header = T ) # accessKeys.csv == the CSV downloaded from AWS containing your Acces & Secret keys
AWS_ACCESS_KEY_ID <- as.character( keyTable$Access.key.ID )
AWS_SECRET_ACCESS_KEY <- as.character( keyTable$Secret.access.key )

#activating the connection
Sys.setenv( "AWS_ACCESS_KEY_ID" = AWS_ACCESS_KEY_ID,
            "AWS_SECRET_ACCESS_KEY" = AWS_SECRET_ACCESS_KEY,
            "AWS_DEFAULT_REGION" = "eu-west-1" ) 


# Using custom entity recognition ------------------------------------------

# checking the distribution of texts by length
df_hist <- data.frame( Country = df$country, Length = str_count( df$text ) )

# calculating the mean of Length
mean( df_hist$Length )

# creating a histogram
ggplot( df_hist, aes( x = Length ) ) +
  geom_histogram( fill = "darkgoldenrod" , binwidth = 250 ) +
  labs(x='Length of Text (characters)', y = 'Relative Frequency', title = 'Distribution of Texts by Their Length') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text(size = 12, face = "bold", hjust = 0.5 ) ) +
  geom_vline( xintercept = 5000, linetype = "dashed", color = "firebrick", size = 1 )

# creating a function that can split the text into smaller parts
char.segments <- function( x, segm.length ){
  char.counter <- str_count( x )
  f <- c( 1, rep( 0, segm.length - 1 ) )
  f <- cumsum( rep( f, length.out = char.counter ) )
  s <- split( unlist( strsplit( x,'' ) ), f )
  unname( sapply( s, paste, collapse = '' ) )
}

# creating an empty table for the recognized entities
df_entities <- data.frame( 'Entity' = character(), 'Country' = character() )

# filling the table using a for loop
for ( i in df$country ) {
  if ( str_count( df$text[ df$country == i ] ) < 4500 ){
    by_country <- detect_entities( df$text[ df$country == i ] )[ , 5 ]
    by_country <- unique( by_country )
    df_country <- data.frame( Entity = by_country, Country = i )
    
    df_entities <- rbind( df_entities, df_country )
  } else {
    split_text <- char.segments( df$text[ df$country == i ], 4500 )
    by_country <- detect_entities( split_text )[ , 5 ]
    by_country <- unique( by_country )
    df_country <- data.frame( Entity = by_country, Country = i )
    
    df_entities <- rbind( df_entities, df_country )
  }
}

# checking which entities appeared the most frequently in the texts
df_entities %>%
  group_by( Entity ) %>%
  summarise( n = n() ) %>%
  arrange( -n ) %>%
  head( 10 ) %>% 
  ggplot( mapping = aes( x = reorder( Entity, n ), y = n ) ) +
  geom_col( fill = 'firebrick' ) +
  coord_flip() +
  labs( y = 'Number of Appearances', x = 'Entities', title = 'Most Frequent Entities') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text(size = 12, face = "bold", hjust = 0.5 ) )

# checking how many countries have a Santa Claus tradition
df_entities %>%
  filter( Entity == 'Santa Claus') %>%
  select( Country ) %>%
  unique() %>%
  count()


# Using key phrase recognition --------------------------------------------

# creating an empty table for the recognized phrases
df_phrases <- data.frame( 'Phrase' = character(), 'Country' = character() )

# filling the table using a for loop
for ( i in df$country ) {
  if ( str_count( df$text[ df$country == i ] ) < 4500 ){
    by_country <- detect_phrases( df$text[ df$country == i ] )[ , 5 ]
    by_country <- unique( by_country )
    df_country <- data.frame( Phrase = by_country, Country = i )
    
    df_phrases <- rbind( df_phrases, df_country )
  } else {
    split_text <- char.segments( df$text[ df$country == i ], 4500 )
    by_country <- detect_phrases( split_text )[ , 5 ]
    by_country <- unique( by_country )
    df_country <- data.frame( Phrase = by_country, Country = i )
    
    df_phrases <- rbind( df_phrases, df_country )
  }
}

# changing all phrases to fully lowercase characters to avoid duplicates
df_phrases$Phrase <- tolower( df_phrases$Phrase )

# checking which phrases appeared the most frequently in the texts
df_phrases %>%
  group_by( Phrase ) %>%
  summarise( n = n() ) %>%
  arrange( -n ) %>%
  head( 10 ) %>% 
  ggplot( mapping = aes( x = reorder( Phrase, n ), y = n ) ) +
  geom_col( fill = 'darkgreen' ) +
  coord_flip() +
  labs( y = 'Number of Appearances', x = 'Phrases', title = 'Most Frequent Phrases') +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text(size = 12, face = "bold", hjust = 0.5 ) )
