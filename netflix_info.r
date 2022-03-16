library(tidyverse)
library(readr)
library(ggplot2)
library(tibble)
library(lubridate)

#Load Data
netflix_info <- read_csv("netflix_titles.csv", show_col_types = 'FALSE')

#Preview Data
glimpse(netflix_info)
summary(netflix_info)

#Separate into Three Category Columns & Keep the First
netflix_info <- netflix_info %>% separate(listed_in, c("Category1", "Category2", "Category3"), sep=",")
netflix_info <- netflix_info %>% select(c(-"Category2", -"Category3"))

#Separate Date Added (into Date & Year)
netflix_info <- netflix_info %>% separate(date_added, c("date_added", "year_added"), sep=",")

#GG Plot For Films Released By Year (Separated by Type: Movie / TV Show)
ggplot(netflix_info, mapping= aes(x=release_year, fill=type)) +
  geom_histogram(color = "black", binwidth=2.5) +
  labs(title="Netflix TV Shows/Movies Released by Year", x="Release Year", y="Total Films")
  
#Updating Year Added to Numeric Value
netflix_info$year_added <- as.numeric(netflix_info$year_added)

#Updating GG Plot
ggplot(netflix_info, mapping= aes(x=release_year, fill=type)) +
  geom_histogram(color = "black", binwidth=2.5) +
  labs(title="Netflix: Films Added by Year", x="Year", y="Total Films")+

#Creating New Data Frame for Film Types
netflix_types <- netflix_info %>% group_by(type) %>% count() %>% ungroup() %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

head(netflix_types)

#GG Plot for Pie Chart % Movie vs. TV Show
ggplot(netflix_types, aes(x="", y = perc, fill= type)) +
  geom_col()+
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title="Netflix Film Type: Movies vs TV Shows") +
  theme_void() +

#Netflix Films by Genre's
genre1 <- netflix_info %>% group_by(Category1) %>% count()

#Creating Top 9 Genres + Combining "Others": Into a Top 10 Genres DataFrame
top9_genres <- filter(genre1, n > 300) %>% arrange(desc(n))
other_genres <- filter(genre1, n < 300)

other_genres <- data.frame(Category1 = c("Others*"), n=c(sum(other_genres$n)))
top10_genres <- rbind(top9_genres, other_genres)

#Preview Genres DataFrame
head(top10_genres)

#GGPlot For Top 10 Genres
ggplot(data = top10_genres, mapping=aes(x=reorder(Category1, n), y=n, fill=Category1))+
  geom_bar(stat = "identity", show.legend=FALSE)+
  coord_flip()+
  labs(title = "Netflix: Top 10 Genres", x = "Genres", y = "Total Films", caption = "Others: Anime, British, Classic, Docuseries, Horror, International, etc.")  

#Netflix Films by Ratings
ratings <- netflix_info %>% group_by(rating) %>% count()

#Adding Top 9 Ratings + Combining "Others": Into a Top 10 Ratings
top9_ratings <- filter(ratings, n > 200) %>% arrange(desc(n))
other_ratings <- filter(ratings, n < 200)

other_ratings <- drop_na(other_ratings)
other_ratings <- data.frame(rating = c("Others*"), n = c(sum(other_ratings$n)))
top10_ratings <- rbind(top9_ratings, other_ratings)

#Preview Ratings DataFrame
head(top10_ratings)

#GGPlot For Top 10 Ratings
ggplot(data = top10_ratings, aes(x = reorder(rating, n), y=n, fill=rating)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title= "Netflix: Top 10 Ratings", x="Ratings", y="Total Films", caption = "Others: G, NR, NC-17,  TV-Y7-FV, UR, etc.")


#Netflix Films By Country
countries <- netflix_info %>%
  select(country, type, title, year_added)
sum(is.na(countries$country))/nrow(countries)

countries <- countries %>%
  filter(!is.na(country))

#Split Countries into Single Country
max(str_count(countries$country, ','))
countr <- countries %>%
  separate(country, into =c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l')
           ,", ", convert = TRUE)
countr <-countr[,1:12]
countr_list <- countr %>%
  unlist()
countr_tibble <- tibble(
  country_name=countr_list
)

#Create Country Tibble
ctr <- countr_tibble %>%
  group_by(country_name) %>%
  count() %>%
  filter(!is.na(country_name))

#GGPlot for Films by Country
ctr %>%
  filter(n > 100 && country_name != '') %>%
  ggplot(aes(n, reorder(country_name, FUN=median, n), fill = n > 1000)) +
  geom_bar(stat='identity', show.legend = F) +
  labs(title="Netflix: Films Created by Country", x="Total Films", y="Country Name")
