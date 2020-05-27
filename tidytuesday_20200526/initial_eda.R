
library("tidyverse")
library("scales")
library("patchwork")
library("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-05-26')

cocktails <- tuesdata$cocktails

cocktails %>% View()

table(cocktails$category)

cocktails %>% filter(drink == "1-900-FUK-MEUP") %>% View()

# -----------
# upset plot
# first get most common ingredients
cocktails %>% 
  group_by(ingredient) %>% summarize(n = n()) %>% 
  arrange(desc(n)) %>% slice(1:12) %>% 
  ggplot(aes(x = fct_reorder(ingredient,n), y = n)) +
  geom_col() +
  coord_flip() 
  
top_ingredients <- cocktails %>% 
  group_by(ingredient) %>% summarize(n = n()) %>% 
  arrange(desc(n)) %>% slice(1:12) %>% 
  select(ingredient)

x<-cocktails %>% 
  # inner_join(top_ingredients) %>% 
  select(drink,ingredient) %>% 
  group_by(ingredient) %>% nest() %>% 
  # mutate(n = map_dbl(data,nrow)) %>% 
  # filter(n > 1) %>% select(-n) %>% 
  unnest(cols = c(data))

y <- split(x$drink, x$ingredient)


UpSetR::upset(UpSetR::fromList(y))

UpSetR::upset(UpSetR::fromList(y), order.by = "freq")

# lemon juice by itself
cocktails %>% 
  group_by(drink) %>% summarize(n = n()) %>% 
  filter(n == 1)


# above not working; pivot wider and make binary matrix

x<-cocktails %>% 
  select(drink,ingredient) %>% 
  mutate(b = 1) %>% 
  pivot_wider(names_from = ingredient, values_from = b, values_fill = 0, values_fn = length) %>% 
  select(-drink) %>%
  as.data.frame()

x %>% glimpse()

x[x>1] <- 1

UpSetR::upset(x, nsets = 7, 
              main.bar.color = "SteelBlue", 
              sets.bar.color = "DarkCyan",
              text.scale = c(rep(1.4, 5), 1), 
              order.by = "freq")




# get a de-duplicated list of all followers
aRdent_followers <- unique(followers$user_id)

# for each follower, get a binary indicator of whether they follow each tweeter or not and bind to one dataframe
binaries <- rstaters %>% 
  map_dfc(~ ifelse(aRdent_followers %in% filter(followers, account == .x)$user_id, 1, 0) %>% 
            as.data.frame) # UpSetR doesn't like tibbles

# set column names
names(binaries) <- rstaters



# get a de-duplicated list of all followers
aRdent_followers <- unique(cocktails$ingredient)

# for each follower, get a binary indicator of whether they follow each tweeter or not and bind to one dataframe
binaries <- cocktails %>% 
  map_dfc(~ ifelse(aRdent_followers %in% filter(followers, account == .x)$user_id, 1, 0) %>% 
            as.data.frame) # UpSetR doesn't like tibbles

# set column names
names(binaries) <- rstaters



lt = list(set1 = c("a", "b", "c"),
          set2 = c("b", "c", "d", "e"))

UpSetR::upset(UpSetR::fromList(lt))

# sequence analysis for ingredients


# simple viz

cocktails %>% glimpse()

# distribution of number of ingredients



# most common ingredients

cocktail_bar_plot <- function(y, title){
  cocktails %>% 
    group_by({{y}}) %>% summarize(n = n()) %>% 
    arrange(desc(n)) %>% slice(1:12) %>% 
    mutate(pct = n/sum(n)) %>% 
    ggplot(aes(x = fct_reorder({{y}},n), y = n)) +
    geom_col(aes(fill = n)) +
    geom_label(aes(label = percent(pct, accuracy = 1)), fill = "#c7673e") +
    coord_flip() +
    ggthemes::theme_fivethirtyeight() +
    ggthemes::scale_fill_continuous_tableau(palette = "Blue") +
    guides(fill = FALSE)+
    labs(title = title)
}

p1 <- cocktail_bar_plot(ingredient,"Ingredients")
p2 <-cocktail_bar_plot(iba,"IBA")
p3 <-cocktail_bar_plot(alcoholic,"Alcoholic")
p4 <-cocktail_bar_plot(category,"Category")
p5 <-cocktail_bar_plot(glass,"Glass")

p1+p3+p4+p5

# cross tab

cocktails %>% 
  group_by(ingredient,glass) %>% tally() %>% 
  filter(n > 10) %>% 
  ggplot(aes(x = glass, y = ingredient)) +
  geom_tile(aes(fill = n)) +
  geom_text(aes(label = n)) +
  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_fill_continuous_tableau(palette = "Blue") +
  labs(x = "", y = "") +
  guides(fill = FALSE)

# number of ingredients
cocktails %>% 
  group_by(drink) %>% mutate(mx = max(ingredient_number)) %>% 
  group_by(mx) %>% tally() %>% 
  ggplot(aes(x = as.factor(mx), y = n)) +
  geom_col(aes(fill = n)) +
  geom_label(aes(label = n), fill = "#c7673e") +
  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_fill_continuous_tableau(palette = "Blue") +
  guides(fill = FALSE) +
  labs(title = "Number of Ingredients")

# upset plot

# most common combos by 

di <- cocktails %>% select(drink,ingredient)

di %>%
  group_by(ingredient) %>%
  filter(n() > 1) %>%
  split(.$ingredient) %>%
  map(., 1) %>%
  map(~combn(.x, m = 2)) %>%
  map(~t(.x)) %>%
  map_dfr(as_tibble) %>% print(n = 100)


di %>% 
  group_by(drink) %>% 
  expand(ingredient)



library("tidytext")

x <- cocktails %>% select(category,drink,ingredient) %>% 
  mutate(ingredient = str_replace_all(ingredient, " ", "_")) %>% 
  group_by(category, drink) %>% 
  mutate(ingredient_list = paste0(ingredient, collapse = " ")) %>% 
  ungroup() %>% 
  distinct(drink,.keep_all = T)


library(dplyr)
library(tidytext)
library(janeaustenr)

x %>% 
  unnest_tokens(bigram, ingredient_list, token = "ngrams", n = 2) %>% filter(str_detect(bigram,"vodka") == TRUE) %>% View()
  group_by(bigram) %>% tally() %>% 
  filter(n > 3) %>% 
  ggplot(aes(x = fct_reorder(bigram,n), y = n)) +
  geom_col(aes(fill = n)) +
  # geom_label(aes(label = percent(pct, accuracy = 1)), fill = "#c7673e") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_fill_continuous_tableau(palette = "Blue") +
  guides(fill = FALSE)

  
fausten_bigrams









