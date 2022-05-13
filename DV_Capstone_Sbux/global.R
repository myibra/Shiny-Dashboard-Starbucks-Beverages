# load libraries
library(shiny)
library(shinydashboard)
library(DT)

library(tidyverse)
library(dplyr) 
library(readr) 

library(ggplot2) 
library(plotly) 
library(glue) 
library(scales) 
library(stringr)

# data preparation
sbux <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')

sbux$milk <- sbux$milk %>% 
  replace(sbux$milk == 0, 'none') %>% 
  replace(sbux$milk == 1, 'nonfat') %>%
  replace(sbux$milk == 2, '2%') %>%
  replace(sbux$milk == 3, 'soy') %>%
  replace(sbux$milk == 4, 'coconut') %>%
  replace(sbux$milk == 5, 'whole') 

sbux$whip <- sbux$whip %>% 
  replace(sbux$whip == 0, 'without whip cream') %>% 
  replace(sbux$whip == 1, 'with whip cream')

sbux_clean <- sbux %>% 
  mutate(
    product_name = as.factor(product_name),
    size = as.factor(size),
    milk = as.factor(milk),
    whip = as.factor(whip),
    trans_fat_g = as.numeric(trans_fat_g),
    fiber_g = as.numeric(fiber_g)
  )

sbux_clean$product_name <- str_to_title(sbux_clean$product_name)

cat_coffee <- c('Brewed Coffee - Dark Roast', 
                'Brewed Coffee Traveler - Dark Roast',
                'Brewed Coffee - Decaf Pike Place Roast',
                'Brewed Coffee - Medium Roast',
                'Brewed Coffee Traveler - Decaf Pike Place Roast',
                'Brewed Coffee Traveler - Medium Roast',
                'Brewed Coffee - True North Blend Blonde Roast',
                'Brewed Coffee Traveler - True North Blend Blonde Roast',
                'Caffè Misto',
                'Clover Brewed Coffee - Dark Roast',
                'Clover Brewed Coffee - Light Roast',
                "Clover Brewed Coffee - Light Roast",
                'Clover Brewed Coffee - Medium Roast',
                'Iced Coffee',
                'Iced Coffee With Milk',
                'Cold Brewed Coffee',
                'Vanilla Sweet Cream Cold Brew')

cat_espresso <- c('Caffè Americano',
                  'Iced Caffè Americano',
                  'Caffè Latte',
                  'Iced Caffè Latte',
                  'Caffè Mocha',
                  'Iced Caffè Mocha',
                  'Cappuccino',
                  'Caramel Macchiato',
                  'Iced Caramel Macchiato',
                  'Cinnamon Dolce Latte',
                  'Espresso',
                  'Iced Espresso',
                  'Espresso Con Panna',
                  'Espresso Macchiato',
                  'Flat White',
                  'Latte Macchiato',
                  'Skinny Cinnamon Dolce Latte',
                  'Iced Skinny Cinnamon Dolce Latte',
                  'Skinny Mocha',
                  'Iced Skinny Mocha',
                  'Starbucks Doubleshot On Ice',
                  'White Chocolate Mocha',
                  'Iced White Chocolate Mocha',
                  'Espresso - Caffè Americano',
                  'Espresso - Iced Caffè Americano')

cat_tea <- c('Iced Black Tea',
             'Iced Black Tea Lemonade',
             'Chai Tea Latte',
             'Iced Chai Tea Latte',
             'Earl Grey Brewed Tea',
             "Emperor's Clouds And Mist Brewed Tea",
             'English Breakfast Black Brewed Tea',
             'English Breakfast Black Tea Latte',
             'Green Tea Latte',
             'Iced Green Tea Latte',
             'Iced Green Tea',
             'Iced Green Tea Lemonade',
             'Jade Citrus Mint Brewed Tea',
             'London Fog Tea Latte',
             'Iced Mango Black Tea',
             'Iced Mango Black Tea Lemonade',
             'Mint Majesty Brewed Tea',
             'Oprah Chai Herbal Brewed Tea',
             'Oprah Cinnamon Chai Brewed Tea',
             'Oprah Cinnamon Chai Latte',
             'Iced Oprah Cinnamon Chai Latte',
             'Passion Tango Brewed Tea',
             'Iced Passion Tango tea',
             'Iced Passion Tango Tea Lemonade',
             'Peach Iced Green Tea',
             'Peach Iced Green Tea Lemonade',
             'Peach Tranquility Brewed Tea',
             'Youthberry Brewed Tea',
             "Iced Passion Tango Tea")

cat_refresher <- c('Cool Lime Starbucks Refreshers',
                   'Very Berry Hibiscus Starbucks Refreshers')

cat_smoothies <- c('Chocolate Smoothie',
                   'Orange Mango Smoothie',
                   'Strawberry Smoothie')

cat_frappuccino_blended_coffee <- c('Caffè Vanilla Frappuccino Blended',
                                    'Caramel Frappuccino Blended',
                                    'Coffee Frappuccino Blended',
                                    'Espresso Frappuccino Blended',
                                    'Java Chip Frappuccino Blended',
                                    'Mocha Frappuccino Blended')

cat_frappuccino_light_blended_coffee <-  c('Caffè Vanilla Frappuccino Light',
                                           'Caramel Frappuccino Light',
                                           'Coffee Frappuccino Light',
                                           'Espresso Frappuccino Light',
                                           'Java Chip Light Frappuccino',
                                           'Mocha Light Frappuccino')

cat_frappucino_blended_crème <- c('Blended Strawberry Lemonade',
                                  'Chai Crème Frappuccino Blended',
                                  'Double Chocolaty Chip Crème Frappuccino Blended',
                                  'Green Tea Crème Frappuccino Blended',
                                  'Oprah Cinnamon Chai Crème Frappuccino Blended',
                                  'Strawberries & Crème Frappuccino Blended',
                                  'Vanilla Bean Crème Frappuccino Blended')

cat_hot_chocolate_other <- c('Caramel Apple Spice',
                             'Hot Chocolate',
                             'Lemonade',
                             'Milk',
                             'Skinny Hot Chocolate',
                             'White Hot Chocolate')

cat_add_ons <- c('Protein & Fibre Powder',
                 'Matcha Green Tea Powder',
                 'Espresso Shot')

sbux_new <- sbux_clean %>%
  dplyr::mutate(category = case_when(
    product_name %in% cat_coffee ~ 'Coffee',
    product_name %in% cat_espresso ~ 'Espresso',
    product_name %in% cat_tea ~ 'Tea',
    product_name %in% cat_refresher ~ 'Refresher',
    product_name %in% cat_smoothies ~ 'Smoothies',
    product_name %in% cat_frappuccino_blended_coffee ~ 'Frappuccino Blended Coffee',
    product_name %in% cat_frappuccino_light_blended_coffee ~ 'Frappuccino Light Blended Coffee',
    product_name %in% cat_frappucino_blended_crème ~ 'Frappucino Blended Crème',
    product_name %in% cat_hot_chocolate_other ~ 'Hot Chocolate & Other',
    product_name %in% cat_add_ons ~ 'Add-Ons'))

sbux_new$category[is.na(sbux_new$category)] = 'Coffee'