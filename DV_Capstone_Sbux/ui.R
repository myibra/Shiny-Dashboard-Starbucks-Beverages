
title <- tags$a(tags$img(src="images/logo.jpg", height = '30', width = '30'))

navbarPage(
  title = title,
  theme = 'style/style.css',
  footer = includeHTML("footer.html"),
  fluid = TRUE, 
  collapsible = TRUE,
  tabPanel('Nutrition in Starbucks Beverages',
           # Top N Plot
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = 'input_nutrition',
                             label = 'Select Nutrition',
                             choices = c('Calories'= 'calories',
                                         'Total Fat' = 'total_fat_g',
                                         'Saturated Fat' = 'saturated_fat_g',
                                         'Trans Fat' = 'trans_fat_g',
                                         'Cholesterol' = 'cholesterol_mg',
                                         'Sodium' = 'sodium_mg',
                                         'Total Carbs' = 'total_carbs_g',
                                         'Fiber' = 'fiber_g',
                                         'Sugar' = 'sugar_g',
                                         'Caffeine' = 'caffeine_mg')),
                 radioButtons(inputId = 'input_category',
                              label = 'Select Beverage Category',
                              choices = unique(sbux_new$category))),
               mainPanel(
                 plotlyOutput(outputId = 'plot1'))
             )),
           
           # Plot Hist
           fluidRow(
             sidebarLayout(
               sidebarPanel(
                 sliderInput(inputId = 'input_bin', 
                             label = 'Numbers of Bin', 
                             min = 1, 
                             max = 100,
                             value = 20)
               ),
               mainPanel(
                 plotlyOutput(outputId = 'plot2')
               )
             ))),
  tabPanel("Beverage Comparison",
           #filter
           fluidRow(
             column(width = 6,
                    h4("Beverage 1"),
                    selectInput(inputId = 'cat1',
                                label = 'Select Category',
                                choices = unique(sbux_new$category)),
                    selectInput(inputId = 'pro1',
                                label = 'Select Product',
                                choices = unique(agg_sbux_cat1$product_name))),
             column(width = 6,
                    h4("Beverage 2"))
           )),
  tabPanel("Data", DT::dataTableOutput("table")),
  

)



