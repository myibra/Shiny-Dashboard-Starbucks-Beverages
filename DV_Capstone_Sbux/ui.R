
title <- tags$a(tags$img(src="images/logo.jpg", height = '30', width = '30'))

navbarPage(
  title = title,
  theme = 'style/style.css',
  footer = includeHTML("footer.html"),
  fluid = TRUE, 
  collapsible = TRUE,
  tabPanel('Home',
           box(width = 12,
             fluidRow(
               column(width = 10, h4(textOutput('part1'))),
               column(width = 2, align = 'center', img(src = "images/Roasted_coffee_beans.jpeg", width = 100))
           )
           ),
           box(width = 12,
             fluidRow(
               column(width = 10, textOutput('part2')),

             )
           )
           ),
  tabPanel('Nutrition',
           # Top N Plot
           fluidRow(
             h1('Starbucks Beverage Nutrition Facts', align = 'center')
           ),
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
           fluidRow( align="center",
             'buat membandingkan 2 minuman'
           ),
           fluidRow(
             column(width = 6,
                    align="center",
                    h4("Beverage 1"),
                    selectInput(
                      inputId = "cat1", 
                      label = "Select Category",
                      choices = unique(sbux_new$category)),
                    selectInput(
                      inputId = "pro1", 
                      label = "Select Product",
                      choices = NULL), #unique(agg_sbux_cat1$product_name)
                    selectInput(
                      inputId = 'milk1',
                      label = 'Select Milk',
                      choices = NULL), #unique(agg_sbux_pro1$milk)
                    selectInput(
                      inputId = 'whip1',
                      label = 'Select Whip Cream',
                      choices = NULL), #unique(agg_sbux_milk1$whip)
                    selectInput(
                      inputId = 'size1',
                      label = 'Select Size',
                      choices = NULL)

                    ),
             column(width = 6,
                    align="center",
                    h4("Beverage 2"),
                    selectInput(
                      inputId = "cat2", 
                      label = "Select Category",
                      choices = unique(sbux_new$category)),
                    selectInput(
                      inputId = "pro2", 
                      label = "Select Product",
                      choices = NULL), #unique(agg_sbux_cat1$product_name)
                    selectInput(
                      inputId = 'milk2',
                      label = 'Select Milk',
                      choices = NULL), #unique(agg_sbux_pro1$milk)
                    selectInput(
                      inputId = 'whip2',
                      label = 'Select Whip Cream',
                      choices = NULL), #unique(agg_sbux_milk1$whip)
                    selectInput(
                      inputId = 'size2',
                      label = 'Select Size',
                      choices = NULL)
                    )
           ),
           fluidRow(
             plotlyOutput('comparison')
           )),
  tabPanel("Data", DT::dataTableOutput("table")),
  

)



