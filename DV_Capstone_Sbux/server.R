function(input, output, session) {
  
  #Halaman Pertama
  output$plot1 <- renderPlotly({ 
    
    judul1 <- ifelse(input$input_nutrition == 'calories', print('Calories'), 
                     ifelse(input$input_nutrition == 'total_fat_g', print('Total Fat'),
                            ifelse(input$input_nutrition == 'saturated_fat_g', print('Saturated Fat'),
                                   ifelse(input$input_nutrition == 'trans_fat_g', print('Trans Fat'),
                                          ifelse(input$input_nutrition == 'cholesterol_mg', print('Cholesterol'),
                                                 ifelse(input$input_nutrition == 'sodium_mg', print('Sodium'),
                                                        ifelse(input$input_nutrition == 'total_carbs_g', print('Total Carbs'),
                                                               ifelse(input$input_nutrition == 'fiber_g', print('Fiber'),
                                                                      ifelse(input$input_nutrition == 'sugar_g', print('Sugar'), print('Caffeine'))))))))))  
    
    lab1 <- ifelse(input$input_nutrition == 'calories', print('Calories (kcal)'), 
                   ifelse(input$input_nutrition == 'total_fat_g', print('Total Fat (g)'),
                          ifelse(input$input_nutrition == 'saturated_fat_g', print('Saturated Fat (g)'),
                                 ifelse(input$input_nutrition == 'trans_fat_g', print('Trans Fat (g)'),
                                        ifelse(input$input_nutrition == 'cholesterol_mg', print('Cholesterol (mg)'),
                                               ifelse(input$input_nutrition == 'sodium_mg', print('Sodium (mg)'),
                                                      ifelse(input$input_nutrition == 'total_carbs_g', print('Total Carbs (g)'),
                                                             ifelse(input$input_nutrition == 'fiber_g', print('Fiber (g)'),
                                                                    ifelse(input$input_nutrition == 'sugar_g', print('Sugar (g)'), print('Caffeine (mg)')))))))))) 
    
    sat1 <- ifelse(input$input_nutrition == 'calories', print('kcal'), 
                   ifelse(input$input_nutrition == 'total_fat_g', print('g'),
                          ifelse(input$input_nutrition == 'saturated_fat_g', print('g'),
                                 ifelse(input$input_nutrition == 'trans_fat_g', print('g'),
                                        ifelse(input$input_nutrition == 'cholesterol_mg', print('mg'),
                                               ifelse(input$input_nutrition == 'sodium_mg', print('mg'),
                                                      ifelse(input$input_nutrition == 'total_carbs_g', print('g'),
                                                             ifelse(input$input_nutrition == 'fiber_g', print('g'),
                                                                    ifelse(input$input_nutrition == 'sugar_g', print('g'), print('mg'))))))))))     
    
    top_10_sbux <- sbux_new %>% 
      filter(category == input$input_category) %>%
      dplyr::group_by(product_name) %>% 
      dplyr::summarise(avg = mean((!!as.symbol(input$input_nutrition)))) %>% 
      ungroup() %>% 
      arrange(desc(avg)) %>% 
      head(10) %>% 
      mutate(label = glue('Product Name: {product_name}
                      Average {judul1} : {scales::comma(avg)} {sat1}'))
    
    plot1 <-  ggplot(top_10_sbux, aes(x = avg, y = reorder(product_name, avg), text = label)) +
      geom_point(size = 3, color="#00704A") +
      geom_segment(aes(x = 0, xend = avg, yend = product_name), size = 1, color = "#00704A") +
      scale_fill_gradient(low = "green", high = "black") +
      labs(y = NULL, x = glue('Average {lab1}'), title = glue('Top 10 {judul1} in {input$input_category} Category')) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 23))+
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.25)) 
    
    ggplotly(plot1, tooltip = 'text')                   
    
  })
  
  
  output$plot2 <- renderPlotly({
    
    judul2 <- ifelse(input$input_nutrition == 'calories', print('Calories'),
                     ifelse(input$input_nutrition == 'total_fat_g', print('Total Fat'),
                            ifelse(input$input_nutrition == 'saturated_fat_g', print('Saturated Fat'),
                                   ifelse(input$input_nutrition == 'trans_fat_g', print('Trans Fat'),
                                          ifelse(input$input_nutrition == 'cholesterol_mg', print('Cholesterol'),
                                                 ifelse(input$input_nutrition == 'sodium_mg', print('Sodium'),
                                                        ifelse(input$input_nutrition == 'total_carbs_g', print('Total Carbs'),
                                                               ifelse(input$input_nutrition == 'fiber_g', print('Fiber'),
                                                                      ifelse(input$input_nutrition == 'sugar_g', print('Sugar'), print('Caffeine'))))))))))
    
    lab2 <- ifelse(input$input_nutrition == 'calories', print('Calories (kcal)'),
                   ifelse(input$input_nutrition == 'total_fat_g', print('Total Fat (g)'),
                          ifelse(input$input_nutrition == 'saturated_fat_g', print('Saturated Fat (g)'),
                                 ifelse(input$input_nutrition == 'trans_fat_g', print('Trans Fat (g)'),
                                        ifelse(input$input_nutrition == 'cholesterol_mg', print('Cholesterol (mg)'),
                                               ifelse(input$input_nutrition == 'sodium_mg', print('Sodium (mg)'),
                                                      ifelse(input$input_nutrition == 'total_carbs_g', print('Total Carbs (g)'),
                                                             ifelse(input$input_nutrition == 'fiber_g', print('Fiber (g)'),
                                                                    ifelse(input$input_nutrition == 'sugar_g', print('Sugar (g)'), print('Caffeine (mg)'))))))))))
    
    sat2 <- ifelse(input$input_nutrition == 'calories', print('kcal'),
                   ifelse(input$input_nutrition == 'total_fat_g', print('g'),
                          ifelse(input$input_nutrition == 'saturated_fat_g', print('g'),
                                 ifelse(input$input_nutrition == 'trans_fat_g', print('g'),
                                        ifelse(input$input_nutrition == 'cholesterol_mg', print('mg'),
                                               ifelse(input$input_nutrition == 'sodium_mg', print('mg'),
                                                      ifelse(input$input_nutrition == 'total_carbs_g', print('g'),
                                                             ifelse(input$input_nutrition == 'fiber_g', print('g'),
                                                                    ifelse(input$input_nutrition == 'sugar_g', print('g'), print('mg'))))))))))
    
    plot2 <- sbux_new %>% 
      filter(category == input$input_category) %>% 
      #mutate(label = glue("Sugar : {sugar_g} g
      #                  Freq : {sugar_g}")) %>% 
      ggplot(aes(x = (!!as.symbol(input$input_nutrition)))) + 
      geom_histogram(fill="#00704A", color="#27251F", bins = input$input_bin) +
      theme_minimal() +
      ggtitle(glue('{judul2} Content in {input$input_category} Beverages')) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x= glue('{lab2}'), y = 'Freq')
    
    gg <- ggplotly(plot2)
    
    text_x <- number(
      gg$x$data[[1]]$x,
      prefix = "Freq: "
    )
    
    text_y <- number(
      gg$x$data[[1]]$y,
      prefix = glue("{judul2}: "),
      suffix = glue(" {sat2}")
    )
    
    gg %>%
      style(text = paste0(text_y, "</br></br>", text_x), traces = 1) 
  })
  
  #Halaman Kedua
  
  #Column 1
  output$category1_selector <- renderUI({
    
    selectInput(
      inputId = "cat1", 
      label = "Select Category",
      choices = unique(sbux_new$category))
    
  })
  
  output$product1_selector <- renderUI({
    
    agg_sbux_cat1 <- sbux_new[sbux_new$category == input$cat1, 'product_name']
    
    selectInput(
      inputId = "pro1", 
      label = "Select Product",
      choices = unique(agg_sbux_cat1$product_name))
    
  })
  
  output$milk1_selector <- renderUI({
    
    agg_sbux_pro1 <- agg_sbux_cat1[agg_sbux_cat1$product_name == input$pro1, 'milk']
    
    selectInput(
      inputId = 'milk1',
      label = 'Select Milk',
      choices = unique(agg_sbux_pro1$milk)
    )
  
  })
  
  output$whip1_selector <- ({
    agg_sbux_milk1 <- agg_sbux_pro1[agg_sbux_pro1$milk == input$milk1, 'whip']
    
    selectInput(
      inputId = 'whip1',
      label = 'Select Whip',
      choices = unique(agg_sbux_milk1$whip)
    )
  })
  #Halaman Ketiga
  output$table <- DT::renderDataTable({
    DT::datatable(sbux_new)
  })
  
  output$footer <- renderUI({
    includeHTML('footer.html')
  })
}