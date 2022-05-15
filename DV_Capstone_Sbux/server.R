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
  
  ##Filter Column 1
  observeEvent(input$cat1,{
    updateSelectInput(session,'pro1',
                      choices=unique(sbux_new$product_name[sbux_new$category == input$cat1]))
  }) 
  
  observeEvent(input$pro1,{
    updateSelectInput(session,'milk1',
                      choices=unique(sbux_new$milk[sbux_new$category == input$cat1 & sbux_new$product_name == input$pro1]))
  }) 
  
  observeEvent(input$milk1,{
    updateSelectInput(session,'whip1',
                      choices=unique(sbux_new$whip[sbux_new$category == input$cat1 & sbux_new$product_name == input$pro1 & sbux_new$milk == input$milk1]))
  }) 
  
  observeEvent(input$whip1,{
    updateSelectInput(session,'size1',
                      choices=unique(sbux_new$size[sbux_new$category == input$cat1 & sbux_new$product_name == input$pro1 & sbux_new$milk == input$milk1 & sbux_new$whip == input$whip1]))
  }) 
  
  ##Filter Column 2
  observeEvent(input$cat2,{
    updateSelectInput(session,'pro2',
                      choices=unique(sbux_new$product_name[sbux_new$category == input$cat2]))
  }) 
  
  observeEvent(input$pro2,{
    updateSelectInput(session,'milk2',
                      choices=unique(sbux_new$milk[sbux_new$category == input$cat2 & sbux_new$product_name == input$pro2]))
  }) 
  
  observeEvent(input$milk2,{
    updateSelectInput(session,'whip2',
                      choices=unique(sbux_new$whip[sbux_new$category == input$cat2 & sbux_new$product_name == input$pro2 & sbux_new$milk == input$milk2]))
  }) 
  
  observeEvent(input$whip2,{
    updateSelectInput(session,'size2',
                      choices=unique(sbux_new$size[sbux_new$category == input$cat2 & sbux_new$product_name == input$pro2 & sbux_new$milk == input$milk2 & sbux_new$whip == input$whip2]))
  })
  
  ## Comparison Plot
  
  output$comparison <- renderPlotly({
    
    beverage1 <- sbux_new %>% 
      filter(category == input$cat1) %>% 
      filter(product_name == input$pro1) %>% 
      filter(milk == input$milk1) %>%
      filter(whip == input$whip1) %>% 
      filter(size == input$size1) %>% 
      select(-c(size,milk,whip,serv_size_m_l,category)) %>% 
      pivot_longer(cols = c(calories,total_fat_g,saturated_fat_g, trans_fat_g, cholesterol_mg, sodium_mg, total_carbs_g, fiber_g, sugar_g, caffeine_mg), names_to = 'nutrition')
    
    
    beverage2<- sbux_new %>% 
      filter(category == input$cat2) %>% 
      filter(product_name == input$pro2) %>% 
      filter(milk == input$milk2) %>%
      filter(whip == input$whip2) %>% 
      filter(size == input$size2) %>% 
      select(-c(size,milk,whip,serv_size_m_l,category)) %>% 
      pivot_longer(cols = c(calories,total_fat_g,saturated_fat_g, trans_fat_g, cholesterol_mg, sodium_mg, total_carbs_g, fiber_g, sugar_g, caffeine_mg), names_to = 'nutrition') 
    
    comp_beverage <- bind_rows(beverage1, beverage2)
    
    comp <- ggplot(comp_beverage, aes(x = nutrition, y = value, fill = product_name)) +
      geom_bar(stat="identity", position="dodge", width=0.8) +
      theme_minimal() +
      theme(legend.position = 'none') +
      ylab('') +
      xlab('') +
      scale_x_discrete(breaks = c("caffeine_mg",
                                  "calories",
                                  "cholesterol_mg",
                                  "fiber_g",
                                  "saturated_fat_g",
                                  "sodium_mg",
                                  "sugar_g",
                                  "total_carbs_g",
                                  "total_fat_g",
                                  "trans_fat_g"),
                       labels = c("Caffeine",
                                  "Calories",
                                  "Cholesterol",
                                  "Fiber",
                                  "Saturated Fat",
                                  "Sodium",
                                  "Sugar",
                                  "Total Carbs",
                                  "Total Fat",
                                  "Trans Fat")) +
      labs(title = glue('{input$pro1} vs {input$pro2}')) +
      theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(axis.text.y = element_blank()) +
      scale_fill_manual(values=c("#00704A", "#6f4e37"))
    
    ggplotly(comp)
  })
  #Halaman Ketiga
  output$table <- DT::renderDataTable({
    DT::datatable(sbux_new)
  })
  
}