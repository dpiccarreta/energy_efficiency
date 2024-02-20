
server <- function(input, output, session) {
  wid1 <- reactive({
    your_dataset_auto <- medioneautogluon2
    
    result_auto <- your_dataset_auto %>%
      group_by(hv) %>%
      summarise(y = min(query))
    
    result_auto$hv[nrow(result_auto)] <- your_dataset_auto$hv[nrow(your_dataset_auto)] + 0.002
    
    get_y_value_auto <- function(x_value, df) {
      
      conditions <- list()
      
      for (i in 1:(nrow(df) - 1)) {
        condition <- list(
          min_value = df$hv[i],
          max_value = df$hv[i + 1],
          y_value = df$y[i + 1]
        )
        conditions <- c(conditions, list(condition))
      }
      
      for (condition in conditions) {
        if (x_value >= condition$min_value && x_value < condition$max_value) {
          return(condition$y_value)
        }
      }
      
      return(NA)
    }
    
    calculated_width_auto <- get_y_value_auto(input$hv, result_auto)
    #print(calculated_width_auto)
    return(((calculated_width_auto  * input$kw_hr * input$kg*input$mix_energetico )^(1/6))*48)
    print(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
  })
  trova_piu_vicino <- reactive({
    indice <- which.min(abs(medioneautogluon2$hv - input$hv))
    valore_piu_vicino <- medioneautogluon2$hv[indice]
    # print(valore_piu_vicino)
     return(valore_piu_vicino)
   
  })

  output$resultNumber1 <- renderText({
    if (!is.na(wid1())) {
      return(paste(
        "-----------Hyper Volume:", sprintf("%.3f", round(input$hv, 3)),"+/-",sprintf("%.4f", round(tut1[which(trova_piu_vicino() == medioneautogluon2$hv)[1]], 4)),"\n", "-----------",
        "---Kg. CO2: ", round((wid1()/48)^6/100, 2),
        "= ", round(13*(wid1()/48)^6/100/2.380*1000, 2), " Km---"
        
      ))
    }
  })
  
  
  
  output$notAvailableText1 <- renderText({
    if (is.na(wid1())) {
      shinyjs::runjs("shinyjs.hideImg();")
      return("Not Available")
    } else {
      shinyjs::runjs("shinyjs.showImg();")
      return(NULL)
    }
  })
  
  observe({
    if (!is.na(wid1())) {
      shinyjs::runjs(sprintf("$('#img1').attr('src', 'fuelauto.jpg');"))
      shinyjs::runjs(sprintf("$('#img1').width('%s%%');", wid1()))
      shinyjs::runjs(sprintf("$('#img1').css('margin-top', '%s%%');", 3000/(wid1())-20))
    } else {
      shinyjs::runjs(sprintf("$('#img1').attr('src', '');"))  
    }
  })
  
  
  
  
 wid2 <- reactive({
    your_dataset_auto <- medionefang2[1:nrow(medionefang2),]
    
    result_auto <- your_dataset_auto %>%
      group_by(hv) %>%
      summarise(y = min(query))
    
    result_auto$hv[nrow(result_auto)] <- your_dataset_auto$hv[nrow(your_dataset_auto)] + 0.002
    
    get_y_value_auto <- function(x_value, df) {
      
      conditions <- list()
      
      for (i in 1:(nrow(df) - 1)) {
        condition <- list(
          min_value = df$hv[i],
          max_value = df$hv[i + 1],
          y_value = df$y[i + 1]
        )
        conditions <- c(conditions, list(condition))
      }
      
      for (condition in conditions) {
        if (x_value >= condition$min_value && x_value < condition$max_value) {
          return(condition$y_value)
        }
      }
      
      return(NA)
    }
    
    calculated_width_auto <- get_y_value_auto(input$hv, result_auto)
    
    return(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
    print(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
  })
 trova_piu_vicino2 <- reactive({
   indice <- which.min(abs(medionefang2$hv - input$hv))
   valore_piu_vicino <- medionefang2$hv[indice]
   # print(valore_piu_vicino)
   return(valore_piu_vicino)
   
 })
 
 output$resultNumber2 <- renderText({
   if (!is.na(wid2())) {
     return(paste(
       "-----------Hyper Volume:", sprintf("%.3f", round(input$hv, 3)),"+/-",sprintf("%.4f", round(tut2[which(trova_piu_vicino2() == medionefang2$hv)[1]], 4)),"\n","-----------",
       "---Kg. CO2: ", round((wid2()/48)^6/100, 2),
       "= ", round(13*(wid2()/48)^6/100/2.380*1000, 2), " Km---"
       
     ))
   }
 })
 
 
  output$notAvailableText2 <- renderText({
    if (is.na(wid2())) {
      shinyjs::runjs("shinyjs.hideImg();")
      return("Not Available")
    } else {
      shinyjs::runjs("shinyjs.showImg();")
      return(NULL)
    }
  })
  
  observe({
    if (!is.na(wid2())) {
      shinyjs::runjs(sprintf("$('#img2').attr('src', 'fuelfang.jpg');"))
      shinyjs::runjs(sprintf("$('#img2').width('%s%%');", wid2()))
      shinyjs::runjs(sprintf("$('#img2').css('margin-top', '%s%%');", 3000/(wid2())-20))
    } else {
      shinyjs::runjs(sprintf("$('#img2').attr('src', '');"))  # Cambiato per nascondere l'immagine
    }
  })
 
  
  
  #####
  
  wid3 <- reactive({
    your_dataset_auto <- medionebotorch2[1:nrow(medionebotorch2),]
    
    result_auto <- your_dataset_auto %>%
      group_by(hv) %>%
      summarise(y = min(query))
    
    result_auto$hv[nrow(result_auto)] <- your_dataset_auto$hv[nrow(your_dataset_auto)] + 0.002
    
    get_y_value_auto <- function(x_value, df) {
      
      conditions <- list()
      
      for (i in 1:(nrow(df) - 1)) {
        condition <- list(
          min_value = df$hv[i],
          max_value = df$hv[i + 1],
          y_value = df$y[i + 1]
        )
        conditions <- c(conditions, list(condition))
      }
      
      for (condition in conditions) {
        if (x_value >= condition$min_value && x_value < condition$max_value) {
          return(condition$y_value)
        }
      }
      
      return(NA)
    }
    
    calculated_width_auto <- get_y_value_auto(input$hv, result_auto)
    
    return(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
    print(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
  })
  trova_piu_vicino3 <- reactive({
    indice <- which.min(abs(medionebotorch2$hv - input$hv))
    valore_piu_vicino <- medionebotorch2$hv[indice]
    # print(valore_piu_vicino)
    return(valore_piu_vicino)
    
  })
  
  output$resultNumber3 <- renderText({
    if (!is.na(wid3())) {
      return(paste(
          "-----------Hyper Volume:", sprintf("%.3f", round(input$hv, 3)),"+/-",sprintf("%.4f", round(tut3[which(trova_piu_vicino3() == medionebotorch2$hv)[1]], 4)),"\n","-----------",
        "---Kg. CO2: ", round((wid3()/48)^6/100, 2),
        "= ", round(13*(wid3()/48)^6/100/2.380*1000, 2), " Km---"
        
      ))
    }
  })
  
  
  output$notAvailableText3 <- renderText({
    if (is.na(wid3())) {
      shinyjs::runjs("shinyjs.hideImg();")
      return("Not Available")
    } else {
      shinyjs::runjs("shinyjs.showImg();")
      return(NULL)
    }
  })
  
  observe({
    if (!is.na(wid3())) {
      shinyjs::runjs(sprintf("$('#img3').attr('src', 'fuelboto.jpg');"))
      shinyjs::runjs(sprintf("$('#img3').width('%s%%');", wid3()))
      shinyjs::runjs(sprintf("$('#img3').css('margin-top', '%s%%');", 3000/(wid3())-20))
    } else {
      shinyjs::runjs(sprintf("$('#img3').attr('src', '');"))  # Cambiato per nascondere l'immagine
    }
  })
  
  
  ######################### XGB ###############
  wid4 <- reactive({
    your_dataset_auto <- medioneautogluon_xgb2
    
    result_auto <- your_dataset_auto %>%
      group_by(hv) %>%
      summarise(y = min(query))
    
    result_auto$hv[nrow(result_auto)] <- your_dataset_auto$hv[nrow(your_dataset_auto)] + 0.002
    
    get_y_value_auto <- function(x_value, df) {
      
      conditions <- list()
      
      for (i in 1:(nrow(df) - 1)) {
        condition <- list(
          min_value = df$hv[i],
          max_value = df$hv[i + 1],
          y_value = df$y[i + 1]
        )
        conditions <- c(conditions, list(condition))
      }
      
      for (condition in conditions) {
        if (x_value >= condition$min_value && x_value < condition$max_value) {
          return(condition$y_value)
        }
      }
      
      return(NA)
    }
    
    calculated_width_auto <- get_y_value_auto(input$hv, result_auto)
    #print(calculated_width_auto)
    return(((calculated_width_auto  * input$kw_hr * input$kg*input$mix_energetico )^(1/6))*48)
    print(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
  })
  trova_piu_vicino4 <- reactive({
    indice <- which.min(abs(medioneautogluon_xgb2$hv - input$hv))
    valore_piu_vicino <- medioneautogluon_xgb2$hv[indice]
    # print(valore_piu_vicino)
    return(valore_piu_vicino)
    
  })
  
  output$resultNumber4 <- renderText({
    if (!is.na(wid4())) {
      return(paste(
        "-----------Hyper Volume:", sprintf("%.3f", round(input$hv, 3)),"+/-",sprintf("%.4f", round(tut1_xgb[which(trova_piu_vicino4() == medioneautogluon_xgb2$hv)[1]], 4)),"\n", "-----------",
        "---Kg. CO2: ", round((wid4()/48)^6/100, 2),
        "= ", round(13*(wid1()/48)^6/100/2.380*1000, 2), " Km---"
        
      ))
    }
  })
  
  
  
  output$notAvailableText4 <- renderText({
    if (is.na(wid4())) {
      shinyjs::runjs("shinyjs.hideImg();")
      return("Not Available")
    } else {
      shinyjs::runjs("shinyjs.showImg();")
      return(NULL)
    }
  })
  
  observe({
    if (!is.na(wid4())) {
      shinyjs::runjs(sprintf("$('#img4').attr('src', 'fuelauto.jpg');"))
      shinyjs::runjs(sprintf("$('#img4').width('%s%%');", wid4()))
      shinyjs::runjs(sprintf("$('#img4').css('margin-top', '%s%%');", 3000/(wid4())-20))
    } else {
      shinyjs::runjs(sprintf("$('#img4').attr('src', '');"))  
    }
  })
  
  
  
  
  wid5 <- reactive({
    your_dataset_auto <- medionefang_xgb2[1:nrow(medionefang_xgb2),]
    
    result_auto <- your_dataset_auto %>%
      group_by(hv) %>%
      summarise(y = min(query))
    
    result_auto$hv[nrow(result_auto)] <- your_dataset_auto$hv[nrow(your_dataset_auto)] + 0.002
    
    get_y_value_auto <- function(x_value, df) {
      
      conditions <- list()
      
      for (i in 1:(nrow(df) - 1)) {
        condition <- list(
          min_value = df$hv[i],
          max_value = df$hv[i + 1],
          y_value = df$y[i + 1]
        )
        conditions <- c(conditions, list(condition))
      }
      
      for (condition in conditions) {
        if (x_value >= condition$min_value && x_value < condition$max_value) {
          return(condition$y_value)
        }
      }
      
      return(NA)
    }
    
    calculated_width_auto <- get_y_value_auto(input$hv, result_auto)
    
    return(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
    print(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
  })
  trova_piu_vicino5 <- reactive({
    indice <- which.min(abs(medionefang_xgb2$hv - input$hv))
    valore_piu_vicino <- medionefang_xgb2$hv[indice]
    # print(valore_piu_vicino)
    return(valore_piu_vicino)
    
  })
  
  output$resultNumber5 <- renderText({
    if (!is.na(wid5())) {
      return(paste(
        "-----------Hyper Volume:", sprintf("%.3f", round(input$hv, 3)),"+/-",sprintf("%.4f", round(tut2_xgb[which(trova_piu_vicino5() == medionefang_xgb2$hv)[1]], 4)),"\n","-----------",
        "---Kg. CO2: ", round((wid2()/48)^6/100, 2),
        "= ", round(13*(wid2()/48)^6/100/2.380*1000, 2), " Km---"
        
      ))
    }
  })
  
  
  output$notAvailableText5 <- renderText({
    if (is.na(wid5())) {
      shinyjs::runjs("shinyjs.hideImg();")
      return("Not Available")
    } else {
      shinyjs::runjs("shinyjs.showImg();")
      return(NULL)
    }
  })
  
  observe({
    if (!is.na(wid5())) {
      shinyjs::runjs(sprintf("$('#img5').attr('src', 'fuelfang.jpg');"))
      shinyjs::runjs(sprintf("$('#img5').width('%s%%');", wid5()))
      shinyjs::runjs(sprintf("$('#img5').css('margin-top', '%s%%');", 3000/(wid5())-20))
    } else {
      shinyjs::runjs(sprintf("$('#img5').attr('src', '');"))  # Cambiato per nascondere l'immagine
    }
  })
  
  
  
  #####
  
  wid6 <- reactive({
    your_dataset_auto <- medionebotorch_xgb2[1:nrow(medionebotorch_xgb2),]
    
    result_auto <- your_dataset_auto %>%
      group_by(hv) %>%
      summarise(y = min(query))
    
    result_auto$hv[nrow(result_auto)] <- your_dataset_auto$hv[nrow(your_dataset_auto)] + 0.002
    
    get_y_value_auto <- function(x_value, df) {
      
      conditions <- list()
      
      for (i in 1:(nrow(df) - 1)) {
        condition <- list(
          min_value = df$hv[i],
          max_value = df$hv[i + 1],
          y_value = df$y[i + 1]
        )
        conditions <- c(conditions, list(condition))
      }
      
      for (condition in conditions) {
        if (x_value >= condition$min_value && x_value < condition$max_value) {
          return(condition$y_value)
        }
      }
      
      return(NA)
    }
    
    calculated_width_auto <- get_y_value_auto(input$hv, result_auto)
    
    return(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
    print(((calculated_width_auto  * input$kg*input$kw_hr * input$mix_energetico )^(1/6))*48)
  })
  trova_piu_vicino6 <- reactive({
    indice <- which.min(abs(medionebotorch_xgb2$hv - input$hv))
    valore_piu_vicino <- medionebotorch_xgb2$hv[indice]
    # print(valore_piu_vicino)
    return(valore_piu_vicino)
    
  })
  
  output$resultNumber6 <- renderText({
    if (!is.na(wid6())) {
      return(paste(
        "-----------Hyper Volume:", sprintf("%.3f", round(input$hv, 3)),"+/-",sprintf("%.4f", round(tut3_xgb[which(trova_piu_vicino6() == medionebotorch_xgb2$hv)[1]], 4)),"\n","-----------",
        "---Kg. CO2: ", round((wid6()/48)^6/100, 2),
        "= ", round(13*(wid6()/48)^6/100/2.380*1000, 2), " Km---"
        
      ))
    }
  })
  
  
  output$notAvailableText6 <- renderText({
    if (is.na(wid6())) {
      shinyjs::runjs("shinyjs.hideImg();")
      return("Not Available")
    } else {
      shinyjs::runjs("shinyjs.showImg();")
      return(NULL)
    }
  })
  
  observe({
    if (!is.na(wid6())) {
      shinyjs::runjs(sprintf("$('#img6').attr('src', 'fuelboto.jpg');"))
      shinyjs::runjs(sprintf("$('#img6').width('%s%%');", wid6()))
      shinyjs::runjs(sprintf("$('#img6').css('margin-top', '%s%%');", 3000/(wid6())-20))
    } else {
      shinyjs::runjs(sprintf("$('#img6').attr('src', '');"))  # Cambiato per nascondere l'immagine
    }
  })
  
  
  
  
   
}

#shinyApp(ui, server)

