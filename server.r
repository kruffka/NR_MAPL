library(shiny)
library(shinyjs)
library(DT)
library(plotly)

shinyServer(
  
  function(input, output, session) {
    
    # freq_out <- reactive(input$frequency)
    
    # example
    
    table5g <- matrix(c(50, 24, 100, 0.25, 25.5, 25.5, 40, 40, 75.5, 24, 7, 3, -98.12, -98.12, -91.11, -95.11,
                        4.91, -1.32, -86.2, -96.4, -86.20, -121.93, 161.7, 145.93, 13.47, 2.55,
                        148.23, 143.38, 8.6, 8.6, 13.6, 13.6, 126.03, 121.18, 121.18, 121.18), ncol=2, byrow=TRUE)
    colnames(table5g) <- c("DL","UL")
    rownames(table5g) <- c("Power [dB]", "Power [W]", "AntGain [dB]", "BW [MHz]", "EIRP [dBm]", "Noise Figure [dB]",
                           "Thermal noise [dB]", "Noise Level [dB]", "SINR [dB]", "RxSens [dB]",
                           "IPR", "MAPL(isolated) [dB]", "IM [dB]", "MAPL(no clutter)",
                           "PenLos [dB]", "Shadow Margin [dB]", "MAPL(clutter)", "MAPL")
    
    output$table = DT::renderDataTable({
      
      DT::datatable(table5g, options = list(searching = FALSE, paging = FALSE, initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#20B2AA', 'color': '#fff'});",
        "}")
        
        ))
      })
    
    dm = 500
    
    
    # graph example
    
    x <- matrix(1:dm)
    y <- matrix(1:dm)
    
    for(i in 1:dm){
      y[i] <- 114
    }
    
    
    d <- matrix(1:dm)
    pl21 <- matrix(1:dm)
    pl35 <- matrix(1:dm)
    pl28 <- matrix(1:dm)
    
    for(i in 1:dm){
      pl21[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(2.1)
      pl35[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(3.5)
      pl28[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(28)
      
    }
    
    
    
    d1=10^((-22.4 - 21.3*log10(2.1)+114)/35.3)
    d2=10^((-22.4 - 21.3*log10(3.5)+114)/35.3)
    d3=10^((-22.4 - 21.3*log10(28)+114)/35.3)
    
    output$plot <- renderPlot({
      
      main="Graphs"
        plot(x, y,
             xlab="m",
             ylab="dBm",
             type="line",
             col="red")
        lines(d, pl21, col="green")
        lines(d, pl35, col="orange")
        lines(d, pl28, col="black")
        abline(v = c(d1, d2, d3), lty = 2)
        
        legend("bottomright",
               c("MAPL", "2.1 GHz", "3.5 GHz", "28 GHz"),
               fill=c("red", "green", "orange", "blue")
        )
    })
    
    
    
    output$info <- renderText({
      paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    
    
    # error
    
    
    error_line <- reactive(sprintf('<font color="%s">%s</font>','red', 'Select another BW or SCS (check help)'))
    empty_line <- reactive(sprintf('<font color="%s">%s</font>','red', ' '))
    
    output$err <- renderText({
      
      if(((input$bandwidth == 8 || input$bandwidth == 9 || input$bandwidth == 10) && (input$sub_spacing == 1)) || (input$bandwidth == 1 && input$sub_spacing == 3)){
        paste(error_line())
      }else{
        paste(empty_line())
        
      }
      
    })
    
    
    
    # calculateButton
    
    observeEvent(input$calculateButton, {

      # d - dl; u - ul
      
      pd = input$power_dl # power DL, dB
      pu = input$power_ul # power UL, dB
      pd_w = 10^(pd/10)/1000 # power DL, W
      pu_w = 10^(pu/10)/1000 # power UL, W
      agd = input$ant_gain # ant gain DL, dB
      agu = input$ant_gain # ant gain UL, dB
      nfd = input$noise_figure_dl # noise figure DL, dB
      nfu = input$noise_figure_ul # noise figure UL, dB
      
      # select from scs and bw matrix
      
      scs <- matrix(c(15, 30, 60), ncol = 1)
      bwarr <- matrix(c(5, 10, 15, 20, 25, 40, 50, 60, 80, 100), ncol = 1)
      bw = bwarr[as.numeric(input$bandwidth)] # bandwidth, MHz
      bwscs <- matrix(c(25, 52, 70, 106, 133, 216, 270, 0, 0, 0,
                        11, 24, 38, 51, 65, 106, 133, 162, 217, 273,
                        0, 11, 18, 24, 31, 51, 65, 79, 107, 135), ncol=3)

      tn = -173.93 + 10*log10(scs[as.numeric(input$sub_spacing)]*1000*12*bwscs[as.numeric(input$bandwidth), as.numeric(input$sub_spacing)]) # thermal noise, dB
      nld = tn + nfd # noise level DL, dB
      nlu = tn + nfu # noise level UL, dB
      sinrd = input$sinr_dl # sinr DL, dB
      sinru = input$sinr_ul # sinr UL, dB
      rxsensd = nfd + sinrd + tn # rxsens DL, dB
      rxsensu = nfu + sinru + tn # rxsens UL, dB
      irpd = rxsensd - input$ant_gain*0 + 0 # ipr DL
      irpu = rxsensu - input$ant_gain + 0 # ipr UL
      eirpd = input$power_dl - 0 + input$ant_gain # eirp DL, dB
      eirpu = input$power_ul - 0 - 0 # eirp UL, dB
      mapl_isod = eirpd - irpd # mapl isolated DL, dB
      mapl_isou = eirpu - irpu # mapl isolated UL, dB
      imd = input$inter_margin_dl  # im DL, dB
      imu = input$inter_margin_ul # im UL, dB
      mapl_noclutd = mapl_isod - imd # mapl no clutter DL, dB
      mapl_noclutu = mapl_isou - imu # mapl no clutter UL, dB
      pl = input$pen_los # penlos, dB
      sm = input$shadowing_margin # shadowing margin, dB
      mapl_clutd = mapl_noclutd - sm - pl # mapl clutter DL
      mapl_clutu = mapl_noclutu - sm - pl # mapl clutter UL
      # mapld = pd + agd - imd - pl - rxsensd - sm # mapl
      # maplu = pu + agu -imu - pu - rxsensu - sm # mapl
      
      mapl = min(mapl_clutd, mapl_clutu)
      
      table5g <- matrix(c(pd, pu, pd_w, pu_w, agd, agu, bw, bw, eirpd, eirpu, nfd, nfu, tn, tn, nld, nlu,
                          sinrd, sinru, rxsensd, rxsensu, irpd, irpu, mapl_isod, mapl_isou, imd, imu,
                          mapl_noclutd, mapl_noclutu, pl, pl, sm, sm, mapl_clutd, mapl_clutu, mapl, mapl), ncol=2, byrow=TRUE)
      colnames(table5g) <- c("DL","UL")
      rownames(table5g) <- c("Power [dB]", "Power [W]", "AntGain [dB]", "BW [MHz]", "EIRP [dBm]", "Noise Figure [dB]",
                             "Thermal noise [dB]", "Noise Level [dB]", "SINR [dB]", "RxSens [dB]",
                             "IPR", "MAPL(isolated) [dB]", "IM [dB]", "MAPL(no clutter)",
                             "PenLos [dB]", "Shadow Margin [dB]", "MAPL(clutter)", "MAPL")
      output$table = DT::renderDataTable({
        
        DT::datatable(table5g, options = list(searching = FALSE, paging = FALSE, initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#20B2AA', 'color': '#fff'});",
          "}")))
        })
      
      
      maplx <- matrix(1:dm)
      maply <- matrix(1:dm)
      
      for(i in 1:dm){
        maply[i] <- mapl
      }
      
      
      d <- matrix(1:dm)
      pl <- matrix(1:dm)
      pl21 <- matrix(1:dm)
      pl35 <- matrix(1:dm)
      pl28 <- matrix(1:dm)
      
      for(i in 1:dm){
        pl[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(input$frequency)
        pl21[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(2.1)
        pl35[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(3.5)
        pl28[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(28)
        
      }
      
      output$plot <- renderPlot({
        
        
        main="Graphs"
        d4=10^((-22.4 - 21.3*log10(input$frequency)+114)/35.3)
        
      
        
          plot(maplx, maply,
               xlab="m",
               ylab="dBm",
               type="line",
               col="red")
          lines(d, pl, col="blue")
          lines(d, pl21, col="green")
          lines(d, pl35, col="orange")
          lines(d, pl28, col="black")
          abline(v = c(d1, d2, d3, d4), lty = 2)
          legend("bottomright",
                 c("MAPL", "2.1 GHz", "3.5 GHz", "28 GHz", paste(input$frequency, "GHz")),
                 fill=c("red", "green", "orange", "blue", "black")
          )
      
        
        
        #   plot(x, y, "line", col = "red")
      })
      
      
      })
    
    
    # resetButton
    
    
    observeEvent(input$resetButton, {
      
      
      updateTextInput(session, "frequency", value = 2.1)
      updateTextInput(session, "pen_los", value = 8.6)
      updateTextInput(session, "shadowing_margin", value = 13.6)
      updateTextInput(session, "ant_gain", value = 25.5)
      
      updateTextInput(session, "bandwidth", value = 6)
      updateTextInput(session, "sub_spacing", value = 2)
      
      
      updateTextInput(session, "power_dl", value = 50)
      
      updateTextInput(session, "noise_figure_dl", value = 7)
      updateTextInput(session, "sinr_dl", value = 4.91)
      updateTextInput(session, "inter_margin_dl", value = 13.47)
      
      updateTextInput(session, "power_ul", value = 24)
      updateTextInput(session, "noise_figure_ul", value = 3)
      updateTextInput(session, "sinr_ul", value = -1.32)
      updateTextInput(session, "inter_margin_ul", value = 2.55)
      
      
      # back to example
      
      table5g <- matrix(c(50, 24, 100, 0.25, 25.5, 25.5, 40, 40, 75.5, 24, 7, 3, -98.12, -98.12, -91.11, -95.11,
                          4.91, -1.32, -86.2, -96.4, -86.20, -121.93, 161.7, 145.93, 13.47, 2.55,
                          148.23, 143.38, 8.6, 8.6, 13.6, 13.6, 126.03, 121.18, 121.18, 121.18), ncol=2, byrow=TRUE)
      colnames(table5g) <- c("DL","UL")
      rownames(table5g) <- c("Power [dB]", "Power [W]", "AntGain [dB]", "BW [MHz]", "EIRP [dBm]", "Noise Figure [dB]",
                             "Thermal noise [dB]", "Noise Level [dB]", "SINR [dB]", "RxSens [dB]",
                             "IPR", "MAPL(isolated) [dB]", "IM [dB]", "MAPL(no clutter)",
                             "PenLos [dB]", "Shadow Margin [dB]", "MAPL(clutter)", "MAPL")
      
      output$table = DT::renderDataTable({
        
        DT::datatable(table5g, options = list(searching = FALSE, paging = FALSE, initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#20B2AA', 'color': '#fff'});",
          "}")))
        
      })
      
      
      # graph example
      
      x <- matrix(1:dm)
      y <- matrix(1:dm)
      
      for(i in 1:dm){
        y[i] <- 114
      }
      
      
      d <- matrix(1:dm)
      pl21 <- matrix(1:dm)
      pl35 <- matrix(1:dm)
      pl28 <- matrix(1:dm)
      
      for(i in 1:dm){
        pl21[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(2.1)
        pl35[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(3.5)
        pl28[i] = 35.3*log10(d[i]) + 22.4 + 21.3*log10(28)
        
      }
      
      output$plot <- renderPlot({
        
        main="Graphs"
        plot(x, y,
             xlab="m",
             ylab="dBm",
             type="line",
             col="red")
        lines(d, pl21, col="green")
        lines(d, pl35, col="orange")
        lines(d, pl28, col="black")
        abline(v = c(d1, d2, d3), lty = 2)
        legend("bottomright",
               c("MAPL", "2.1 GHz", "3.5 GHz", "28 GHz"),
               fill=c("red", "green", "orange", "blue")
        )
      })
      
      
      })
    })


  

