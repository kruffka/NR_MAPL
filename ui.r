library(shiny)
library(shinythemes)
library(shinyjs)


shinyUI(navbarPage("5G Link Budget", selected = 2,
                   theme = shinytheme("darkly"),
                   
                   tabPanel("Main", value = 2, sidebarPanel(width = 32, fluidPage(
                     fluidRow(
                       column(2,
                              br(), br(),
                              numericInput("frequency", "Frequency:", value = 2.1, step = 1, width = 300),
                              # numericInput("avg_ratio", "AVG ratio:", value = 80, step = 10, width = 300),
                              numericInput("pen_los", "Penetration Loss:", value = 8.6, step = 10, width = 300),
                              numericInput("shadowing_margin", "Shadowing margin:", value = 13.6, step = 10, width = 300),
                              numericInput("ant_gain", "Ant Gain", value = 25.5, step = 1, width = 300),
                              
                              
                              
                              tags$style("label{color:#20B2AA;}")
                              
                              ),
                       column(2,
                              br(), br(),
                              
                              selectInput("bandwidth", "Bandwidth:", 
                                          choices = list("5" = 1, "10" = 2,
                                                         "15" = 3, "20" = 4, "25" = 5,
                                                         "40" = 6, "50" = 7, "60" = 8,
                                                         "80" = 9, "100" = 10), selected = 6, width = 300),
                              selectInput("sub_spacing", "Subcarrier spacing:", 
                                          choices = list("15" = 1, "30" = 2, "60" = 3), selected = 2, width = 300),
                              
                              htmlOutput("err")
                              # radioButtons("duplex_mode", "Duplex Mode:", choices = list("TDD" = 1, "FDD" = 2), selected = 1)
                              
                              ),
                       column(4,
                              column(4,
                                     h3("DL", style = "color:#DC143C"),
                                     numericInput("power_dl", "Power", value = 50, step = 1, width = 300),
                                     #numericInput("ant_gain_dl", "Ant Gain", value = 0, step = 1, width = 300),
                                     numericInput("noise_figure_dl", "Noise Figure",value = 7, step = 1, width = 300),
                                    # numericInput("feeder_loss_dl", "Feeder Loss", value = 2.1, step = 1, width = 300),
                                     numericInput("sinr_dl", "SINR", value = 4.91, step = 1, width = 300),
                                     numericInput("inter_margin_dl", "Interference Margin", value = 13.47, step = 1, width = 300),
                                     
                                     ),
                              column(4,
                                     h3("UL", style = "color:#00BFFF"),
                                     numericInput("power_ul", "Power", value = 24, step = 1, width = 300),
                                    # numericInput("ant_gain_ul", "Ant Gain", value = 25.5, step = 1, width = 300),
                                     numericInput("noise_figure_ul", "Noise Figure", value = 3, step = 1, width = 300),
                                    # numericInput("feeder_loss_ul", "Feeder Loss", value = 2.1, step = 1, width = 300),
                                     numericInput("sinr_ul", "SINR", value = -1.32, step = 1, width = 300),
                                     numericInput("inter_margin_ul", "Interference Margin", value = 2.55, step = 1, width = 300),
                                     
                                     
                              )
                              )))
                     
                     ),
                     
                     actionButton("calculateButton", "Calculate"),
                     actionButton("resetButton", "Reset"),
                     br(),
                     br(),
                     fluidRow(
                       column(6, DT::dataTableOutput("table")),
                       column(6, plotOutput(outputId = "plot", height = 670, click = "plot_click"), verbatimTextOutput("info"))
                     )
                     ),
                   tabPanel("Help",
                            
                             h3("DL - DownLink"), h3("UL - UpLink"), h3("BW - Bandwidth"), h3("PL - Penetration Loss", h3("IM - Interference Margin"), 
                             h3("SINR - Signal to Noise Ratio"), h3("IRP - Isotropic Radiated Power"), h3("EIRP - Effective Isotropic Radiated Power"),
                             h3("MAPL - Maximum Allowable Path Loss"), 
                             
                             h3("Possible values for bw and scs:", style = "color:#20B2AA;"), 
                             titlePanel(title=div(img(src="https://sun9-71.userapi.com/wXSiLKCR4NCovL5IrjYcG3dHVAG1i6zG2w0kLQ/REW2Vy9AM0Q.jpg")))
                             )),
                   tabPanel("About", h4("The app was made by these people:", style = "color:#DC143C"),
                            
                            tags$a(
                              href="https://sun4-17.userapi.com/qpG5kgu3YTgKemj0KVH5s_E5JRd-tg9mswvkpA/FYSo6XuAea4.jpg", 
                              tags$img( 
                                       title="Romka", 
                                       width="600",
                                       height="400")
                            ),
                            tags$a(
                              href="https://sun4-16.userapi.com/ZjGAthGNUcDsAnv9kWm0VtZ7xUhD_OYFtjwGUw/nu2JO86gp7s.jpg", 
                              tags$img(
                                       title="Damir", 
                                       width="600",
                                       height="400")
                            ),
                            tags$a(
                              href="https://sun4-11.userapi.com/JK67LG55BO1cgXaI9lgH55R99nQ6LJzMX2Z4SQ/LpTBsc-btuc.jpg", 
                              tags$img(
                                       title="Andrey", 
                                       width="600",
                                       height="400")
                            ),
                            titlePanel(title=div(img(src="https://sun4-10.userapi.com/UwGgMilWfPgIcj9BCPEIIWiIfUk5Jpgh1BCjcw/ZfOk0womEbw.jpg")))
                            ),
                   
                   # Main Panel
                   mainPanel()
                   
))
