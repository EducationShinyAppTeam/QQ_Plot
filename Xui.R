library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotrix)
library(boastUtils)






ui <- list(
  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = "QQ Plot",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown", 
      boastUtils::surveyLink(name = "QQ_Plot")),
      tags$li(class = "dropdown", tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home")))),
      dashboardSidebar(
        sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "intro", icon = icon("dashboard")),
        menuItem("Explore", tabName = "qqplots", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
        ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::psu_eberly_logo("reversed")
        )
      ),
                      
      dashboardBody(
        tabItems(
        # First tab content
        tabItem(
          tabName = "intro",
          h1("Quantile-Quantile Plot"),
          h3(tags$b("About:")),
          h4("This application is designed to examine normal Q-Q (Quantile-Quantile) plots. 
          The Q-Q plot, or quantile-quantile plot, is a graphical tool to help us assess
          if a set of data plausibly came from some theoretical distribution such 
          as a Normal in use for this app."),
          br(),
          h3(strong("Instructions:")),
          h4(tags$li("There are five population types: left-skewed, right-skewed, symmetric, bimodel and normal.")),
          h4(tags$li("For each population type, you can change the corresponding
          parameters and change the population density as shown in the population graph.")),
          h4(tags$li("A quantile-quantile plot for a random sample from the 
          population is shown on the right.")),
          h4(tags$li("Move the slider to explore how sample size affects the normal 
          q-q plot and see how this varies from sample to sample using the # of paths slider.")),
          #h4("You can also look at the population density below the q-q plot."),
          h4(tags$li("Click 'GO!' to start.")),
          h4(""),
          h4(tags$li("Play with it and have fun!")),
                                  
          div(
            style = "text-align: center" ,
            bsButton(
              inputId = "start", 
              label = "GO!", 
              size = "large", 
              icon = icon("bolt"),
              style = "default")
            ),
          br(),
          h3(strong("Acknowledgements:")),
          h4("This app was developed and coded by Jiajun Gao.")
      ),
                          
      #second tab item
      tabItem(
        tabName = "qqplots",
        
        sidebarLayout(
          sidebarPanel(
            selectInput("dist", "Population Type",
               list( 
                 "Left-skewed" = "leftskewed",
                 "Right-skewed" = "rightskewed",
                 "Symmetric" = "symmetric",
                 "Bimodal" = "bimodal",
                 "Normal" = "normal"), 
                 selected = "leftskewed"
                        ),
         br(),
         br(),
        conditionalPanel(
          condition = "input.dist == 'leftskewed'",
          sliderInput("leftskew", " Skewness:",min = 1, max = 10, value = 1, step = 0.1)
                         ),
         conditionalPanel(
          condition = "input.dist=='rightskewed'",
          sliderInput("rightskew", "Skewness:",min = 1, max = 10, value = 1, step = 0.1)
                          ), 
         conditionalPanel(
           condition = "input.dist=='symmetric'",
           sliderInput("inverse","Peakedness:", min = 0.5, max = 10, value = 1, step = 0.1)
                         ),
         conditionalPanel(
           condition = "input.dist=='bimodal'",
           sliderInput("prop","% under right mode:",min = 0, max = 1, value = 0.2)
                         ),
         conditionalPanel(
           condition = "input.dist == 'normal'",
           sliderInput("normmean", "Mean:", min = -5, max = 5, value = 0, step = 0.1),
           sliderInput("normsd", "Standard Deviation:", min = 1, max = 5, value = 1, step = 0.1)
                         ),
         conditionalPanel(
           condition = "input.dist == 'leftskewed'", 
           sliderInput("leftpath", "# of paths",
           min = 1,
           max = 3,
           value = 1),
           sliderInput("leftsize", "sample size (n)",
           min = 10,
           max = 500,
           value = 100)
                         ),
        conditionalPanel(
          condition = "input.dist == 'rightskewed'", 
          # choose the number of sample means
          sliderInput("rightpath", "# of paths",
          min = 1,
          max = 3,
          value = 1),
          # choose the number of sample means
          sliderInput("rightsize", "sample size (n)",
          min = 10,
          max = 500,
          value = 100)
                         ),
                                      
        conditionalPanel(
          condition = "input.dist == 'symmetric'",
          #choose the number of sample means
          sliderInput("sympath", "# of paths",
          min = 1,
          max = 3,
          value = 1),
          #choose the number of sample means
          sliderInput("symsize", "sample size (n)",
          min = 10,
          max = 500,
          value = 100)
                        ),
        conditionalPanel(
          condition = "input.dist == 'bimodal'",
          #choose the number of sample means
          sliderInput("bipath", "# of paths",
          min = 1,
          max = 3,
          value = 1),
          #choose the number of sample means
          sliderInput("bisize", " sample size (n)",
          min = 10,
          max = 500,
          value = 100)
                       ),
        conditionalPanel(
           condition = "input.dist == 'normal'",
           #choose the number of sample means
           sliderInput("normpath", "# of paths",
           min = 1,
           max = 3,
           value = 1),
           #choose the number of sample means
           sliderInput("normsize", " sample size (n)",
           min = 10,
           max = 500,
           value = 100)
                        )
                           ),
                                    
        mainPanel(
          conditionalPanel(
             condition = "input.dist == 'leftskewed'", 
             plotOutput('plotleft2')),
          conditionalPanel(
             condition = "input.dist == 'rightskewed'", 
             plotOutput('plotright2')),
          conditionalPanel(
             condition = "input.dist == 'symmetric'",
             plotOutput('plotsymmetric2')),
          conditionalPanel(
             condition = "input.dist == 'bimodal'",
             plotOutput('plotbiomodel2')),
          conditionalPanel(
             condition = "input.dist == 'normal'",
             plotOutput('plotnormal2')),
             br(),
          conditionalPanel(
             condition = "input.dist == 'leftskewed'",
             plotOutput('plotleft1')),
          conditionalPanel(
             condition = "input.dist == 'rightskewed'",
             plotOutput('plotright1')),
          conditionalPanel(
             condition = "input.dist == 'symmetric'",
             plotOutput('plotsymmetric1')),
          conditionalPanel(
             condition = "input.dist == 'bimodal'",
             plotOutput('plotbiomodel1')),
          conditionalPanel(
             condition = "input.dist == 'normal'",
             plotOutput('plotnormal1'))
                                    )
                                  )),
      tabItem(
        tabName = "references",
        withMathJax(),
        h2("References"),
        p(
          class = "hangingindent",
          "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny.
          (v0.61). [R package]. Available from
          https://CRAN.R-project.org/package=shinyBS"
        ),
        p(
          class = "hangingindent",
          "Carey, R. and Hatfield, N. (2020), boastUtils: BOAST Utilities. 
          (v. 0.1.10.2), [R Package] Available from 
          https://github.com/EducationShinyAppTeam/boastUtils"
        ),
        p(
          class = "hangingindent",
          "Lemon, J. (2006). plotrix: a package in
          the red light district of R R-News, 6(4), 8-12"
        ),
        p(
          class = "hangingindent",
          "Chang W, Cheng J, Allaire J, Xie Y and McPherson J (2017). 
          shiny: Web Application Framework for R. R package version 1.0.3"
        ),
        p(
          class = "hangingindent",
          "Chang W and Borges Ribeiro B (2017). 
          shinydashboard: Create Dashboards with ‘Shiny’. R package version 0.6.1"
        ),
        p(
          class = "hangingindent",
          "Wickham H (2016). ggplot2: Elegant Graphics for 
          Data Analysis. Springer-Verlag New York. ISBN 978-3-319-24277-4"
        ),
        p(
          class = "hangingindent",
          "Wickham H (2007). reshape2: Reshaping Data with 
          the reshape Package. Journal of Statistical Software, 21(12), 1-20"
        ),
        p(
          class = "hangingindent",
          "Hadley Wickham (2016). scales: Scale Functions for Visualization. 
          R package version 0.4.1"
        ),
        p(
          class = "hangingindent",
          "R Core Team (2018). stats: A language and environment for statistical 
          computing. R Foundation for Statistical Computing, Vienna, Austria."
        ),
        p(
          class = "hangingindent",
          "Boos DD and Nychka D (2012). Rlab: Functions and Datasets 
          Required for ST370 class. R package version 2.15.1"
        ),
        P(
          class = "hangingindent",
          "Kun Ren and Kenton Russell (2016). formattable: Create 
          ‘Formattable’ Data Structures. R package version 0.2.0.1."
        ),
        p(
          class = "hangingindent",
          "Heike Trautmann and Detlef Steuer and Olaf Mersmann and Björn 
          Bornkamp (2014). truncnorm: Truncated normal distribution R 
          version 1.0-7"
        ),
        p(
          class = "hangingindent",
          "Millard SP (2013). EnvStats: An R Package for Environmental 
          Statistics. Springer, New York. ISBN 978-1-4614-8455-4."
        ),
        boastUtils::copyrightInfo()
        
      )
                                  
############################################################################
                      #             wellPanel(
                      #               fluidRow(
                      #                 column(3,
                      #                        selectInput("dist", "Population Type",
                      #                                    list( 
                      #                                          "Left-skewed" = "leftskewed",
                      #                                          "Right-skewed" = "rightskewed",
                      #                                          "Symmetric" = "symmetric",
                      #                                          "Bimodal" = "bimodal",
                      #                                          "Normal" = "normal"), 
                      #                                    selected = "leftskewed"
                      #                        ),
                      #                        conditionalPanel(
                      #                          condition="input.dist=='leftskewed'",
                      #                          sliderInput("leftskew", " Skewness:",min = 1, max = 10, value = 1, step= 0.1)
                      #                        ),
                      #                        conditionalPanel(
                      #                          condition="input.dist=='rightskewed'",
                      #                          sliderInput("rightskew", "Skewness:",min = 1, max = 10, value = 1, step= 0.1)
                      #                        ), 
                      #                        conditionalPanel(
                      #                         condition="input.dist=='symmetric'",
                      #                         sliderInput("inverse","Peakedness:", min = 0.5, max = 10, value = 1, step= 0.1)
                      #                         ),
                      #                          conditionalPanel(
                      #                          condition="input.dist=='bimodal'",
                      #                          
                      #                          sliderInput("prop","% under right mode:",min = 0, max = 1, value = 0.2)
                      #                        ),
                      #                        conditionalPanel(
                      #                          condition = "input.dist == 'normal'",
                      #                          sliderInput("normmean", "Mean:", min = -5, max = 5, value = 0, step = 0.1),
                      #                          sliderInput("normsd", "Standard Deviation:", min = 1, max = 5, value = 1, step = 0.1)
                      #                        )
                      #                        ),
                      #                          
                      #                        column(3,
                      #                               #left skewed
                      #                               conditionalPanel(
                      #                                 condition ="input.dist == 'leftskewed'", 
                      #                                 sliderInput("leftpath",
                      #                                             "# of paths",
                      #                                             min = 1,
                      #                                             max = 3,
                      #                                             value = 1),
                      #                                 sliderInput("leftsize",
                      #                                             "sample size (n)",
                      #                                             min = 10,
                      #                                             max = 1000,
                      #                                             value = 100)
                      #                                 
                      #                               ),
                      #                               conditionalPanel(
                      #                                 condition = "input.dist == 'rightskewed'", 
                      #                                 
                      #                                 # choose the number of sample means
                      #                                 sliderInput("rightpath",
                      #                                             "# of paths",
                      #                                             min = 1,
                      #                                             max = 3,
                      #                                             value = 1),
                      #                                 # choose the number of sample means
                      #                                 sliderInput("rightsize",
                      #                                             "sample size (n)",
                      #                                             min = 10,
                      #                                             max = 1000,
                      #                                             value = 100)
                      #                               ),
                      #                               
                      #                               conditionalPanel(
                      #                                 condition= "input.dist == 'symmetric'",
                      #                                 
                      #                                 #choose the number of sample means
                      #                                 sliderInput("sympath",
                      #                                             "# of paths",
                      #                                             min = 1,
                      #                                             max = 3,
                      #                                             value = 1),
                      #                                 #choose the number of sample means
                      #                                 sliderInput("symsize",
                      #                                             "sample size (n)",
                      #                                             min = 10,
                      #                                             max = 1000,
                      #                                             value = 100)
                      #                                 
                      #                               ),
                      #                               conditionalPanel(
                      #                                   condition= "input.dist == 'bimodal'",
                      #                                   
                      #                                   #choose the number of sample means
                      #                                   sliderInput("bipath",
                      #                                               "# of paths",
                      #                                               min = 1,
                      #                                               max = 3,
                      #                                               value = 1),
                      #                                   #choose the number of sample means
                      #                                   sliderInput("bisize",
                      #                                               " sample size (n)",
                      #                                               min = 10,
                      #                                               max = 1000,
                      #                                               value = 100)
                      #                                   
                      #                                 ),
                      #                               
                      #                               conditionalPanel(
                      #                                 condition= "input.dist == 'normal'",
                      #                                 
                      #                                 #choose the number of sample means
                      #                                 sliderInput("normpath",
                      #                                             "# of paths",
                      #                                             min = 1,
                      #                                             max = 3,
                      #                                             value = 1),
                      #                                 #choose the number of sample means
                      #                                 sliderInput("normsize",
                      #                                             " sample size (n)",
                      #                                             min = 10,
                      #                                             max = 1000,
                      #                                             value = 100)
                      #                          )
                      #                        ),
                      #                          
                      #                 column(6,
                      #                        conditionalPanel(
                      #                          condition ="input.dist == 'leftskewed'", 
                      #                          plotOutput('plotleft2')),
                      #                        conditionalPanel(
                      #                          condition = "input.dist == 'rightskewed'", 
                      #                          plotOutput('plotright2')),
                      #                        conditionalPanel(
                      #                          condition= "input.dist == 'symmetric'",
                      #                          plotOutput('plotsymmetric2')),
                      #                        conditionalPanel(
                      #                          condition= "input.dist == 'bimodal'",
                      #                          plotOutput('plotbiomodel2')),
                      #                        conditionalPanel(
                      #                          condition= "input.dist == 'normal'",
                      #                          plotOutput('plotnormal2'))
                      #                 )
                      #                 
                      #                 
                      #     
                      #     )
                      # ),
                      # br(),
                      # fluidRow(
                      #   
                      #   column(6,
                      #          conditionalPanel(
                      #            condition ="input.dist == 'leftskewed'",
                      #            plotOutput('plotleft1')),
                      # 
                      #          conditionalPanel(
                      #            condition = "input.dist == 'rightskewed'",
                      #            plotOutput('plotright1')),
                      #          conditionalPanel(
                      #            condition= "input.dist == 'symmetric'",
                      #            plotOutput('plotsymmetric1')),
                      #          conditionalPanel(
                      #            condition= "input.dist == 'bimodal'",
                      #            plotOutput('plotbiomodel1')),
                      #          conditionalPanel(
                      #            condition= "input.dist == 'normal'",
                      #            plotOutput('plotnormal1'))
                      #   )
                      #   )
####################################################################
  

  
)
)))

