# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(Rlab)
library(EnvStats)
library(shinyWidgets)

# Define common functions ----
# create density plot function
makeDensityPlot <- function(data, xlims, path = 0) {
  plot <- ggplot(aes(x = x, y = y), data = data) +
    geom_path(color = "#0072B2", size = 1.5) +
    xlim(xlims) +
    xlab("Value") +
    ylab("Density") +
    ggtitle("Population Graph") +
    theme(
      axis.text = element_text(size = 18),
      plot.title = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 18),
      panel.background = element_rect(fill = "white", color = "black")
    )
  # For case in symmetric where path is 1 causing "box" shape
  if (path == 1) {
    plot <- plot +
      geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), color = "#0072B2", size = 1.5) +
      geom_segment(aes(x = 1, y = 0, xend = 1, yend = 1), color = "#0072B2", size = 1.5)
  }
  return(plot)
}

# Create example data sets ----
skewedData <- -1 * rgamma(n = 75, shape = 0.1, scale = 1)
normalData <- rnorm(n = 75, mean = 0, sd = 1)


# Create UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Q-Q Plots",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(class = "dropdown",
              boastUtils::surveyLink(name = "QQ_plot")),
      tags$li(class = "dropdown",
              tags$a(href = 'https://shinyapps.science.psu.edu/', icon("home"))
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "intro", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "qqplots", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(
          ### Overview ----
          tabName = "intro",
          h1("Quantile-Quantile Plots"),
          p("This application is designed to examine Q-Q (Quantile-Quantile)
            plots. The Q-Q plot, or quantile-quantile plot, is a graphical tool
            to help us assess if a set of data plausibly came from some
            theoretical distribution such as a normal (Gaussian) distribution."),
          br(),
          h2("Instructions"),
          tags$ul(
            tags$li("There are five types of population types you can choose: left-skewed, right-skewed,
                  symmetric, bimodel, and normal (Gaussian)."),
            tags$li("For each type of population, you can change the values of
                    corresponding parameters to change the population density as
                    shown in the population graph."),
            tags$li("A quantile-quantile (Q-Q) plot for a random sample from the
                    population is shown to the lower right, showing how the sample
                    compares to a particular theoretical normal (Gaussian)
                    distribution."),
            tags$li("Move the slider to explore how sample size affects the normal
                    Q-Q plot and see how this varies from sample to sample using
                    the number of paths slider.")
          ),
          div(
            style = "text-align: center;" ,
            bsButton(
              inputId = "start",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          br(),
          h3("Acknowledgements"),
          p("This app was developed and coded by Jiajun Gao and modified by
            Adam Poleski in 2021.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/3/2021 by NJH")
          )
        ),
        ### Prereq page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to fully understand this app and the purpose of a
            Quantile-Quantile (Q-Q) plot, review this information before heading
            to the explore page."),
          tags$ul(
            tags$li("A Quantile-Quantile Plot (Q-Q plot) displays the quantiles
                    of one distribution against the quantiles of another."),
            tags$li("Typically, we use the empirical quantiles from a data sample
                    (often standardized) on one axis (here we've used the
                    vertical axis). For the other axis, we use the theoretical
                    quantiles from particular named distribution such as the
                    Standard Normal"),
            tags$li("The Q-Q plot is commonly used in regression (and modeling)
                    diagnostics to check if the residuals follow a normal (Gaussian)
                    distribution, which is a common assumption for inference."),
            tags$li("Many times a diagnonal line going from the lower left to
                    the upper right appears in the Q-Q plot. This reference line
                    indicates perfect matching between the quantiles of the two
                    distributions."),
            tags$li("If the points on the plot are close to a diagonal line,
                    there is no evidence that the assumption is violated. As more
                    and more points depart from this line, the more we can doubt
                    that the two distributions are consistent with each other.")
          ),
          h3(" QQ Plot Example"),
          p("Looking at the two plots below, the plot on the left would not
              satisfy the regression assumption as many points are deviating from
              the referenc line. In the right plot, the quantiles of the sample
              values do roughly line up with the quantiles of the normal
              distribution, so there is no apparent violation of the normality
              assumption."
          ),
          fluidRow(
            column(
              width = 6,
              offset = 0,
              plotOutput("skewExamplePlot")
            ),
            column(
              width = 6,
              offeset = 0,
              plotOutput("goodExamplePlot")
              )
          ),
          div(
            style = "text-align: center;" ,
            bsButton(
              inputId = "start1",
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        ### Explore page ----
        tabItem(
          tabName = "qqplots",
          h2("Explore Q-Q Plots"),
          p("Use the controls on the left to explore Q-Q plots for different
            types of populations and different sample sizes."),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                selectInput(
                  inputId = "dist",
                  label = "Population type",
                  choices = list(
                    "Left-skewed" = "leftskewed",
                    "Right-skewed" = "rightskewed",
                    "Symmetric" = "symmetric",
                    "Bimodal" = "bimodal",
                    "Normal" = "normal"
                  ),
                  selected = "leftskewed"
                ),
                checkboxInput(
                  inputId = "standardOrNo",
                  label = "Show standardized values",
                  value = TRUE,
                  width = NULL
                ),
                conditionalPanel(
                  condition = "input.dist == 'leftskewed'",
                  sliderInput("leftskew", " Skewness:",min = 0, max = 1, value = .5, step = 0.01,
                              ticks = F)
                ),
                conditionalPanel(
                  condition = "input.dist=='rightskewed'",
                  sliderInput("rightskew", "Skewness:",min = 0, max = 1, value = .5, step = 0.01,
                              ticks = F)
                ),
                conditionalPanel(
                  condition = "input.dist=='symmetric'",
                  sliderInput("inverse","Peakedness:", min = 0, max = 1, value = .5, step = 0.01,
                              ticks = F)
                ),
                conditionalPanel(
                  condition = "input.dist=='bimodal'",
                  sliderInput("prop","Percent under right mode:",min = 10,
                              max = 90, value = 50, ticks = F, post = "%")
                ),
                conditionalPanel(
                  condition = "input.dist == 'normal'",
                  sliderInput("normmean", "Mean:", min = -5, max = 5, value = 0, step = 0.1,
                              ticks = F),
                  sliderInput("normsd", "Standard Deviation:", min = 1, max = 5, value = 1, step = 0.1,
                              ticks = F)
                ),
                conditionalPanel(
                  condition = "input.dist == 'leftskewed'",
                  sliderInput("leftpath", "Number of paths",
                              min = 1,
                              max = 3,
                              value = 1),
                  sliderInput("leftsize", "Sample size (n)",
                              min = 10,
                              max = 500,
                              value = 100)
                ),
                conditionalPanel(
                  condition = "input.dist == 'rightskewed'",
                  # choose the number of sample means
                  sliderInput("rightpath", "Number of paths",
                              min = 1,
                              max = 3,
                              value = 1),
                  # choose the number of sample means
                  sliderInput("rightsize", "Sample size (n)",
                              min = 10,
                              max = 500,
                              value = 100)
                ),

                conditionalPanel(
                  condition = "input.dist == 'symmetric'",
                  #choose the number of sample means
                  sliderInput("sympath", "Number of paths",
                              min = 1,
                              max = 3,
                              value = 1),
                  #choose the number of sample means
                  sliderInput("symsize", "Sample size (n)",
                              min = 10,
                              max = 500,
                              value = 100)
                ),
                conditionalPanel(
                  condition = "input.dist == 'bimodal'",
                  #choose the number of sample means
                  sliderInput("bipath", "Number of paths",
                              min = 1,
                              max = 3,
                              value = 1),
                  #choose the number of sample means
                  sliderInput("bisize", " Sample size (n)",
                              min = 10,
                              max = 500,
                              value = 100)
                ),
                conditionalPanel(
                  condition = "input.dist == 'normal'",
                  #choose the number of sample means
                  sliderInput("normpath", "Number of paths",
                              min = 1,
                              max = 3,
                              value = 1),
                  #choose the number of sample means
                  sliderInput("normsize", " Sample size (n)",
                              min = 10,
                              max = 500,
                              value = 100)
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              #### Population Graphs ----
              conditionalPanel(
                condition = "input.dist == 'leftskewed'",
                plotOutput('plotleft0')),
              conditionalPanel(
                condition = "input.dist == 'rightskewed'",
                plotOutput('plotright1')),
              conditionalPanel(
                condition = "input.dist == 'symmetric'",
                plotOutput('plotsymmetric1')),
              conditionalPanel(
                condition = "input.dist == 'bimodal'",
                plotOutput('plotbimodal1')),
              conditionalPanel(
                condition = "input.dist == 'normal'",
                plotOutput('plotnormal1')),
              #### QQ Plots ----
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
            )
          )
        ),
        ### References page ----
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
            "Boos, D. D., and Nychka, D. (2012). Rlab: Functions and datasets
            required for ST370 class. (v. 2.15.1) [R package]. Available from
            https://CRAN.R-project.org/package=Rlab"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. (2021), boastUtils: BOAST Utilities.
            (v. 0.1.11.1), [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W. and Borges Ribeiro, B. (2021). shinydashboard: Create
            dashboards with ‘Shiny’. (v. 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v. 1.7.1) [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Millard, S. P. (2013). EnvStats: An R package for environmental
            statistics. Springer, New York. ISBN 978-1-4614-8455-4."
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2021). shinyWidgets: Custom
            inputs widgets for shiny (v. 0.6.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis.
            Springer-Verlag New York. ISBN 978-3-319-24277-4"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the server ----
server <- function(input, output, session) {
  ## Set up go button 1 ----
  observeEvent(
    eventExpr = input$start,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )

  ## Set up go button 2 ----
  observeEvent(
    eventExpr = input$start1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "qqplots"
      )
    }
  )

  ## Set up Info Button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This app will help you become more familiar with
        Quanitile-Quantile plots, as well as its uses and applications."
      )
    }
  )

  ## Create example plots for prereq's ----
  ### Skewed example ----
  output$skewExamplePlot <- renderPlot({
    qqPlot(
      as.vector(scale(skewedData)),
      distribution = "norm",
      param.list = list(mean = 0, sd = 1),
      points.col = boastUtils::boastPalette[5],
      line.col = "red",
      pch = 16,
      plot.type = "Q-Q",
      qq.line.type = "0-1",
      add.line = TRUE,
      cex.lab = 1.5,
      cex.axis = 1.5,
      cex.main = 1.5,
      cex.sub = 1.5,
      main = "Normal Q-Q Plot",
      ylab = "Standardized Sample Quantiles"
    )
  })

  ### Gaussian example plot ----
  output$goodExamplePlot <- renderPlot({
    qqPlot(
      as.vector(scale(normalData)),
      distribution = "norm",
      param.list = list(mean = 0, sd = 1),
      points.col = boastUtils::boastPalette[5],
      line.col = "red",
      pch = 16,
      plot.type = "Q-Q",
      qq.line.type = "0-1",
      add.line = TRUE,
      cex.lab = 1.5,
      cex.axis = 1.5,
      cex.main = 1.5,
      cex.sub = 1.5,
      main = "Normal Q-Q Plot",
      ylab = "Standardized Sample Quantiles")
  })

  ## One OE for everything!?!?!?!?! ----
  observeEvent({
    # choose population type
    input$dist

    # Left skewed
    input$leftskew
    input$leftpath
    input$leftsize

    # Right skewed
    input$rightskew
    input$rightpath
    input$rightsize

    # Symmetric
    input$inverse
    input$sympath
    input$symsize

    # Bimodal
    input$prop
    input$bipath
    input$bisize

    #Normal
    input$normmean
    input$normsd
    input$normpath
    input$normsize
  },
  {
    #Polplation of leftskewed
    leftSkew <- reactive({
      11 - 10 * input$leftskew
    })

    # Population of left skewed
    output$plotleft0 <- renderCachedPlot(
      {
        # Define parameters for density plot
        x <- seq((leftSkew()) - 9 * sqrt((leftSkew())), 0, length = input$symsize)
        y <- dgamma(-x, shape = (leftSkew()), beta = 1)
        data <- data.frame(x = x, y = y)

        # Make Density Plot
        makeDensityPlot(data = data, xlims = c((leftSkew()) - 9 * sqrt((leftSkew())), 0))
      },
      cacheKeyExpr = {
        list(input$leftskew)
      }
    )

    # Matrix of rgamma values
    data1 <-
      reactive(matrix(
        -rgamma(
          n = input$leftpath * input$leftsize,
          (leftSkew()),
          beta = 1
        ),
        nrow = input$leftsize,
        ncol = input$leftpath
      ))

    output$plotleft1 <- renderPlot({
      curve(dgamma(-x, shape = input$leftskew, beta = 1),
            main = "Population Graph",
            col = "black",
            xlab = "value",
            ylab = "density",
            lwd = 5,
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            xlim = c(input$leftskew - 9*sqrt(input$leftskew), 0))
    })



    #qqplot for leftskewed
    output$plotleft2 <- renderPlot({
      matrix <- data1()
      if (input$leftpath == 1) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$leftpath == 2) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               pch = 16,
               plot.type = "Q-Q",
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles"
        )
      }
      else if (input$leftpath == 3) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               pch = 16,
               plot.type = "Q-Q",
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[2],
               pch = 16,
               plot.type = "Q-Q",
               yaxt = 'n',
               xaxt = 'n',
               qq.line.type = "0-1",
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
    })

    rightSkew <- reactive({
      11 - 10 * input$rightskew
    })
    # Population of right skewed
    output$plotright1 <- renderCachedPlot(
      {
        # Define parameters for density plot
        x <- seq(0, (rightSkew()) + 9 * sqrt(rightSkew()), length = input$symsize)
        y <- dgamma(x, shape = (rightSkew()), beta = 1)
        data <- data.frame(x = x, y = y)

        # Make the density plot
        makeDensityPlot(data = data, xlims = c(0, (rightSkew()) + 9 * sqrt((rightSkew()))))
      },
      cacheKeyExpr = {
        list(input$rightskew)
      }
    )

    # Matrix of rgamma values
    data2 <-
      reactive(matrix(
        rgamma(
          n = input$rightpath * input$rightsize,
          (rightSkew()),
          beta = 1
        ),
        nrow = input$rightsize,
        ncol = input$rightpath
      ))
    output$plotright2 <- renderPlot({
      matrix <- data2()
      if (input$rightpath == 1) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               pch = 16,
               line.col = "red",
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$rightpath == 2) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               pch = 16,
               line.col = "red",
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               pch = 16,
               plot.type = "Q-Q",
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$rightpath == 3) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[2],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               qq.line.type = "0-1",
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
    })

    #Population of symmetric
    inverse <- reactive({
      round(14.6 * input$inverse^3 - 5.7 * input$inverse^2 +
              input$inverse + .1, 3)
    })
    # Population of Symmetric skewed
    output$plotsymmetric1 <- renderCachedPlot(
      {
        x <- seq(0, 1, length = input$symsize)
        dens <-
          dbeta(x,
                shape1 = inverse(),
                shape2 = inverse()
          )
        data <- data.frame(x = x, y = dens)

        # Make density plot separated by case where the peakedness is exactly 1 (causes a "box" shape)
        makeDensityPlot(data = data, xlims = c(-0.03, 1.03), path = inverse())
      },
      cacheKeyExpr = {
        list(input$symsize, input$inverse)
      }
    )

    # Matrix of rbeta values
    data3 <- reactive(matrix(
      rbeta(
        input$sympath * input$symsize,
        shape1 = inverse(),
        shape2 = inverse()
      ),
      nrow = input$symsize,
      ncol = input$sympath
    ))

    # Matrix of rbeta values
    data3 <- reactive(matrix(rbeta(input$sympath*input$symsize,
                             shape1 = input$inverse, shape2 = input$inverse),
                             nrow = input$symsize, ncol = input$sympath))

    # qq plot symmetric
    output$plotsymmetric2 <- renderPlot({
      matrix <- data3()
      if (input$sympath == 1) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$sympath == 2) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               pch = 16,
               plot.type = "Q-Q",
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$sympath == 3) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[2],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               qq.line.type = "0-1",
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
    })

    # Population for bimodel
    prop <- reactive({
      input$prop / 100
    })
    output$plotbimodal1 <- renderCachedPlot(
      {
        # Define parameters for density plot
        t <- 1 / (input$bisize * input$bipath)
        y <- seq(0, 1, t)
        z <- seq(1, 0, -t)
        leftdraw <- dbeta(z, 4, 14) * .2
        rightdraw <- dbeta(y, 4, 14) * .2
        data <- data.frame(x = seq(0, 5, t * 5), y = prop() * leftdraw + (1 - prop()) *
                             rightdraw)

        # Make the density plot
        makeDensityPlot(data = data, xlims = c(0, 5))
      },
      cacheKeyExpr = {
        list(input$prop)
      }
    )

    # Create data for bimodel
    data4 <-
      reactive({
        # Random vector of 0s and 1s to determine which distribution each element
        # samples from
        rand <- sample(
          x = c(0, 1),
          size = input$bisize * input$bipath,
          replace = TRUE,
          prob = c(1 - prop(), prop())
        )

        # Number of elements sampled from the right distribution (represented by 1)
        rights <- sum(rand)
        # Number of elements sampled from left distribution (represented by 0)
        lefts <- input$bisize * input$bipath - rights
        leftGammas <- rbeta(lefts, 4, 14) * 5

        # rgamma(lefts, 1.25, beta = 1) # Samples left distribution
        rightGammas <- 5 - rbeta(rights, 4, 14) * 5 # Samples right distribution

        # Loop to assign values from gamma distributions to rand
        rightIndex <- 1
        leftIndex <- 1
        for (x in 1:length(rand)) {
          if (rand[x] == 0) {
            rand[x] <- leftGammas[leftIndex]
            leftIndex <- leftIndex + 1
          }
          else {
            rand[x] <- rightGammas[rightIndex]
            rightIndex <- rightIndex + 1
          }
        }

        # Turn vector rand into a matrix with proper dimensions
        matrix(rand, nrow = input$bisize, ncol = input$bipath)
      })


    output$plotbiomodel2 <- renderPlot({
      matrix <- data4()
      if (input$bipath == 1) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$bipath == 2) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$bipath == 3) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1", add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[2],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               qq.line.type = "0-1",
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
    })

    #Polplation of normal
    output$plotnormal1 <- renderPlot({
      curve(dnorm(x, mean = input$normmean,
                  sd = input$normsd),
            main = "Population Graph",
            col = "black",
            xlab = "value",
            ylab = "density",
            lwd = 5,
            cex.lab = 1.5,
            cex.axis = 1.5,
            cex.main = 1.5,
            cex.sub = 1.5,
            xlim = c((input$normmean - 4*input$normsd), (input$normmean + 4*input$normsd)))
    })

    # Matrix of rnorm values
    data5 <- reactive(matrix
                      (rnorm
                        (n = input$normpath*input$normsize,
                          input$normmean,
                          input$normsd),
                        nrow = input$normsize,
                        ncol = input$normpath))

    #qqplot for normal
    output$plotnormal2 <- renderPlot({
      matrix <- data5()
      if (input$normpath == 1) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$normpath == 2) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n', xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
      else if (input$normpath == 3) {
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[5],
               line.col = "red",
               pch = 16,
               plot.type = "Q-Q",
               qq.line.type = "0-1",
               add.line = TRUE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[8],
               plot.type = "Q-Q",
               pch = 16,
               yaxt = 'n',
               xaxt = 'n',
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               qq.line.type = "0-1",
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]),
               distribution = "norm",
               param.list = list(mean = 0, sd = 1),
               points.col = boastUtils::boastPalette[2],
               pch = 16,
               plot.type = "Q-Q",
               yaxt = 'n',
               xaxt = 'n',
               qq.line.type = "0-1",
               add.line = FALSE,
               cex.lab = 1.5,
               cex.axis = 1.5,
               cex.main = 1.5,
               cex.sub = 1.5,
               main = "Normal Q-Q Plot",
               ylab = "Standardized Sample Quantiles")
      }
    })

  }

  )
}

boastUtils::boastApp(ui = ui, server = server)
