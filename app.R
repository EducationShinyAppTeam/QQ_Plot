# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(boastUtils)
library(ggplot2)
library(EnvStats)
library(shinyWidgets)

# Define common functions ----
source("popPicker.R")
maxPaths <- 3

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
          p("This application is designed to help the user explore how a QQ 
            (Quantile-Quantile) plot can be used as a graphical tool to assess 
            whether a particular set of observations reasonably follow a particular 
            distribution such as a Gaussian (''normal'')."),
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
            Adam Poleski and Nurul Syafiqah Hamdi.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 6/24/2022 by NJH")
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
          popPickerUI(
            namespaceID = "popPicker1",
            discMenu = "none",
            contMenu = "shapes"
            ),
          hr(),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = "paths",
                  label = "Number of paths",
                  min = 1,
                  max = maxPaths,
                  step = 1,
                  value = 1
                ),
                sliderInput(
                  inputId = "sampleSize",
                  label = "Set sample size",
                  min = 10,
                  max = 500,
                  value = 100
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput("qqPlot")
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
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie,
            Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021).
            shiny: Web application framework for R. (v. 1.7.1) [R package].
            Available from https://CRAN.R-project.org/package=shiny"
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
  
  ## New Server code ----
  simInfo <- popPickerServer(namespaceID = "popPicker1")
  
  sampleData <- reactiveVal(0)
  
  observeEvent(
    eventExpr = c(simInfo$dataFunction(), input$sampleSize, simInfo$pop()),
    handlerExpr = {
      if (simInfo$pop() != "bimodal") {
        makeData <- gsub(
          pattern = "size",
          replacement = (input$sampleSize * maxPaths),
          x = simInfo$dataFunction()
        )
        sampleData(
          matrix(
            data = eval(parse(text = makeData)),
            nrow = input$sampleSize,
            ncol = maxPaths
          )
        )
      } else if (simInfo$pop() == "bimodal") {
        temp1 <- sample(
          x = c(0,1),
          size = input$sampleSize * maxPaths,
          replace = TRUE,
          prob = c(simInfo$dataFunction()$left, 1 - simInfo$dataFunction()$left)
        )
        leftSide <- rbeta(
          n = length(temp1) - sum(temp1),
          shape1 = simInfo$dataFunction()$alpha,
          shape2 = simInfo$dataFunction()$beta
        )
        rightSide <- 1 - rbeta(
          n = sum(temp1),
          shape1 = simInfo$dataFunction()$alpha,
          shape2 = simInfo$dataFunction()$beta
        )
        sampleData(
          matrix(
            data = c(leftSide, rightSide),
            nrow = input$sampleSize,
            ncol = maxPaths
          )
        )
      }
    }
  )

  observeEvent(
    eventExpr = input$paths,
    handlerExpr = {
      output$qqPlot <- renderPlot(
        expr = {
          validate(
            need(
              expr = simInfo$pop() != "start",
              message = "Select a population to explore"
            )
          )
          if (input$paths == 1) {
            qqPlot(
              (sampleData()[,1] - mean(sampleData()[,1]))/sd(sampleData()[,1]),
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
          } else if (input$paths == 2) {
            qqPlot(
              (sampleData()[,1] - mean(sampleData()[,1]))/sd(sampleData()[,1]),
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
            par(new = TRUE)
            qqPlot(
              (sampleData()[,2] - mean(sampleData()[,2]))/sd(sampleData()[,2]),
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
          } else if (input$paths == 3) {
            qqPlot(
              (sampleData()[,1] - mean(sampleData()[,1]))/sd(sampleData()[,1]),
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
            par(new = TRUE)
            qqPlot(
              (sampleData()[,2] - mean(sampleData()[,2]))/sd(sampleData()[,2]),
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
            par(new = TRUE)
            qqPlot(
              (sampleData()[,3] - mean(sampleData()[,3]))/sd(sampleData()[,3]),
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
              ylab = "Standardized Sample Quantiles"
            )
          }
        },
        alt = reactive(
          paste("The Q-Q plots for your selected", simInfo$pop(), "population with ",
                input$paths, "samples (paths) showing")
        )
      )
    }
  )
}

boastUtils::boastApp(ui = ui, server = server)
