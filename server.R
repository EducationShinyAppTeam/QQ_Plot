library(shiny)

library(plotrix)
library(ggplot2)
library(reshape2)
library(scales)
library(stats)
library(Rlab)
library(dplyr)
library(formattable)
library(discrimARTs)
library(truncnorm)
library(EnvStats)
library("shinydashboard", lib.loc="/usr/lib64/R/library")


shinyServer(function(input, output, session) {
  observeEvent(input$start, {
    updateTabItems(session, "tabs", "qqplots")
  })
  #list all input value 
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
    output$plotleft1 <- renderPlot({
      curve(dgamma(-x, shape = input$leftskew, beta = 1),
            main="Population Graph", col="orange", xlab="value", ylab="density",lwd = 5,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c(input$leftskew-9*sqrt(input$leftskew), 0))
    })
    
    # Matrix of rgamma values
    data1 <- reactive(matrix(-rgamma(n = input$leftpath*input$leftsize, 
                                     input$leftskew, beta = 1), 
                             nrow = input$leftsize, ncol = input$leftpath))
    
    #qqplot for leftskewed
    output$plotleft2 <- renderPlot({
      matrix <- data1()
      if(input$leftpath == 1){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        #qqline((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), col = "orange", lwd = 2)
      }
      else if (input$leftpath == 2){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
      else if (input$leftpath == 3){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(138, 43, 226, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n', qq.line.type="0-1",
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,3], col = rgb(138, 43, 226, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
    })
    # output$plotleft2 <- renderPlot(
    #     matrix <- data1()
    #     if(input$leftpath == 1){
    #       qqline(matrix[,1], col = 2)
    #     }
    # 
    # )
    
    
    #Polplation of rightskewed
    output$plotright1<-renderPlot({
      curve(dgamma(x, shape = input$rightskew, beta = 1),
            main="Population Graph", col="orange", xlab="value", ylab="density",lwd = 5,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c(0, input$rightskew+9*sqrt(input$rightskew)))
    })
    
    # Matrix of rgamma values
    data2 <- reactive(matrix(rgamma(n = input$rightpath*input$rightsize, 
                                    input$rightskew, beta = 1), 
                             nrow = input$rightsize, ncol= input$rightpath))
    
    output$plotright2 <- renderPlot({
      matrix <- data2()
      if(input$rightpath == 1){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        #qqline(matrix[,1], col = "orange", lwd = 2)
      }
      else if (input$rightpath == 2){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
      else if (input$rightpath == 3){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(138, 43, 226, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n', qq.line.type="0-1",
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,3], col = rgb(138, 43, 226, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
    })
    
    #Population of symmetric
    output$plotsymmetric1 <- renderPlot({
      x <- seq(0, 1, length = input$symsize)
      dens <- dbeta(x, shape1 = input$inverse, shape2 = input$inverse)
      
      # Dealing with peakness = 1 special case
      if(input$inverse == 1){
        plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.03,1.03),
             cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "value", ylab = "density", main = "Population Graph",
             col = "orange", lwd = 5)
        segments(0,0,0,1, col = "orange",lwd = 5)
        segments(1,0,1,1, col = "orange",lwd = 5)
        lines(x, dens, col = "orange")
        
      }else{
        plot(x, dens, type = "l", yaxs = "i", xaxs = "i", xlim=c(-0.01,1.01),
             cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
             xlab = "value", ylab = "density", main = "Population Graph",
             col = "orange", lwd = 5)
        lines(x, dens, col = "orange")
      }
      
    })
    
    # Matrix of rbeta values
    data3 <- reactive(matrix(rbeta(input$sympath*input$symsize, 
                                   shape1 = input$inverse, shape2 = input$inverse), 
                             nrow = input$symsize, ncol = input$sympath))
    
    output$plotsymmetric2 <- renderPlot({
      matrix <- data3()
      if(input$sympath == 1){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        #qqline(matrix[,1], col = "orange", lwd = 2)
      }
      else if (input$sympath == 2){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
      else if (input$sympath == 3){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(138, 43, 226, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n', qq.line.type="0-1",
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,3], col = rgb(138, 43, 226, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
    })
    
    # Population for biomodel
    output$plotbiomodel1 <- renderPlot({
      a <- data4()
      t <- 5/length(a)
      y <- seq(0+t, 5, t)
      z <- seq(5-t, 0, -t)
      
      x <- seq(0, 5, by=0.005)
      leftdraw <- dgamma(z, input$leftskew, beta=1)
      rightdraw <- dgamma(y, input$rightskew, beta=1)
      Z <- input$prop*leftdraw + (1-input$prop)*rightdraw
      
      
      plot(y, Z, type="l", yaxs="i", xaxs="i",
           xlab="value", ylab="density", main="Population Graph", 
           cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
           col="orange", lwd=5)
      lines(y, Z, type="l", col="orange", xlab="",ylab="")
    })
    # Matrix of rgamma value
    data4 <- reactive(matrix(mix.synthetic.facing.gamma(N = input$bisize*input$bipath, mix.prob = 1-input$prop,
                                                        lower = 0, upper = 6, shape1=input$leftskew, scale1=1, 
                                                        shape2=input$rightskew, scale2=1),
                             nrow = input$bisize, ncol = input$bipath))
    
    output$plotbiomodel2 <- renderPlot({
      matrix<-data4()
      if(input$bipath == 1){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        #qqline(matrix[,1], col = "orange", lwd = 2)
      }
      else if (input$bipath == 2){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
      else if (input$bipath == 3){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(138, 43, 226, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n', qq.line.type="0-1",
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,3], col = rgb(138, 43, 226, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
    })
    
    #Polplation of normal
    output$plotnormal1 <- renderPlot({
      curve(dnorm(x, mean = input$normmean, sd = input$normsd),
            main="Population Graph", col="orange", xlab="value", ylab="density",lwd = 5,
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
            xlim = c((input$normmean - 4*input$normsd), (input$normmean + 4*input$normsd)))
    })
    
    # Matrix of rnorm values
    data5 <- reactive(matrix(rnorm(n = input$normpath*input$normsize, 
                                     input$normmean, input$normsd), 
                             nrow = input$normsize, ncol = input$normpath))
    
    #qqplot for normal
    output$plotnormal2 <- renderPlot({
      matrix <- data5()
      if(input$normpath == 1){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        #qqline((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), col = "orange", lwd = 2)
      }
      else if (input$normpath == 2){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
      else if (input$normpath == 3){
        qqPlot((matrix[,1] - mean(matrix[,1]))/sd(matrix[,1]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(99, 235, 235, maxColorValue = 255), line.col = "orange", plot.type = "Q-Q", 
               qq.line.type="0-1", add.line=TRUE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,1], col = rgb(99, 235, 235, maxColorValue = 255), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,2] - mean(matrix[,2]))/sd(matrix[,2]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(255, 102, 102, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n',
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, qq.line.type="0-1",
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,2], col = rgb(255, 102, 102, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
        par(new = TRUE)
        qqPlot((matrix[,3] - mean(matrix[,3]))/sd(matrix[,3]), distribution="norm", param.list=list(mean=0, sd=1), 
               points.col = rgb(138, 43, 226, maxColorValue = 255), plot.type = "Q-Q", yaxt='n', xaxt='n', qq.line.type="0-1",
               add.line=FALSE, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
               main = "Normal Q-Q Plot", ylab = "Standardized Sample Quantiles")
        #qqnorm(matrix[,3], col = rgb(138, 43, 226, maxColorValue = 255), yaxt='n', cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
      }
    })
    
  }
  
)
})