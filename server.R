server <- function(input, output, session) {
  observeEvent(input$action, {
    data <- input$file$datapath
    data <- fread(data)
    unique(data$Groups)
    top.mar = 0.5
    right.mar = 0.5
    bottom.mar = 0.2
    left.mar = 2
    Group2 <- data$Group2
    output$p1 <- renderPlot({
      ggplot(data, aes(x = Groups, y = Value)) +
        
        geom_beeswarm(
          cex = 1,
          size = input$slider1,
          method = input$select8,
          priority = input$select9,#点布局的方法
          corral = input$select10,#调整横向过宽的点
          color = Group2,
          side = as.numeric(input$select7),#抖动的方向 ，
          pch = as.numeric(input$select6)
        ) +
        scale_fill_manual(values = Group2, name = 'Group2') +
        
        
        xlab(input$MyX) + ylab(input$MyY) + ggtitle(input$text1) +
        theme(
          legend.position = 'none',
          plot.title = element_text(
            face = input$f2,
            colour = "black",
            size = input$slider2,
            hjust = 0.4,
            vjust = 3,
            family = input$t2
          ),
          axis.line = element_blank(),
          axis.ticks = element_line(linewidth = 0.6, colour = "gray30"),
          axis.ticks.length = unit(1.5, units = "mm"),
          axis.title.x = ggplot2::element_text(
            size = input$slider5,
            face = input$f4,
            vjust = 0,
            family = input$t4
          ),
          axis.title.y = ggplot2::element_text(
            size = input$slider3,
            face = input$f3,
            vjust = 3,
            family = input$t3
          ),
          axis.text.y = element_text(
            family = input$tx1,
            size = input$sliderx1,
            color = "black",
            angle = 0,
            vjust = 0.5,
            face = input$fx1
          ),
          axis.text.x = element_text(
            family = input$t1,
            size = input$slider4,
            color = "black",
            angle = 0,
            face = input$f1
          ),
          plot.margin = unit(
            x = c(top.mar, right.mar, bottom.mar, left.mar),
            units = "inches"
          )
        )
    }, height = input$h, width = input$w)
    observe({
      ## *** Download PDF file ***
      output$downloadpdf <- downloadHandler(
        filename = function() {
          paste("plot.pdf")
        },
        content <- function(file) {
          pdf(file, height = input$w / 72, width = input$h / 72)
          print(p)
          dev.off()
        },
        contentType = "application/pdf"
      )
      ## *** Download SVG file ***
      output$downloadsvg <- downloadHandler(filename <-
                                              function() {
                                                paste('plot.svg')
                                              },
                                            content <-
                                              function(file) {
                                                svg(file, width = input$w / 10, height = input$h / 10)
                                                print(p)
                                                dev.off()
                                              },
                                            contentType = 'image/svg')
    })
  })
  observe({
    ## *** Download example data ***
    output$Download1 <- downloadHandler(filename <- function() {
      paste('data.txt')
    },
    content <- function(file) {
      input_file <- "data.txt"
      exampl <- fread(input_file)
      write.table(
        exampl,
        file = file,
        row.names = F,
        col.names = T,
        quote = F
      )
    }, contentType = 'text/csv')
  })
}