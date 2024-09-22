library(dplyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(openxlsx)
library(ggbeeswarm)
library(ggthemes)
library(shiny)
library(shinyBS)
library(data.table)
library(markdown)
ui <- navbarPage(title = "Mantel-test",
                 tabPanel(
                   "Corrplot",
                   tabPanel(
                     "Chord Diagram",
                     sidebarPanel(
                       fileInput("file", h4("Upload Data :")),
                       downloadButton("Download1", "Download example data"),
                       checkboxInput("shape", "形状", FALSE) ,
                       conditionalPanel(
                         condition = "input.shape",
                         sliderInput(
                           "slider1",
                           label = "图形大小",
                           min = 0,
                           max = 5,
                           value = 1,
                           step = 0.1
                         ),
                         selectInput(
                           "select6",
                           label = "形状",
                           choices = list(
                             "圆形" = 16,
                             "三角形" = 17,
                             "菱形" = 18,
                             "正方形" = 15
                           )
                         )
                       ),
                       checkboxInput("others", "方向", FALSE) ,
                       conditionalPanel(
                         condition = "input.others",
                         selectInput(
                           "select7",
                           label = "朝向",
                           choices = list("默认" = 0, "向右" = 1, "向左" = -1)
                         ),
                         selectInput(
                           "select8",
                           label = "method ",
                           choices = list("swarm", "square", "hex", "center")
                         ),
                         selectInput(
                           "select9",
                           h4(
                             "priority:",
                             bsButton(
                               "bs0",
                               label = "",
                               icon = icon("question"),
                               style = "info",
                               size = "small"
                             )
                           ),
                           choices = list("descending", "random", "density", "none")
                         ),
                         bsPopover("bs0", "当method选为swarm时，可以更改priority。", trigger = "focus"),
                         selectInput(
                           "select10",
                           h4(
                             "corral:",
                             bsButton(
                               "bs1",
                               label = "",
                               icon = icon("question"),
                               style = "info",
                               size = "small"
                             )
                           ),
                           choices = list("omit", "random", "wrap", "gutter", "none")
                         ),
                         bsPopover(
                           "bs1",
                           "当method选为swarm并且priority选为random时，可以更改corral。",
                           trigger = "focus"
                         )
                       ),
                       checkboxInput("lollipoptitle", "标题", FALSE) ,
                       conditionalPanel(
                         condition = "input.lollipoptitle",
                         textInput("text1", "标题:",
                                   value = c("")),
                         textInput("MyX", "Plot x axis title:", value = c("My X-axis label")),
                         textInput("MyY", "Plot y axis title:", value = c("My Y-axis label")),
                       ),
                       checkboxInput("condition1", "字体", FALSE),
                       conditionalPanel(
                         condition = "input.condition1",
                         selectInput(
                           "select1",
                           label = "字体位置",
                           choices = list(
                             "标题" = 1,
                             "横坐标" = 2,
                             "纵坐标" = 3,
                             "横坐标标题" = 4,
                             "纵坐标标题" = 5
                           )
                         ),
                         conditionalPanel(
                           condition = "input.select1 == '1'",
                           radioButtons(
                             "f2",
                             label = h4("字体粗斜"),
                             choices = list(
                               "默认" = "plain",
                               "粗体" = "bold",
                               "斜体" = "italic",
                               "粗斜体" = "bold.italic"
                             )
                           ),
                           radioButtons(
                             "t2",
                             label = h4("字体样式"),
                             choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
                           ),
                           sliderInput(
                             "slider2",
                             label = "标题",
                             min = 1,
                             max = 40,
                             value = 15
                           )
                         ),
                         conditionalPanel(
                           condition = "input.select1 == '2'",
                           radioButtons(
                             "f1",
                             label = h4("字体粗斜"),
                             choices = list(
                               "默认" = "plain",
                               "粗体" = "bold",
                               "斜体" = "italic",
                               "粗斜体" = "bold.italic"
                             )
                           ),
                           radioButtons(
                             "t1",
                             label = h4("字体样式"),
                             choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
                           ),
                           sliderInput(
                             "slider4",
                             label = "横坐标字体大小",
                             min = 5,
                             max = 40,
                             value = 10
                           )
                         ),
                         conditionalPanel(
                           condition = "input.select1 == '3'",
                           radioButtons(
                             "fx1",
                             label = h4("字体粗斜"),
                             choices = list(
                               "默认" = "plain",
                               "粗体" = "bold",
                               "斜体" = "italic",
                               "粗斜体" = "bold.italic"
                             )
                           ),
                           radioButtons(
                             "tx1",
                             label = h4("字体样式"),
                             choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
                           ),
                           sliderInput(
                             "sliderx1",
                             label = "纵坐标字体大小",
                             min = 5,
                             max = 40,
                             value = 10
                           )
                         ),
                         conditionalPanel(
                           condition = "input.select1 == '4'",
                           radioButtons(
                             "f4",
                             label = h4("字体粗斜"),
                             choices = list(
                               "默认" = "plain",
                               "粗体" = "bold",
                               "斜体" = "italic",
                               "粗斜体" = "bold.italic"
                             )
                           ),
                           radioButtons(
                             "t4",
                             label = h4("字体样式"),
                             choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
                           ),
                           sliderInput(
                             "slider5",
                             label = "横坐标标题字体大小",
                             min = 1,
                             max = 40,
                             value = 10
                           )
                         ),
                         conditionalPanel(
                           condition = "input.select1 == '5'",
                           radioButtons(
                             "f3",
                             label = h4("字体粗斜"),
                             choices = list(
                               "默认" = "plain",
                               "粗体" = "bold",
                               "斜体" = "italic",
                               "粗斜体" = "bold.italic"
                             )
                           ),
                           radioButtons(
                             "t3",
                             label = h4("字体样式"),
                             choices = list("sans", "serif", "mono", "wqy-microhei" , "STKaiti", "simhei")
                           ),
                           sliderInput(
                             "slider3",
                             label = "纵坐标标题字体大小",
                             min = 1,
                             max = 40,
                             value = 10
                           )
                         ),
                       ),
                       
                       h4("图片下载"),
                       numericInput("h", "Plot download height", value = "600"),
                       numericInput("w", "Plot download width", value = "600"),
                       actionButton("action",
                                    label = "Go")
                     ),
                     mainPanel(
                       downloadButton("downloadpdf", "Download pdf-file"),
                       downloadButton("downloadsvg", "Download svg-file"),
                       plotOutput("p1", height = 600, width = 950)
                     )
                   )
                 ),
                 tabPanel("Help", includeMarkdown("README.md")))

