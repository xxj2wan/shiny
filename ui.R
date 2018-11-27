library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
library(rsconnect)

testData <- read.table(file="trans_wq",sep="\t"
                       ,header=TRUE)
colname <- as.factor(colnames(testData)[-1:-2])

ui <- dashboardPage(
  dashboardHeader(title = "Microbiome Lab-MBL"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "Home"),
      menuItem("Analysis", tabName = "Analysis"),
      menuItem("Community", tabName = "Community")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              HTML("<div class = Contents_header><center><p><h1>Fish farm Project</h1></p></center></div>
                   ")
              ),
      tabItem(tabName = "Analysis",
              tabsetPanel(
                  tabPanel("Water Quality", sidebarLayout(position = "left",
                                                          
                                                          sidebarPanel(
                                                            selectInput(inputId = "type",
                                                                        label= strong("Index"),
                                                                        choice=unique(colname)),
                                                            radioButtons(inputId = "radi"
                                                                         ,label = "Select Site"
                                                                         ,choices = unique(testData$Group)
                                                                         ,inline = TRUE
                                                            )
                                                          ),
                                                          
                                                          mainPanel(
                                                            plotOutput("box"),
                                                            verbatimTextOutput("stats"),
                                                            actionButton(inputId = "clicks", label= "clikc me")
                                                          )
                  )),
                  tabPanel("MiSeq", sidebarLayout(position = "left",
                                                  sidebarPanel(selectInput(inputId="miseqtype",
                                                                           label = strong("MiSeq"),
                                                                           choice = unique(colname)
                                                                           )
                                                               ),
                                                  mainPanel(
                                                    
                                                  )
                                                  )
                           ),
                  tabPanel("HiSeq", "under the construction of HiSeq Analysis"),
                  tabPanel("Plasmidome", sidebarLayout(position = "left",
                                                       sidebarPanel(selectInput(inputId = "plamidome_type",
                                                                                label = strong("Index"),
                                                                                choice = c("Vibrio_isolation","test1","test2")
                                                                                )
                                                                    ),
                                                       mainPanel(
                                                         plotOutput("vibrio")
                                                       )
                                                       )
                           )
                  )
              )
    )
  )
)

