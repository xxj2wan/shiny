
library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)

testData <- read.table(file="trans_wq",sep="\t"
                       ,header=TRUE)
vibrio_sm <- read.table(file="works/shiny/vibrio_isolation",sep="\t",header=TRUE)
colnames(vibrio_sm)[2] <- "cfu/ml"

colname <- as.factor(colnames(testData)[-1:-2])
BPtestdata <- function(test){
  BPtest1 <- select(testData, Group, test)
  var_object <- BPtest1[,2]
  var_group <- BPtest1[,1]
  plot(x = var_group,y = var_object, main = test)
}
BPtestdata_summary <-function(test, groups){
  BPtest1 <- select(testData, Group, test)
  BPtest2 <- filter(BPtest1, Group == groups)
  summary(BPtest2)
  ss <- summary(BPtest2)
  ss[,2]
  
  #return(summary(BPtest2))
}

Vibrio_summary <- function(){
  
  p <- ggplot(data=vibrio_sm, aes(x=site, y=freeze_stock, group=1))+
    geom_bar(stat="identity",fill="gray")+
    geom_text(aes(label=freeze_stock),vjust=-0.5)+
    theme_classic()
  p2 <- p + geom_line(aes(y = `cfu/ml`*10, colour = "CFU/mL"),
                       color="black",
                       linetype="dashed")
  p2 <- p2 + geom_point(aes(y = `cfu/ml`*10),
                        color="red")+
    scale_y_continuous(sec.axis = sec_axis(~./10, name = "CFU/mL"))
  p2 <- p2 + geom_text(aes(y = `cfu/ml`*10.8, label=`cfu/ml`),
                       color="red")

}

########################################
trans_get_row <- as.table(t(get_row))
trans_get_ro

AB_name <- c("Oxytretracycline","Ampicillin","Chloramphenicol","Nalidixic acid","Ceftazidime","Sulfamethoxazole")
trans_get_row
########################################
library(ggplot2)
AR <- read.table("works/shiny/trans_ast_result",header=TRUE)
library(dplyr)
AR <- filter(AR, AR$index == "Resistants")
AR <- AR[,-2]

sitename <- colnames(AR)
sitename <- sitename[-1]
sitenames <- c(rep(sitename[1],6),rep(sitename[2],6),rep(sitename[3],6),rep(sitename[4],6),rep(sitename[5],6),rep(sitename[6],6))
ab <- AR[,1]
ab <- as.character(ab)
values <- c(AR$site1,AR$site2,AR$site3,AR$site4,AR$site5,AR$site6)
str(values)
values <- as.character(values)
data = data.frame(sitenames,ab,values)
data
astP <- ggplot(data, aes(fill=ab, x=sitenames, y=values))+
  geom_bar(position="dodge",stat="identity")+
  #geom_line(aes(y = sdata$vls))+
  #facet_wrap(~ab)+
  theme_classic()
astP
#################################################
## data from AST result is used for make bar plot which contain Resistants information
#################################################

ARs <- read.table("works/shiny/trans_ast_result",header=TRUE)
ARs <- filter(ARs, ARs$index == "sensitive")

ARs <- ARs[,-2]
sn2 <- colnames(ARs)
sn2 <- sn2[-1]
sn2s <- rep(sn2,each=length(sn2))
vls <- c(ARs$site1)
for(i in 2:(length(sn2)+1)){
  if(i == 2){
  vls <- c(ARs[,i])
  }else{
    vls <- c(vls,ARs[,i],recursive=TRUE)
    }
}
sn2s
ab
vls <- as.character(vls)
vls
sdata = data.frame(sn2s,ab,vls)

astPs <- astP + geom_line(aes(y = vls, colour = "sensitive"),
                          color="black",
                          linetype="dashed")

astPs





p2 <- p + geom_line(aes(y = `cfu/ml`*10, colour = "CFU/mL"),
                    color="black",
                    linetype="dashed")
p2 <- p2 + geom_point(aes(y = `cfu/ml`*10),
                      color="red")+
  scale_y_continuous(sec.axis = sec_axis(~./10, name = "CFU/mL"))
######################################
## And this is for make line plot to combind bar + line plot
######################################

## i think i have to make big one database which is contain resistants and sensitive data.

## in now, i seperate resistants and sensitive . 
## let's try combind or make new function 
######################################

server <- function(input,output){
  
  output$box <- renderPlot({
    BPtestdata(input$type)
  })
  output$stats <- renderPrint({
    BPtestdata_summary(input$type,input$radi)
  })
  observeEvent(input$clicks, { 
    print(as.numeric(input$clicks))
  })
  output$vibrio <- renderPlot({
    Vibrio_summary()
  })
}