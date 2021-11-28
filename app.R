##############################################################################################################
#### This script has been written by Veo Chae for the purpose of Independent Research Fall 2021 ##############
##############################################################################################################

install.packages("memisc")
install.packages("shiny")
install.packages("ggplot2")
install.packages("gganimate")
install.packages("gifski")
install.packages("rgl")
install.packages("hrbrthemes", dependencies = TRUE)
install.packages("GGally")
install.packages("viridis")
install.packages("DT")
library("DT")
library("hrbrthemes")
library("GGally")
library("viridis")
library("memisc")
library("shiny")
library("dplyr")
library("ggplot2")
library("gganimate")
library("gifski")
library("rgl")
library("tidyverse")

##############################################################################################################
############################################ DATA CLEANING ###################################################
##############################################################################################################

#Data Import
dataset_17 <- data.frame(as.data.set(spss.system.file("D:/Project Files/GEM Dataset Visual/GEM 2017 APS Global National Level Data_27Oct.sav"))) 
dataset_16 <- data.frame(as.data.set(spss.system.file("D:/Project Files/GEM Dataset Visual/GEM 2016 APS Global National Level Data_31Oct.sav")))

#each variable has a year identifier within the name of the questionaire.
#Therefore, to look at the change in variable values between 16 and 17,
#removed the year
remove_yr = function(mydata){
  for(i in c(1:nrow(mydata))){
    mydata[i,1] <- gsub('17', '_', mydata[i,1])
  }
  return(mydata)
}

#giving new names wihtout year specifics in the variable
names <- as.data.frame(names(dataset_17))
names <- remove_yr(names)
names <- as.array(names)

colnames(dataset_17) <- names
colnames(dataset_16) <- names

#Extracting Attributes of Dataset Variables
attributes <- dataset_17 %>%  map_dfc(attr, "label")
attributes <- as.data.frame(t(attributes))
rownames(attributes) <- names

#checking for missing data
missing_dt_17 <- as.data.frame(colSums(is.na(dataset_17)))
missing_dt_16 <- as.data.frame(colSums(is.na(dataset_16)))

#only utilizing non-NA included rows
dataset_17 <- dataset_17[complete.cases(dataset_17),]
dataset_16 <- dataset_16[complete.cases(dataset_16),]

#adding a year column to distinguish two different year values
dataset_17$year <- 2017
dataset_16$year <- 2016

#inner joining the two datasets in order to create a complete "mydata"
c_16 <- as.data.frame(dataset_16$country)
c_17 <- as.data.frame(dataset_17$country)

colnames(c_16) <- "country"
colnames(c_17) <- "country"

com_1 <- merge(c_16,c_17, by = "country")

dataset_17 <- merge(com_1, dataset_17, by = "country")
dataset_16 <- merge(com_1, dataset_16, by = "country")

#restructuring the mydata dataset such that "year" comes first
mydata <- rbind(dataset_16,dataset_17)
mydata <- mydata[,c(194,1:193)]

#the two variables contain same information as "country" therefore Nullified.
mydata[,3:4] <- NULL


########################################################################################
################################# SHINY APP VISUALIZATION ##############################
########################################################################################

########################################################################################
##################################### SHINY APP UI  ####################################
########################################################################################
options(rgl.useNULL = TRUE)


ui = fluidPage(
  
  tabsetPanel(
    
    #Panel 1: Welcome, Opening Page
    tabPanel("About",
             sidebarLayout(
               sidebarPanel( position = "left",
                 h3("About the GEM dataset", align = "center"),
                 p("Global Entreprenuer Monitor (GEM) dataset tracks and assesses the difference in global entrepreneurs and their characteristics. 
                    And in these series of visualizations, the global version was utilized. Global
                    version for 2016 and 2017 was used for the purpose of tracking the change in responses over the period of 1 year.
                    Although 2014 and 2015 dataset had consistent datsets, there were many missing values therefore could not inner join with many outcomes."),
                 br(),
                 p("The source data is publically available versions upto year 2017 and can be found in:"),
                 p("https://www.gemconsortium.org/data/sets?id=aps"),
                 br(),
                 p("The app and visualizations have been created by Dong Hyun (Veo) Chae"),
                 img(src = "GEM.png", height = 72, width = 72)),
               
               mainPanel(
                 h1("2017 GEM Dataset (Individual)", align = "center"),
                 br(),
                 br(),
                 h2("Visualization 1: Data Exploratory:"),
                 p("The frist visualization is an interactive table where users can select and see different characteristics of the dataset"),
                 br(),
                 h2("Vizualization 2: Interacrtive Barplot"),
                 p("The frist visualization allows for users to see the change in statistics over the time span of 1 year"),
                 br(),
                 h2("Visualization 3: 3-D Comparison Plot"),
                 p("The Second visualization allows for users to see the relative positions of the countries by 3 dimensional variables"),
                 br(),
                 h2("Visualization 4:Comparative Line Plot"),
                 p("The third visualization allows for the users to see the trend of differing variables in regards to the countries development phases"),
                     )
                        )
             ),
    
    #Panel 2: Data Explorartory: Data Dictionary Page
    tabPanel("Data Dictionary",
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput("column", "Choose Columns to show:",
                                         choices = names(mydata),
                                         multiple = TRUE),
                             radioButtons("year_3", "Year",
                                        choices = unique(mydata$year))),
                             mainPanel(
                               DT::dataTableOutput("plot4"),
                               DT::dataTableOutput("plot5"))
                           )),
    
    #Panel 3: Barplot Visualization
    tabPanel("Interactive Barplot",
             sidebarLayout( position = "right",
                           sidebarPanel(
                               selectInput("country", "Country:",
                                           choices <- unique(mydata$country),
                                           multiple = TRUE),
                               selectInput("variable", "Variable:",
                                           choices <- colnames(mydata))),
             mainPanel(
               imageOutput("plot1"))
                          )),
    
    #Panel 4: 3D Scatterplot Visualization
    tabPanel("Interactive 3D Scatterplot",
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput("country_2", "Country:",
                                         choices <- unique(mydata$country),
                                         multiple = TRUE),
                             selectInput("variable_2", "Var_2 (choose 3 var):",
                                         choices<- colnames(mydata),
                                         multiple = TRUE),
                             selectInput("year","Year:",
                                         choices <- unique(mydata$year))),
                             mainPanel(
                               rglwidgetOutput("plot2", width = 800, height = 600)
                             )
                           )),
    
    #Panel 5: Parallel Coordinate Visualization
    tabPanel("Parallel Coordinate Visualization",
             sidebarLayout(position = "right",
                           sidebarPanel(
                             selectInput("variable_3", "Variables:",
                                         choices <- colnames(mydata),
                                         multiple = TRUE),
                             radioButtons("year_2", "Year",
                                          choices = unique(mydata$year))
                           ),
                           mainPanel( 
                             plotOutput("plot3", width = 800, height = 600))
                                      )
                         )
    
    
    
    
            )
          )

########################################################################################
################################## SHINY APP SERVER ####################################
########################################################################################

server <- function(input,output){

  ################PLOT 1#################
  output$plot1 <- renderImage({
    
    outfile <- tempfile(fileext='.gif') #creating a temporary file to save the gif file for interactive visualization
    
    mydata_2 <- subset(mydata, mydata$country %in% input$country) #subsetting the rows with selected country inputs
    
    p = ggplot(mydata_2, aes(x=country, y=mydata_2[,input$variable], fill=CAT_GCR2)) + 
      geom_bar(stat='identity') + 
      labs(title = "Year: {frame_time}", x = "Country" , y = input$variable) +
      theme_bw() + 
      transition_time(year) +
      ease_aes("sine-in-out")
    
    anim_save("outfile.gif", animate(p,renderer = gifski_renderer(),width = 1000, height = 1000, end_pause = 15)) 
      #the animation save saves the outfile gif which was temporary with gifski renderer.
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE) #listing the frames that consist gif file and deleting them after the visualization
  
  ################PLOT 2#################
  save <- options(rgl.inShiny = TRUE)
  on.exit(options(save))
  
  output$plot2 <- 
    renderRglwidget({
      try(rgl.close())
      mydata_2 <- subset(mydata, mydata$country %in% input$country_2)
      mydata_2 <- subset(mydata_2, mydata_2$year %in% input$year)
      mycolors <- c('darkseagreen1', 'deeppink', 'cadetblue1')
      mydata_2$color <- mycolors[as.factor(mydata_2$CAT_GCR2)]
      plot3d( 
        x=mydata_2[,input$variable_2[1]], 
        y=mydata_2[,input$variable_2[2]], 
        z=mydata_2[,input$variable_2[3]], 
        col = mydata_2$color, 
        type = 's', 
        radius = 0.7,
        xlab=input$variable_2[1], 
        ylab=input$variable_2[2], 
        zlab=input$variable_2[3])
      rglwidget()
    })
  
  ################PLOT 3#################
  output$plot3 <- renderPlot({
    
    mydata_2 <- subset(mydata, mydata$year %in% input$year_2)
    
    ggparcoord(mydata_2,
               columns = match(input$variable_3,names(mydata_2)), groupColumn = 4, order = "anyClass",
               scale="globalminmax",
               showPoints = TRUE, 
               title = "No scaling",
               alphaLines = 0.3
    ) + 
      scale_color_viridis(discrete=TRUE) +
      theme(
        legend.position="right",
        plot.title = element_text(size=13)
      ) +
      xlab("")
  })
  
  ################PLOT 4#################
  output$plot4 <-  DT::renderDataTable({
    
    mydata_2 <- subset(mydata, mydata$year %in% input$year_3)
    
    DT::datatable(mydata_2[, input$column, drop = FALSE])
  })
  
  ################PLOT 5#################
  output$plot5 <-  DT::renderDataTable({
    
    DT::datatable(attributes[input$column, 1, drop = FALSE])
  })
  
  
}

shinyApp(ui = ui, server = server)


################################################################################
################################################################################
################################################################################


