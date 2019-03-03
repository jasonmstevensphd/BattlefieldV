
library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)

BFV <- read_xlsx("D:/Data_Science/BattlefieldV/Battlefield5.xlsx", sheet = "Sheet1") %>%
  filter(!is.na(Date)) %>%
  mutate(`Net_K/D` = Kills-Deaths)

MapList <- unique(BFV$Map)
GameModeList <- unique(BFV$GameMode)
SideList <- c("O", "N", "D", NA)
WeaponList <- unique(BFV$Weapon)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Battlefield V Companion"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("MapInput", "Select a Map", MapList, selected = "Fjell 652"),
         selectInput("GameModeInput", "Select the Game Mode", GameModeList, selected = "Conquest"),
         selectInput("SideInput", "Select your Side", SideList, selected = "N"),
         selectInput("WeaponInput", "Select your Weapon", WeaponList, selected = "STG 44")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Global Plot",
                   plotOutput("globalPlot")),
          tabPanel("Global Table",
                   dataTableOutput("globalTable")),
          tabPanel("Local Plot",
                   plotOutput("localPlot")),
          tabPanel("Local Table",
                   dataTableOutput("localTable"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$globalPlot <- renderPlot({
     
     globalPlot_Data <- BFV %>%
       filter(Map == input$MapInput) %>%
       filter(GameMode == input$GameModeInput) %>%
       filter(Side == input$SideInput) %>%
       group_by(Weapon) %>%
       summarise(mean_KD = mean(`K/D`))
     
     GlobalPlot <- ggplot(globalPlot_Data) +
       geom_point(aes(x = Weapon, y = mean_KD), size = 2)
     
     GlobalPlot
       
   })
   
   output$globalTable <- renderDataTable({
     
     globalTable_Data <- BFV %>%
       filter(Map == input$MapInput) %>%
       filter(GameMode == input$GameModeInput) %>%
       filter(Side == input$SideInput) %>%
       group_by(Weapon) %>%
       summarise(mean_KD = mean(`K/D`), Net_KD = mean(`Net_K/D`), Counts = n()) %>%
       arrange(desc(mean_KD))
     
     globalTable_Data
     
   })
   
   output$localPlot <- renderPlot({
     
     localPlot_Data <- BFV %>%
       filter(Map == input$MapInput) %>%
       filter(GameMode == input$GameModeInput) %>%
       filter(Side == input$SideInput) %>%
       filter(Weapon == input$WeaponInput)
     
     LocalPlot <- ggplot(localPlot_Data, aes(x = `K/D`)) +
       geom_histogram()
     
     LocalPlot
     
   })
   
   output$localTable <- renderDataTable({
     
     localTable_Data <- BFV %>%
       filter(Map ==input$MapInput) %>%
       filter(Side == input$SideInput) %>%
       filter(Weapon == input$WeaponInput) %>%
       filter(GameMode == input$GameModeInput) %>%
       select(Entry, Date, Round, `K/D`, `Net_K/D`, GameScore) %>%
       arrange(desc(`K/D`))
     
     localTable_Data
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

