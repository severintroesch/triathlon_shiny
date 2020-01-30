#############################
#### WSA Triathlon Shiny ####
#############################

library(shiny)
library(shinydashboard)
library(shinythemes)
library(plyr)
library(tidyverse)
library(lubridate)
library(forecast)

# https://shiny.rstudio.com/articles/generating-reports.html

#### get clean dataset ####
## load file
path <- "\\\\adb.intra.admin.ch/Userhome$/BASPO-01/U80822632/config/Desktop/R/Projekte_Analysen/shiny/app sevi wsa tri/data/world_champs_tri_2.csv"
tri_dat <- read.csv(path, header = T, sep = ";") # geht das so???

## clean file
#extract elite
tri_dat <- tri_dat %>% filter(program %in% c("Elite Men", "Elite Women")) #only elite

# haldle times (zeit-spalten mit h:m:s-fomrat aus excel in zeit (hms) konvertieren)
new_cols <- tri_dat %>% 
  select(c(7:11,13)) %>% 
  lapply(.,function(x) {
    hms(x) %>% as.numeric()
  }) %>% 
  data.frame()

# zeitspalten in altem df durch sekunden-spalten ersetzen
tri_new <- tri_dat
tri_new[,c(7:11,13)] <- new_cols

#total_position as numeric (position ist in "tri" faktor) und 0 durch NA ersetzen
tri_new[,"total_position"] <- as.numeric(paste(tri_new$total_position))
tri_new[tri_new==0] <- NA

# keep only finished races
tri_new <- tri_new %>% filter(total_time != "NA")

# add var with rank of nationality
nat_rank <- tri_new %>% 
  group_by(nationality) %>% 
  summarise(nat_median = median(total_position),
            anz_ath  = nlevels(as.factor(athlete_id))) %>% 
  filter(anz_ath > 10) %>% #only nations with more than 10 athletes
  arrange(nat_median) %>% 
  mutate(nat_rank = rank(nat_median)) %>% 
  select(1,4)

tri_new <- tri_new %>% left_join(nat_rank)


# simplify
tri <- tri_new #simpler name for code below


#### Define UI for application ####

ui <- navbarPage(
    theme = shinytheme("flatly"),
    title = 'Weltstandsanalyse Triathlon - V3, Jun18',
                 
                 # Allgeieine Infos Tab ----
                 tabPanel(title = "Beschreibung",
                         # static cext
                         h2("Interaktive Weltstandsanalyse Triathlon"),
                         h3("Weltmeisterschaften 1989 - 2017"),
                         br(),br(),
                         p(strong("Wo steht der Triathlonsport und wie entwickelt er sich?")), br(),
                         p(strong("Wie steht die Schweiz im 
                           internationalen Vergleich und wie entwickelt sie sich?")),
                         br(),br(),
                         p("Mit dieser Applikation können diese Fragen interaktiv analysiert werden. 
                           In den Abschnitten ", em("Nationenvergleich"), "und ", 
                           em("Leistungsentwicklung"), 
                           "stehen die entsprechenden Tools zur Verfügung. Die zugrundeliegenden Daten stammen aus der ", 
                         a(href = "https://www.triathlon.org/results#q=&hPP=15&idx=events_reverse_sort&p=0&dFR%5Bevent_categories.cat_name%5D%5B0%5D=World%20Championships&dFR%5Bspecification.cat_name%5D%5B0%5D=Standard&dFR%5Bsport.cat_name%5D%5B0%5D=Triathlon&fR%5Bfederation_event%5D%5B0%5D=false&is_v=1", "ITU Resultatedatenbank"),
                         " und umfassen alle WM-Rennen (Triathlon World Championships bis 2008 und Triathlon Grand Finals ab 2009)."),
                         br(),
                         p("Das Tool soll gebraucht werden, um in den ITU-Daten interessante Zusammenhänge zu finden..."), 
                         br(), br(),br(),br(),br(),
                         p(em("Kontakt:")), 
                         p("severin.troesch@baspo.admin.ch"),
                         br(), 
                         img(src="sticker.jpg")),
                 
                 # Nationenvergleiche-Tab I: Topplatzierungen ----
    
    navbarMenu("Nationenvergleich", #fuer beide nationenvergleiche-panels
               
                 tabPanel(title = "Topplatzierungen",
                          # static cext
                          h2("Nationenvergleich an Weltmeisterschaften I"),
                          h3("Topplatzierungen"),
                          br(),
                          p("Hier können Nationen bezüglich erreichter Topplatzierungen (Podest oder Top-10 Ränge) miteinander 
                            verglichen werden."), 
                          br(),
                          p(em("Anwendung:")),p(" Interessierende ", 
                            strong("Kategorie, Rangierung"), "und ", strong("Zeitperiode"),
                            " wählen. Der Plot zeigt für alle Nationen die 
                            durchschnittliche Anzahl Personen pro Jahr,
                            die der Auswahl entsprechen. Die Tabelle zeigt die
                            Gesamtanzahl an Personen, die der Auswahl entsprechen."),
                          hr(),
                          
                          sidebarLayout(
                            
                            # button input to choose gender
                            sidebarPanel(
                              radioButtons(inputId = "gender1",
                                           label = "Kategorie wählen:",
                                           choices = list("Elite Women", "Elite Men"),
                                           selected = "Elite Women"
                              ),
                              
                              
                              # button input to choose position, ACHTUNG: in server as.numeri()
                               radioButtons(inputId = "position1",
                                            label = "Rangierung wählen:",
                                            choices = list("Top 3" = 3, "Top 10" = 10),
                                            selected = 10
                              ),
                              
                              # choose year range
                              sliderInput(inputId = "year1",
                                          label = "Zeitperiode wählen (von - bis [Jahr]):",
                                          min = min(tri$year), 
                                          max = max(tri$year),
                                          value = c(2000, 2010),
                                          step = 1,
                                          sep = "")
                            ),
                            
                            # Show a plot
                            mainPanel(
                              fluidRow(
                                splitLayout(#cellWidths = c("70%", "30%"), 
                                            plotOutput(outputId = "plot1"), 
                                            tableOutput(outputId = "tab1")
                                            )
                              )
                              
                              
                            )
                          )
                        ),
                 
                 # Nationenvergleiche-Tab II: nach Disziplin ----
                 tabPanel(title = "Disziplinen",
                          
                          # static cext
                          h2("Nationenvergleich an Weltmeisterschaften II"),
                          h3("Disziplinen"),
                          br(),
                          p("Hier können Nationen bezüglich ihrer Leistungen (erreichte Zeiten) in den drei Disziplinen verglichen werden."), 
                          br(),
                          p(em("Anwendung:")),p(" Interessierende ", 
                                                strong("Kategorie"), "und ", strong("Zeitperiode"),
                                                " wählen. Die Plots zeigen für die Top-Nationen, 
                                                aufgeteilt nach Disziplin, die Verteilung der erreichten Zeiten
                                                durch Personen, die der Auswahl entsprechen."),
                          hr(),
                          
                          sidebarLayout(
                            
                            # button input to choose gender
                            sidebarPanel(
                              radioButtons(inputId = "gender2",
                                           label = "Kategorie wählen:",
                                           choices = list("Elite Women", "Elite Men"),
                                           selected = "Elite Women"
                              ),
                              
                              
                              # choose year range
                              sliderInput(inputId = "year2",
                                          label = "Zeitperiode wählen (von - bis [Jahr]):",
                                          min = min(tri$year), 
                                          max = max(tri$year),
                                          value = c(2000, 2010),
                                          step = 1,
                                          sep = "")
                            ),
                            
                            # Show plots
                            mainPanel(
                              tabsetPanel(
                                tabPanel(title = "Schwimmen",
                                         plotOutput(outputId = "plot2")),
                                tabPanel(title = "Rad",
                                         plotOutput(outputId = "plot3")),
                                tabPanel(title = "Laufen",
                                         plotOutput(outputId = "plot4"))
                                )
                              )
                            )
                          )
                        ),
                 
                 # Leistungsentwicklung-Tab ----
             
                 tabPanel(title = "Leistungsentwicklung",
                          
                          # static cext
                          h2("Leistungsentwicklung an Weltmeisterschaften"),
                          h3("Entwicklungsverläufe"),
                          br(),
                          p("Hier wird die Leistungsentwicklung in den einzelnen Disziplinen für die Gesamtränge 
                            3, 10 und 20 dargestellt (schwarze Linie) und eine Vorhersage gemacht, wir sich die Zeiten 
                            in den nächsten fünf Jahren entwickeln könnten (blaue Linie ± Unsicherheit; sog. ets-Modell). Zum Vergleich wird die 
                            Entwicklung der Schweiz dargestellt (graue Linie; jeweils Median aller CH-Resultate)."), 
                          br(),
                          p(em("Anwendung:")),p(" Interessierende ", 
                                                strong("Kategorie"), "und ", strong("Gesamtrangierung"),
                                                " wählen. Die Plots zeigen, 
                                                aufgeteilt nach Disziplin, die Entwicklung der erreichten Zeiten
                                                durch Personen, die der Auswahl entsprechen."),
                          hr(),
                          
                          sidebarLayout(
                            
                            # button input to choose gender
                            sidebarPanel(
                              radioButtons(inputId = "gender3",
                                           label = "Kategorie wählen:",
                                           choices = list("Elite Women", "Elite Men"),
                                           selected = "Elite Women"
                                          ),
                              
                              
                            # button input to choose position, ACHTUNG: in server as.numeric()
                            radioButtons(inputId = "position2",
                                                         label = "Gesamtrang wählen:",
                                                         choices = list("Rang 3" = 3, "Rang 10" = 10, "Rang 20" = 20),
                                                         selected = 10
                                           )
                            ),
                            
                            # Show a plot
                            mainPanel(
                                  tabsetPanel(
                                    tabPanel(title = "Schwimmen",
                                             plotOutput(outputId = "plot5")),
                                    tabPanel(title = "Rad",
                                             plotOutput(outputId = "plot6")),
                                    tabPanel(title = "Laufen",
                                             plotOutput(outputId = "plot7"))
                                )
                              )
                            )
                          )
                        ) #navbar Page

#### Define server logic ####
server <- function(input, output) {
  
  
  ## Tab Nationenvergleich I ---------------------------------------------------
  
  data <- reactive({
    
    # generate dataset for Nationenvergleich I based on input from ui.R 
   tri %>% 
      filter(program == input$gender1) %>% #filter for gender (user input)
      filter(between(year, input$year1[1], input$year1[2])) %>% #filter year via userinput (slider min max)
      filter(total_position <= as.numeric(input$position1)) %>% # filter nach gewuenschten raengen (user sagt 3 oder 10)
      group_by(year, nationality) %>% 
      summarise(nn = n())  %>%  #nn = wieviel mal in raengen pro nation und jahr
      complete(year, nationality,fill = list(nn = 0)) %>% # set 0 fo all year-nation combinations ohne rangierung
      ungroup() %>% 
      group_by(nationality) %>% 
      summarise(mean = mean(nn), #wievielmal durchschnittlich in raengen pro nation in zeitrange
                se = sd(nn)/n(), # sd of erreichte rangierungen
                sum = sum(nn)) %>% #sum of rangierungen - to filter in next line
      filter(sum > 0) %>% #exclude nations that had only 1 rangierung
      mutate(SUI = ifelse(nationality == "SUI",yes = "ja",no = "nein")) %>% 
      arrange(desc(mean))
  })
  
    #Tabelle zu nationenvergleich I
  output$tab1 <- renderTable({
    data() %>% transmute(Nation = nationality,
                         Anzahl = as.integer(sum))
  })


    #Plot zu nationenvergleich I
  output$plot1 <- renderPlot({
       
       # draw plot
      ggplot(data(), aes(x = reorder(nationality, -mean), 
                        y = mean, 
                        ymin = mean-se, 
                        ymax = mean+se,
                        fill = SUI))+
        geom_col()+
        geom_linerange() +
        theme_bw() +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.9, 0.9))+
        scale_fill_manual(values = c("ja" = "red", "nein" = "gray")) +
        labs( x = "Nation", 
              y = "Anzahl Personen in Rangierung pro Jahr")
    
    })
  
  
  ## Tab Nationenvergleich II ---------------------------------------------------
  
  data2 <- reactive({
    
    # generate dataset for Nationenvergleich I based on input from ui.R 
    tri %>% 
      filter(program == input$gender2) %>% #filter for gender (user input)
      filter(between(year, input$year2[1], input$year2[2])) %>% #filter year via userinput (slider min max)
      filter(nat_rank<=15) %>% #remove uninteresting nations
      filter(between(swim,left = 10*60, right = 22*60), 
             between(bike,left = 45*60, right = 90*60),
             between(run,left = 28*60,right =  40*60)) %>% #remove funny values
      mutate(SUI = ifelse(nationality == "SUI",yes = "ja",no = "nein")) %>%
      group_by(nationality) %>%
      
      mutate(median_swim = median(swim, na.rm = T),
             median_bike = median(bike, na.rm = T),
             median_run = median(run, na.rm = T))
    
  })
  
  #Plot1 zu nationenvergleich II
  output$plot2 <- renderPlot({
    
    # draw plot
    ggplot(data2(), aes(x = reorder(nationality, median_swim), 
                        y = swim/60,
                        fill = SUI))+
      geom_boxplot()+
      theme_bw() +
      theme_bw() +
      theme(
      #   panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.1, 0.9))+
      scale_fill_manual(values = c("ja" = "red", "nein" = "gray")) +
      labs( x = "Nation", 
            y = "Schwimmzeit (min)")
    
  })
  
  #Plot2 zu nationenvergleich II
  output$plot3 <- renderPlot({
    
    # draw plot
    ggplot(data2(), aes(x = reorder(nationality, median_bike), 
                        y = bike/60,
                        fill = SUI))+
      geom_boxplot()+
      theme_bw() +
      theme_bw() +
      theme(
      # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.1, 0.9))+
      scale_fill_manual(values = c("ja" = "red", "nein" = "gray")) +
      labs( x = "Nation", 
            y = "Radzeit (min)")
    
  })
  
  #Plot3 zu nationenvergleich II
  output$plot4 <- renderPlot({
    
    # draw plot
    ggplot(data2(), aes(x = reorder(nationality, median_run), 
                        y = run/60,
                        fill = SUI))+
      geom_boxplot()+
      theme_bw() +
      theme_bw() +
      theme(
      # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.1, 0.9))+
      scale_fill_manual(values = c("ja" = "red", "nein" = "gray")) +
      labs( x = "Nation", 
            y = "Laufzeit (min)")
    
  })
  
  ## Tab Leistungsentwicklung ---------------------------------------------------
  
  data3 <- reactive({
    
    # generate first dataset for Leistungsentwicklung based on input from ui.R 
    tri %>% 
      expand(year, program , total_position = 1:20) %>% #make explicit missing values at missing positions
      full_join(tri) %>% 
      filter(program == input$gender3) %>% #filter for gender (user input)
      filter(total_position == as.numeric(input$position2)) #%>% #choose total (!) rank based on input
  })
  
  
  data4 <- reactive({
    
    # generate second dataset for Leistungsentwicklung based on input from ui.R 
    tri %>% 
      filter(nationality == "SUI",
             program == input$gender3) %>% 
      group_by(year) %>% 
      summarise(median_sui_swim = median(swim),
                median_sui_bike = median(bike),
                median_sui_run  = median(run)
      )
  })
  
  
  #Plot1 zu leistungsentwicklung
  output$plot5 <- renderPlot({ #swim-plot
    
    # construct time series
    ts_swim <- ts(data = data3()$swim/60,
                  start =  min(data3()$year),
                  end = max(data3()$year),
                  frequency = 1)
    
    # draw plot
    ts_swim %>%  forecast(h = 5, robust = T) %>% autoplot()+
      guides(fill=guide_legend(title="Forecast CI (%)")) +
      theme_bw() +
         labs( x = "Jahr", 
               y = "Schwimmzeit (min)",
               title = "")+
      geom_smooth(data = data4(), 
                  mapping = aes(x = year, y = median_sui_swim/60, color = "Median SUI"),
                  se = F)+
      scale_color_manual("", 
                         breaks = c("Median SUI"),
                         values = c("gray")) +
      coord_cartesian(ylim = c(10, 35)) 
  })
  
  #Plot2 zu leistungsentwicklung
  output$plot6 <- renderPlot({ #bike-plot
    
    # construct time series
    ts_bike <- ts(data = data3()$bike/60,
                  start =  min(data3()$year),
                  end = max(data3()$year),
                  frequency = 1)
    
    # draw plot
    ts_bike %>%  forecast(h = 5, robust = T) %>% autoplot()+
      guides(fill=guide_legend(title="Forecast CI (%)")) +
      theme_bw() +
      labs( x = "Jahr", 
            y = "Radzeit (min)",
            title = "")+
      geom_smooth(data = data4(), 
                  mapping = aes(x = year, y = median_sui_bike/60, color = "Median SUI"),
                  se = F)+
      scale_color_manual("", 
                         breaks = c("Median SUI"),
                         values = c("gray")) +
      coord_cartesian(ylim = c(45, 80)) 
  })
  
  #Plot3 zu leistungsentwicklung
  output$plot7 <- renderPlot({ #run-plot
    
    # construct time series
    ts_run <- ts(data = data3()$run/60,
                  start =  min(data3()$year),
                  end = max(data3()$year),
                  frequency = 1)
    
    # draw plot
    ts_run %>%  forecast(h = 5, robust = T) %>% autoplot()+
      guides(fill=guide_legend(title="Forecast CI (%)")) +
      theme_bw() +
      labs( x = "Jahr", 
            y = "Laufzeit (min)",
            title = "")+
      geom_smooth(data = data4(), 
                  mapping = aes(x = year, y = median_sui_run/60, color = "Median SUI"),
                  se = F)+
      scale_color_manual("", 
                         breaks = c("Median SUI"),
                         values = c("gray")) +
      coord_cartesian(ylim = c(25, 45)) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
