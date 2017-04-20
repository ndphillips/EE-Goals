# ------------------------------
#  ShinyBandit
#
#   CODE SECTIONS
#
#   0: Load libraries
#   A: Setup Game
#     A1: Game parameters
#     A2: Data saving
#   B: Overall layout
#   C: Reactive values
#   D: Page layouts
#   E: Event (button) actions
#     F1: Page navigation buttons
#     F2: Event tracking
#   F: Save data
# ------------------

# --------------------------
# Section 0: Load libraries ----
# --------------------------
library(shiny)
library(shinyjs)
library(rdrop2)
library(dplyr)
library(yarrr)


# --------------------------
# Section A: Setup game     -----
# --------------------------

# Section A1: GAME PARAMETERS

condition <- 1

nTrials <- 25
nTrialsPractice <- 20
n.games <- 10 # inclusive practice game
game <- 1

m.practice <- 3
sd.practice <- 2

# number of draws, means and sds of the 2 options

m1 <- 3
m2 <- 2

sd1 <- 2
sd2 <- 8

# no goal

goal <- 90
goal.practice <- 25

bonus <- 150



# Option outcomes as a list
outcomes <- list()
for (ga in 1:n.games){
  if (ga == 1){
    outcomes.temp <- cbind(round(rnorm(nTrialsPractice, m.practice, sd.practice), 0),
                           round(rnorm(nTrialsPractice, m.practice, sd.practice), 0))
  }
  if (ga > 1){
    outcomes.temp <- cbind(round(rnorm(nTrials, m1, sd1), 0),
                           round(rnorm(nTrials, m2, sd2), 0))
  }
  
  outcomes[[ga]] <- outcomes.temp
}

options.n <- ncol(outcomes[[1]])



locations.r <- matrix(NA, ncol = 2, nrow = n.games)
# Randomize option locations
for (ga in 1:n.games){
  locations.r[ga, 1:2] <- sample(1:2)
  outcomes[[ga]] <- outcomes[[ga]][,locations.r[ga,]]
}

option.order <- NULL
for (rows in 1:nrow(locations.r)){
  option.order <- c(option.order, paste(locations.r[rows,], collapse = ";"))
}
option.order <- c(rep(option.order[1], nTrialsPractice), rep(option.order[2:length(option.order)], each = nTrials))

# Maximum number of trials in game (will be updated if practice and game are different)

#trials.max <- nTrialsPractice



# Section A2: DATA SAVING

# saveDataLocation <- "local"           # Either dropbox, email, or local
# outputDir <- getwd()  # Directory to save data

# --------------------------
# Dropbox Parameters
# --------------------------

EPtoken <- readRDS("EP_droptoken.rds")          # Reads in authentication for EP dropbox
outputDir <- "msteiner/GoalBanditJava/data"          # Determine dropbox output folder
idDir <- "msteiner/GoalBanditJava/ids"
expContrDir <- "msteiner/GoalBanditJava/expControll"
link.i <- "https://econpsychbasel.shinyapps.io/Questionnaire1/"

linkPage =paste0("location.href='",link.i , "';")



# --------------------------------
# Section B: The user Interface and its JavaScript logic to run the game----
# -------------------------------

ui <- fixedPage(
  
  title = "Boxes Game",
  uiOutput("MainAction"),
  includeCSS("style.css"),
  includeScript("script.js"),
  useShinyjs()
                   )


server <- function(input, output, session) {
  
  # --------------------------------
  # Section C: Define Reactive Values ----
  #   These store the main values in the game
  # --------------------------------
  
  # CurrentValues stores scalers representing the latest game outcomes
  
  # CurrentValues stores scalers representing the latest game outcomes
  CurrentValues <- reactiveValues(page = "welcome",
                                  game = 1,
                                  trials.max = nTrials,
                                  nGoalsReached = 0)
  
  # GameValues stores vectors of histories
  GameData <- reactiveValues(trial = c(),          
                             time = c(),
                             selection = c(),
                             outcome = c(),
                             points.cum = c(),
                             game = c())
  
  
  # --------------------------------
  # Section D: Page Layouts ----
  # --------------------------------
  
  # Send the maximum pumpvalue to the Javascript part, so it knows how big to draw the balloon:

  # Send dynamic UI to ui - DON'T CHANGE!
  output$MainAction <- renderUI( {
    PageLayouts()
  })
  
  PageLayouts <- reactive({
    
    # 1) WELCOME PAGE
    if (CurrentValues$page == "welcome") {
      
      return(
        div(class = "welcome", checked = NA,
            list(
              tags$br(), tags$br(), tags$br(),
              h1("Decision Making", class = "firstRow"),
              p("If you consent to participating in this study, please enter your mturk WorkerID below and click Continue."),
              textInput(inputId = "workerid", 
                        label = "Please enter your WorkerID", 
                        value = "", 
                        placeholder = "e.g.; AXLDKGHSJM"),
              # This displays the action putton Next.
              tags$br(),
              disabled(actionButton(inputId = "gt_inst1", 
                                    label = "Continue", class = "continueButtons"))
            )
        )
      )}
    
    # INSTRUCTIONS INTRO
    
    if (CurrentValues$page == "inst1") {
      return(
        div(class = "inst", checked = NA,
            list(
              tags$br(), tags$br(),
              h2("Please read the instructions carefully!", class = "firstRow"),
              p("In this study, you will play the Boxes Game and then answer a few brief questionnaires. In total, this HIT should take around 20 minutes to complete."),
              p("Please note that we are running this HIT on a server that occasionally, but rarely, crashes. If the HIT crashes while you are completing it, it is important that you let us know right away. If this happens please email us at turkpsych@gmail.com and we will give you further instructions."),
              p("As you play the Boxes game, you will have the opportunity to gain (or lose) points. You will play the Boxes game 10 times. Your goal is to reach a certain amount of points in as many of the 10 games as possible. Each time you reach this minimum amount of points you will earn a bonus of 20 cents."),
              p("On the next page we explain how the Boxes game works. Please read the instructions carefully.  The better you understand the instructions, the more points you will earn in the games."),
              tags$br(),
              actionButton(inputId = "gt_inst2", 
                           label = "Continue", class = "continueButtons") 
            )
        )
      )
    }
    # 3) INSTRUCTIONS
    if (CurrentValues$page == "inst2") {
      
      return(
        div(class = "inst", checked = NA,
            list(
              tags$br(),
              h2("The Boxes Game", class = "firstRow"),
              p(paste("There are two boxes in the game.")),
              p(paste("Each box contains a mixture of negative and positive point values, however, the distribution of values is different in each box. Some boxes have, on average, higher points than others, and some boxes have a wider range of point values than others. When you start a game, you won't know anything about each box. Over the course of the game, you can learn about boxes and earn (or lose) points by using a budget of", nTrials, "clicks.")),
              h3(paste("Using", nTrials, "clicks to learn about boxes and earn points")),
              tags$p("At the top of the screen you will always see three important pieces of information: The number of clicks you have remaining in the game, the total number of points you have earned so far in the game and the goal."),
              h3("Here is a screenshot of how the game will look:"),
              tags$br(),
              fixedRow(column(12, align ="center", tags$img(src = "instGoal.png"))),
              # fixedRow(column = 12, plotOutput("InstructionDisplay")),
              # fixedRow(column = 12, plotOutput("resultsDisplayInstructions")),
              p(paste("To use one of your", nTrials, "clicks, click in one of the boxes. When you do this, the computer will randomly select one of the box's point values. This point value will be displayed in the box. The drawn point value will then be added to (or substracted from) your point total. The number of clicks you have remaining will then decrease by 1. When you have 0 clicks remaining, the game will end.")),
              h3("Points are returned to the boxes"),
              p("The computer will always draw a random point value from the box and will always return that point value back to the box. In other words, the distribution of point values in each box",  strong("will not change over time as a result of your clicks.")),
              p("The total points of all games will be summed together and used to calculate your bonus payment. The more points you earn the higher your bonus will be."),
              tags$br(),
              actionButton(inputId = "gt_goalInst", label = "Continue", class = "continueButtons"),
              tags$br(),tags$br(),tags$br()
            )
        )
      )}
    
    
    # 3.5) GOAL INSTRUCTIONS
    if (CurrentValues$page == "goalInst") {
      
      return(
        div(class = "inst", checked = NA,
            list(
              tags$br(), tags$br(),
              h2("The Goal", class = "firstRow"),
              tags$p("At the top of the screen you will always see three important pieces of information: The number of clicks you have remaining in the game, the total number of points you have earned so far in the game and a ", strong("goal of 90 points.")),
              h3("End the game with at least 90 points to gain bonus points!"),
              p("If, at the end of your game, you have earned at least 90 points, you", strong(paste("will earn an extra bonus of 20 cents."))),
              p("For example:"),
              p("If you", strong("end a game with 70 points"), " (less than the goal of 90) you won't receive the goal-bonus and will earn  no additional money for that game."),
              p("If you", strong("end a game with 100 points"), "(more than the goal of 90), then you", em("will"), "receive the goal-bonus of 20 cents."),
              p("You will only receive the bonus if you", strong("end"), "the game with at least as many points as the goal. This means that, even if you are above the goal in the middle of the game, if you end the game with fewer points than the goal, then you won't earn the bonus."),
              p("Again, you will play 10 different games. Your bonus across all games will be added together and determine your final monetary bonus. For example if in the 10 games you reach the goal 3 times, you will receive an extra 3 x 20 cents, i.e. 0.6$."),
              tags$br(),
              actionButton(inputId = "gt_inst3", label = "Continue", class = "continueButtons")
            )
        )
      )}
    
    # 4) PRACTICE GAME INSTRUCTIONS
    
    if (CurrentValues$page == "inst3") {
      return(
        div(class = "inst", checked = NA,
            list(
              tags$br(), tags$br(),
              h2("Play a Practice Game", class = "firstRow"),
              p("Now you can play a practice game."),
              p(paste("In the practice game, you will have", nTrialsPractice, "clicks to see how the interface works.")),
              p("The points you earn in the practice game don't matter, and all the boxes in the practice game have the same point values, so feel free to play around and experiment."),
              p(strong(paste0("When you finished a game, that is, once you arrive at trial ", nTrialsPractice, ", a button labeled \"Click to Continue to next Game\" will appear. Click it to continue..."))),
              tags$br(),
              actionButton(inputId = "gt_practicegame", 
                           label = "Start Practice Game", class = "continueButtons") 
            )
        )
      )
    }
    
    
    # 3) practice game PAGE
    if (CurrentValues$page %in% c("practicegame", "game")) {
      
      session$sendCustomMessage(type = 'envHandler', list(eno = outcomes[[CurrentValues$game]][,1],
                                                          ent = outcomes[[CurrentValues$game]][,2],
                                                          ens = outcomes[[CurrentValues$game]][,2] + sample(-2:2, 1),
                                                          env = outcomes[[CurrentValues$game]][,1] + sample(-2:2, 1),
                                                          nTrials = ifelse(CurrentValues$page == "game", nTrials, nTrialsPractice),
                                                          game = CurrentValues$game,
                                                          goal = ifelse(CurrentValues$page == "game", goal, goal.practice)))
        
        return(
          list(
            # Main Display: Contains both a pump and a save button
            fixedRow(
              tags$script('newGame();'),
              column(12,
                     fixedRow(tags$br()),
                     fixedRow(
                       column(3, align="center", h3(class = "upperParams", "Clicks Remaining")),
                       column(6, align="center", h3(class = "upperParams", "Points Earned")),
                       column(3, align="center", h3(class = "upperParams", "Goal"))
                     ),
                     fixedRow(
                       column(3, align="center", p(id = "clicksRemaining",
                                                   paste(ifelse(CurrentValues$page == "game", nTrials, nTrialsPractice)))),
                       column(6, align="center", p(id = "pointCounter", "0")), # This is updated via JavaScript
                       column(3, align="center", p(id = "goalvalue", paste(ifelse(CurrentValues$page == "game", goal, goal.practice))))
                     ),
                     fixedRow(tags$br()),
                     fixedRow(
                       column(1, align="center",
                              HTML('<h1 id="emptySpace1" class="emptySpace">Place</h1>')),
                       column(5, align="center",
                              HTML('<h1 id="deck1" class="decks" onclick="updateValue(\'deck1\', \'deck2\', \'pointCounter\',
                                  \'clicksRemaining\', ens, eno, env, ent, ind, 1, outcome, outcomeCum, selection, nTrials, gameNr,
                                  respTime, trial, t, goal)"> </h1>')),
                      # column(2, align="center",
                      #        HTML('<p id="emptySpace2" class="emptySpace">Place</p>')),
                      column(5, align="center",
                             HTML('<h1 id="deck2" class="decks" onclick="updateValue(\'deck2\', \'deck1\', \'pointCounter\',
                                  \'clicksRemaining\', env, ent, ens, eno, ind, 2, outcome, outcomeCum, selection, nTrials, gameNr, respTime,
                                  trial, t, goal)"> </h1>')),
                      column(1, align="center",
                             HTML('<h1 id="emptySpace2" class="emptySpace">Place</h1>'))),

                     fixedRow(
                       column(6,
                              hidden(actionButton("continueGame", label = "Click to Continue to next Game",
                                                  style =  "margin-top: 2em; margin-left: 5.3em; margin-bottom: 3em" )),
                              offset =  4))))
            )
          )
      }

    # 6) POST PRACTICE GAME
    if (CurrentValues$page == "postPractice"){
      return(
        div(class = "gameInfo", checked = NA,
            list(
              tags$br(), tags$br(),
              h2("Finished with Practice Game", class = "firstRow"),
              p("You are now finished with the practice game. On the next pages, you'll start playing the first of 10 real games that will count towards your bonus!"),
              p("Here are a few additional notes and reminders about the game:"),
              tags$ul(
                tags$li("You will play 10 games in total. Your bonus will be based on the sum of all the points you earn across all five games. If you earn a negative number of points in one game, they will be subtracted from your earnings in other games. Therefore, you should always try to maximize your point earnings and minimize any point losses."),
                tags$li("The boxes are the same in each game. However, the", strong("locations of the boxes will be randomly determined"), "at the start of each game. The boxes might be in the same location, or different locations, in each game."),
                tags$li("The point values in the boxes", strong("do not change over time."), " Each time you choose and option, the point value you see is always returned to the box."),
                tags$li(strong("Remember, each time you reach the goal by the end of a game you earn a bonus of 20 cents!"))
              ),
              p(strong("On the next page the first real game will start. Click to continue when you are ready.")),
              tags$br(),
              actionButton(inputId = "gt_game", 
                           label = "Start Game 1", class = "continueButtons") 
            )
        )
      )
    }
    
    # 4) END OF GAME PAGE
    if (CurrentValues$page == "pageEndGame") {
      return(
        div(class = "gameInfo", checked = NA,
            list(
              tags$br(), tags$br(),
              h3(paste("You finished game", CurrentValues$game - 2, "!"), class = "firstRow"),
              p(paste("You earned", GameData$points.cum[length(GameData$points.cum)], "points in the game.")),
              if (GameData$points.cum[length(GameData$points.cum)] < goal){p(paste("Your goal was", goal, "points. Because you did not reach the goal you do"), strong(" not "), paste("earn the 20 cents bonus for this game."))},
              if (GameData$points.cum[length(GameData$points.cum)] >= goal){p(paste("Your goal was", goal, "points. Because you"), strong(" did "), paste("reach the goal you earn the 20 cents bonus for this game."))},
              p("Please click continue to play the next game. Again, your bonuses across all 10 games that you will play will be added together to produce your total bonus."),
              p("As a reminder, the next game will have the same boxes as the previous game(s). However, the positions of the boxes will be randomly shuffled when the game starts."),
              tags$br(),
              actionButton(inputId = "gt_games", 
                           label = paste0("Start Game ", CurrentValues$game - 1), class = "continueButtons"))))
    }
    
    if (CurrentValues$page == "lastEndGame") {
      
      return(
        div(class = "gameInfo", checked = NA,
            list(
              tags$br(), tags$br(),
              h3("You finished all games!", class = "firstRow"),
              p(paste("You earned", GameData$points.cum[length(GameData$points.cum)], "points in the game.")),
              if (GameData$points.cum[length(GameData$points.cum)] < goal){p(paste("Your goal was", goal, "points. Because you did not reach the goal you do"), strong(" not "), paste("earn the 20 cents bonus for this game."))},
              if (GameData$points.cum[length(GameData$points.cum)] >= goal){p(paste("Your goal was", goal, "points. Because you"), strong(" did "), paste("reach the goal you earn the 20 cents bonus for this game."))},
              p(strong(paste0("You have now finished playing all 10 games. You have reached the goal ", CurrentValues$nGoalsReached, " times. Thus you receive an extra monetary bonus of ", CurrentValues$nGoalsReached, " x 20 cents, that is ", CurrentValues$nGoalsReached * .2, " $."))),
              tags$br(),
              actionButton(inputId = "gt_part2Inst", 
                           label = "Continue", class = "continueButtons"))))
    }
    
    if (CurrentValues$page == "part2Inst") {
      
      return(
        div(class = "inst", checked = NA,
            list(
              tags$br(), tags$br(),
              h3("Study Part 2", class = "firstRow"),
              p("You will now complete a few surveys about the game and how you make decisions in general."),
              p("Your answers to all future questions will not affect your bonus. However, please make sure to complete the rest of the survey for your work and bonus to be accepted."),
              p("Click on \"Continue\" to  start with the second part of the study. You will have to enter your workerid again on the next page."),
              tags$br(),
              actionButton(inputId = "gt_Questionnaire", 
                           label = "Continue", class = "continueButtons", onclick =linkPage))))
    }
    
  })
  
  
  
  # --------------------------------
  # Section E: Event (e.g.; button) actions ----
  # --------------------------------
  
  # Section F1: Page Navigation Buttons
  observeEvent(input$gt_inst1, {CurrentValues$page <- "inst1"})
  observeEvent(input$gt_inst2, {CurrentValues$page <- "inst2"})
  observeEvent(input$gt_goalInst, {CurrentValues$page <- "goalInst"})
  observeEvent(input$gt_inst3, {CurrentValues$page <- "inst3"})
  observeEvent(input$gt_practicegame, {CurrentValues$page <- "practicegame"})
  observeEvent(input$continueGame, {
    if (CurrentValues$game == 1){
      CurrentValues$page <- "postPractice"
    } else { if (CurrentValues$game %in% c(2:(n.games - 1))){
      CurrentValues$page <- "pageEndGame"
    } else {
      CurrentValues$page <- "lastEndGame"
    }
    }
    
    CurrentValues$game <- unique(GameData$game)[length(unique(GameData$game))] + 1
    # if (CurrentValues$game == 1){
    #   CurrentValues$game <- 2
    # } else {
    #   if (CurrentValues$game == 2){
    #     CurrentValues$game <- 3
    #   } else {
    #     if (CurrentValues$game == 3){
    #       CurrentValues$game <- 4
    #     } else {
    #       if (CurrentValues$game == 4){
    #         CurrentValues$game <- 5
    #       } else {
    #         if (CurrentValues$game == 5){
    #           CurrentValues$game <- 6
    #         } else { 
    #           if (CurrentValues$game == 6){
    #             CurrentValues$game <- 7
    #           } else {
    #             if (CurrentValues$game == 7){
    #               CurrentValues$game <- 8
    #             } else {
    #               if (CurrentValues$game == 8){
    #                 CurrentValues$game <- 9
    #               } else {
    #                 CurrentValues$game <- 10 
    #               }
    #             }
    #           }
    #         }
    #       }
    #     }
    #   }
    # }
    
    })
  observeEvent(input$gt_game, {CurrentValues$page <- "game"})
  observeEvent(input$gt_games, {CurrentValues$page <- "game"})

  # Section F2: Event tracking buttons

  observeEvent(input$gameNr,{
    if (length(input$gameNr == CurrentValues$game) == ifelse(CurrentValues$game == 1, nTrialsPractice , nTrials)){
      index <- (length(input$trial) - length(input$gameNr == CurrentValues$game)) : length(input$trial)
      toggle("continueGame")
      GameData$trial <- c(GameData$trial, input$trial[index])
      GameData$time <- c(GameData$time, input$respTime[index])
      GameData$selection <- c(GameData$selection, input$selection[index])
      GameData$outcome <- c(GameData$outcome, input$outcome[index])
      GameData$points.cum <- c(GameData$points.cum, input$outcomeCum[index])
      GameData$game <- c(GameData$game, input$gameNr[index])
      if (input$outcomeCum[length(input$outcomeCum)] >= goal){
        CurrentValues$nGoalsReached <- CurrentValues$nGoalsReached + 1
      }
    }
  })

  
  # --------------------------------
  # Section F: Save data ---- Commented out for now
  # --------------------------------
  observeEvent(input$gt_part2Inst, {
    
    # Create progress message
    withProgress(message = "Saving data...",
                 value = 0, {
                   
                   incProgress(.25)
                   
                   GameData.i <- data.frame("trial" = GameData$trial,
                                            "time" = GameData$time,
                                            "selection" = GameData$selection, 
                                            "outcome" = GameData$outcome,
                                            "game" = GameData$game,
                                            "points.cum" = GameData$points.cum,
                                            "option.order" = option.order,
                                            "workerid" = input$workerid,
                                            "goal" = goal,
                                            "condition" = condition)
                   
                   
                   incProgress(.5)
                   
                   GameDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(GameData.i), "_g.csv")
                   GameDatafilePath <- file.path(tempdir(), GameDatafileName)
                   write.csv(GameData.i, GameDatafilePath, row.names = FALSE, quote = TRUE)
                   rdrop2::drop_upload(GameDatafilePath, 
                                       dest = outputDir, 
                                       dtoken = EPtoken)
                   
                   CurrentValues$page <- "part2Inst"
                   Sys.sleep(.25)
                   incProgress(1)
                   
                 })
    
  })
  
  observe({
    
    # Check if input was given and enable and disable the continue button
    if(CurrentValues$page == "welcome"){
      
      if(!is.null(input$workerid)){
        
        if(nchar(as.character(input$workerid)) > 4){
          
          enable("gt_inst1")
          
        }
      }
    }
    
    if(CurrentValues$page == "inst1"){
      onlyID <- data.frame("workerID" = input$workerid)
      # Write survey data 
      IDDatafileName <- paste0(input$workerid, as.integer(Sys.time()), digest::digest(onlyID), "_g.csv")
      IDDatafilePath <- file.path(tempdir(), IDDatafileName)
      write.csv(onlyID, IDDatafilePath, row.names = FALSE, quote = TRUE)
      rdrop2::drop_upload(IDDatafilePath, dest = idDir, dtoken = EPtoken)
      
    }
  })
  
}

# Create app!
shinyApp(ui = ui, server = server)
