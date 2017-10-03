#server.R
library(shiny)
library(shinydashboard)
library(proxy)
library(recommenderlab)
library(reshape2)
library(plyr)
library(dplyr)
library(DT)
library(RCurl)

d <- file.path("C:","Users", "ellen", "Desktop","moviereco")
movies <- read.csv("moviesclean.csv", header = TRUE,stringsAsFactors=FALSE,encoding="UTF-8")
ratings <- read.csv("ratingsclean.csv", header = TRUE,fileEncoding="UTF-8")


server <- shinyServer(function(input, output) {
  output$name1 <- renderText({
    input$select
  })
  
  output$myImage <- renderImage({
    list(src = paste0("data/photo/", input$select, ".png"), width=201, height=300) 
  }, deleteFile = FALSE)
  
  output$des1 <- renderText({
    movies$des[movies$title == input$select]
  })
  
  
  
  
  
  Table <- reactive({
    movie_recommendation <- function(input){
      row_num <- which(movies[,3] == input)
      userSelect <- matrix(NA,length(unique(ratings$movieId)))
      userSelect[row_num] <- 5 
      userSelect <- t(userSelect)
      
      ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
      ratingmat <- ratingmat[,-1]
      colnames(userSelect) <- colnames(ratingmat)
      ratingmat2 <- rbind(userSelect,ratingmat)
      ratingmat2 <- as.matrix(ratingmat2)
      
      #Convert rating matrix into a sparse matrix
      ratingmat2 <- as(ratingmat2, "realRatingMatrix")
      
      #Create Recommender Model
      recommender_model <- Recommender(ratingmat2, method = "UBCF",param=list(method="Cosine",nn=30))
      recom <- predict(recommender_model, ratingmat2[1], type = "topNList", n=25)   
      recom_list <- as(recom, "list")
      recom_result <- data.frame(matrix(NA,25))
      recom_result[1:25,1] <- movies[as.integer(recom_list[[1]][1:25]),3]
      recom_result <- data.frame(na.omit(recom_result[order(order(recom_result)),]))
      recom_result <- as.character(recom_result[1:25,])
      # recom_result <- data.frame(recom_result[1:5,])
      # colnames(recom_result) <- " "
      set.seed(100)
      recom_result1 <- sample(recom_result,5,replace=FALSE)
      return(recom_result1)
      
    }
    
    movie_recommendation(input$select)
  })
  #ar <- sample(Table(),5,replace=FALSE)
  #sample(Table()[1:30], 3)
  #ar <- sample(c(Table()[1],Table()[2]),1)
######################################################################################  

  
  output$myImage1 <- renderImage({
    list(src = paste0("data/photo/", Table()[1], ".png"), width=100, height=150)
  }, deleteFile = FALSE)
  
  output$myreco1 <- renderText({
    Table()[1]
  })
 #~~~~~~~~~~~~~~~~~~~~~~~ 
  output$myImage2 <- renderImage({
    list(src = paste0("data/photo/", Table()[2], ".png"), width=100, height=150)
  }, deleteFile = FALSE)
  
  output$myreco2 <- renderText({
    Table()[2]
  })
#~~~~~~~~~~~~~~~~~    
    output$myImage3 <- renderImage({
      list(src = paste0("data/photo/", Table()[3], ".png"), width=100, height=150)
    }, deleteFile = FALSE)
    
    output$myreco3 <- renderText({
      Table()[3]
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$myImage4 <- renderImage({
      list(src = paste0("data/photo/", Table()[4], ".png"), width=100, height=150)
    }, deleteFile = FALSE)
    
    output$myreco4 <- renderText({
      Table()[4]
    })
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$myImage5 <- renderImage({
      list(src = paste0("data/photo/",Table()[5] , ".png"), width=100, height=150)
    }, deleteFile = FALSE)
    
    output$myreco5 <- renderText({
      Table()[5]
    })

    
  movie.ratings <- merge(ratings, movies)
  output$tableRatings1 <- renderValueBox({
    movie.avg1 <- summarise(subset(movie.ratings, title==input$select),
                            Average_Rating1 = mean(rating, na.rm = TRUE))
    valueBox(
      value = format(movie.avg1, digits = 3),
      subtitle = HTML("&nbsp;"),   #NULL,  #input$select
      icon = if (movie.avg1 >= 3) icon("thumbs-up") else icon("thumbs-down"),
      color = if (movie.avg1 >= 3) "aqua" else "red"
    )
    
  })
})
  
  
