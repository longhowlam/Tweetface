
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(curl)
library(jpeg)
library(DT)
library(stringr)

 
twitter_token = readRDS("twittertoken.rds")
source("Keys.R")

#faceURL = "https://api.projectoxford.ai/face/v0/detections?analyzesFaceLandmarks=true&analyzesAge=true&analyzesGender=true&analyzesHeadPose=true"
#faceURL = URLencode(faceURL)

faceURL = "https://api.projectoxford.ai/face/v1.0/detect?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair"

visionURL = "https://api.projectoxford.ai/vision/v1/analyses?visualFeatures=All"

URL.emoface = "https://api.projectoxford.ai/emotion/v1.0/recognize"

AllActors = readRDS("data/AllActors.rds")


createLink <- function(val, naam) {
  sprintf('<a href="%s" target="_blank"> %s </a>',val,naam)
}



#########  do image analysis with micorsoft oxford API

MicrosoftImageAPI = function (linkje, findsim)
{
  
  #################  Vision API
  
  
  mybody = list(url = linkje)
  
  visionRESO = POST(
    url = visionURL, 
    content_type("application/json"), add_headers(.headers = c("Ocp-Apim-Subscription-Key" = VisionKey)),
    body = mybody,
    encode = "json"
  )
  print("*** vision api ***")
  print(visionRESO)
  
  visionObject = content(visionRESO)
  print(visionObject$adult)
  print(visionObject$categories)
  print(visionObject$faces)
  IMtype = IMscore = ""
  for (j in 1:length(visionObject$categories))
  {
    IMtype = paste(IMtype, visionObject$categories[[j]]$name, sep=" ------  ")
    IMscore =  paste(IMscore, visionObject$categories[[j]]$score, sep=" -----  ")
  }

  
  #########################  face analysis
  
  mybody = list(url = linkje)
  
  faceRESO = POST(
    url = faceURL, 
    content_type("application/json"), add_headers(.headers = c("Ocp-Apim-Subscription-Key" = FaceKey)),
    body = mybody,
    encode = "json"
  )
  
  
  #### kenmerken van gezicht zijn in de content van het response object
  if(length(content(faceRESO))>0){
    faceDetails = content(faceRESO)[[1]]
    
    ## age en gender
    
    as.data.frame(faceDetails$faceLandmarks)
    GES = faceDetails$faceAttributes$gender
    Age = faceDetails$faceAttributes$age
    
    
    #####################   similarity check
    actricematch = "no match"
    if (findsim){
      sim.URI = "https://api.projectoxford.ai/face/v1.0/findsimilars"
      fID = content(faceRESO)[[1]]$faceId
      print(fID)
      mybody = list(faceID = fID, faceListID = "allactors"  )
      
      faceSIM = POST(
        url = sim.URI, 
        content_type("application/json"), add_headers(.headers = c("Ocp-Apim-Subscription-Key" = FaceKey)),
        body = mybody,
        encode = "json"
      )
      print(faceSIM)
      print(content(faceSIM))
      
     
      if( faceSIM$status_code == 200 ){
      
        yy = content(faceSIM)
        
        if(length(yy)>0){
          mt = AllActors[ yy[[1]]$persistedFaceId == AllActors$faceID,]
          print(mt)
          actricematch = mt$imgalts
        }
      }
      else{
        actricematch ="Something went wrong"
      }
    }
    
    ########### EMOTIE
    
    mybody = list(url = linkje)
    
    faceEMO = POST(
      url = URL.emoface, 
      content_type("application/json"), add_headers(.headers = c("Ocp-Apim-Subscription-Key" = Emotion)),
      body = mybody,
      encode = "json"
    )
    emoscores = content(faceEMO)[[1]]
    print(emoscores)
    return(
      list(
        h3("Microsoft Project Oxford Face API"),
        p("Image analysed by Microsoft face detection. Estimation for gender, age, facial hair and emotion"),
        img(src=linkje, height = 400, width = 500,align="center"),
        p(strong("Gender:"), GES),
        p(strong("Age:"), Age),
        p(h4("Facial hair scores (from 0 to 1)")),
        p(strong("Moustache"),faceDetails$faceAttributes$facialHair$moustache ),
        p(strong("Beard"),faceDetails$faceAttributes$facialHair$beard ),
        p(strong("Sideburns"),faceDetails$faceAttributes$facialHair$sideburns ),
        p(h4("EMOTIIONAL SCORES (from 0 to 1)")),
        p(strong("hapiness"), round(emoscores$scores$happiness,2) ),
        p(strong("sadness"), round(emoscores$scores$sadness,2) ),
        p(strong("fear"), round(emoscores$scores$fear,2) ),
        p(strong("anger"),round(emoscores$scores$anger,2) ),
        p(strong("Match found with"), actricematch)
      )
    )
  }
  else
  {
    return(
    list(
      h3("Microsoft Project Oxford Face API"),
      img(src=linkje, height = 400, width = 500,align="center"),
      p("Selected image was analysed by Microsoft face detection no face was recognized. Howver, the following was detected:"),
      p(strong("Type   "), IMtype),
      p(strong("Score  "), IMscore)
    )
    )
  }
}


shinyServer(function(input, output, session) {
  
  
  datasetTweets <- reactive({
    twitteradres = "https://api.twitter.com/1.1/search/tweets.json?q="
    tw.query = input$zoeksleutel
    tw.query = URLencode(tw.query, reserved=TRUE)
    URI = paste(
      twitteradres,
      tw.query,
      "&count=75",
      sep=""  
    )
    
    print(URI)
    r2 = GET(url = URI, config(token = twitter_token))
    print(r2$status_code)
    ## json output is in lijsten gezet en inhoud kun je uit lijsten pulleken
    r3 = content(r2)
    
    tweets = r3[[1]]
    NT = length(tweets)
    print(NT)
    tweet.text = ScreenName = img.urls = tweet.datum = profileImage = character(NT)
    if (NT > 0){
      for (i in 1:NT){
        eentweet      = tweets[[i]]
        
        tweet.datum[i] = eentweet$created_at
        tweet.text[i]  = eentweet$text
        entities       = eentweet$entities
        mediaent       = entities$media[[1]]
        img.urls[i]    = ifelse( is.null(mediaent$media_url), "", mediaent$media_url)
        ScreenName[i]  = eentweet$user$screen_name
      }
      
      TwitterSetje = data.frame(tweet.datum, ScreenName,  tweet.text, img.urls, profileImage, stringsAsFactors = FALSE)
      TwitterSetje$fotodirect = ifelse( TwitterSetje$img.urls != "", "Y","N")
     
      return(TwitterSetje[TwitterSetje$fotodirect=="Y", ])
    }
    else{
      return(data.frame(tweetdatum="",ScreenName="",tweet.text=""))
    }
  })

  
  output$inleiding <- renderUI({
    
    list(
      h3("Explanation face analyzer"),
      p("Type in a search string to retrieve tweets, only tweets with an image will be retrieved"),
      p("Select a tweet then it will display the image and do a facial analyis with the Microsoft project oxford API."),
      p("If you are looking for a certain screenname, type in the screenname and check the analyse profile image."),
      p("The image is also compared with a  list of ", a(href="http://www.imdb.com/list/ls050128191/","100 most sexy actresses"), "and", a(href="http://www.imdb.com/list/ls000004615/", "a top 100 actors list")),
      p(strong("Privacy note, The tweet pictures (which are already public anyway) are sent to the Microsoft cloud! ")),
      p(strong("Warning,  certain searches on twitter may result in adult content that is not apropiate for people under 18, nor is it appropriate in the office!!!")),
      p("Cheers, Longhow"),
      
      p("Powered by ", a(href="https://www.projectoxford.ai/","Microsoft’s Project Oxford"))
      
    )
  })
  
  
  output$User = renderUI(
  {
    twitteradres = "https://api.twitter.com/1.1/users/lookup.json?screen_name="
    tw.query = input$zoeksleutel2
    tw.query = URLencode(tw.query, reserved=TRUE)
    URI = paste(
      twitteradres,
      tw.query,
      sep=""  
    )
    gebruiker = GET(url = URI, config(token = twitter_token))
    print(gebruiker)
    if (gebruiker$status_code == 200){
      gebruikerinfo = content(gebruiker)[[1]]
      linkje = gebruikerinfo$profile_image_url
      linkje = str_replace(linkje,"_normal","")
      print(linkje)
    
      MicrosoftImageAPI(linkje, input$findsim)
    }
    else{
      p("No user found on twitter matching search critera")
    }
    
    
  })
  
  
  
  output$LinkToImage = renderUI(
    {
    if(input$hyperlink != "")
    {
      MicrosoftImageAPI(input$hyperlink, input$findsim)
    }
    else{
      p("No link given")
    }
    
      
    })
    
  output$Tweets = DT::renderDataTable({
    
    TwitterSetje = datasetTweets()
    datatable(TwitterSetje[TwitterSetje$fotodirect=="Y",c(1,2,3)] ,selection="single",  options = list(pageLength = 10))
    }, server = FALSE
  )
  
 
  output$Beschrijving = renderUI({
    
    s = input$Tweets_rows_selected

    if(!is.null(s)){
      
      TwitterSetje = datasetTweets()
      linkje = TwitterSetje[s,4]
      
      if (linkje !=""){
        
        MicrosoftImageAPI(linkje, input$findsim)
        
      }
    }
  })
})
