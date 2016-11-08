# Tweetface

R code for the tweetface shiny app, the app can be viewed running on [my little shiny server](http://5.100.228.219:3838/sample-apps/TweetFace/).

To get the it running you will need 

* twiter token. 
I have set up the authentication token in a different R session and saved it in a RDS file so that at the beginning of the shiny app I can import it. with twitter_token = readRDS("twittertoken.rds").

* Microsoft keys
Set up in a R file that is sourced in at the beginning of the shiny app

VisionKey = "123456789"
FaceKey = "qwertyuiop"
Emotion = "987654321"

Cheers
Longhow
