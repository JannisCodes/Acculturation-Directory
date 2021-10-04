unlink('rsconnect', recursive=TRUE)
library(rsconnect)
rsconnect::setAccountInfo(name='acculturation-review', 
                          token='D68F9BC7ECB061AC7338C894778F0099', 
                          secret='b64FgrXVQnbqazL5e7d0zaJI9Jqn9W0wgilMtYbS')

# deploy application
rsconnect::deployApp(account = "acculturation-review",
                     appFiles = c("app.R", 
                                  "scales-data.RData",
                                  "favicon.ico",
                                  "www"),
                     appName = "scale-directory")
