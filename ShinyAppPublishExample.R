unlink('rsconnect', recursive=TRUE)
library(rsconnect)
rsconnect::setAccountInfo(name='acculturation-review', # name of your shinyapp.io account 
                          token='YOUR SHINYAPPS.IO ACCOUNT TOKEN', 
                          secret='YOUR SHINYAPPS.IO ACCOUNT SECRET')

# deploy application
rsconnect::deployApp(account = "acculturation-review",
                     # Select which files should be up uploaded
                     appFiles = c("app.R", 
                                  "scales-data.RData",
                                  "favicon.ico",
                                  "www"),
                     appName = "scale-directory")
