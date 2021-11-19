# acculturation-scales

This GitHub repository collects the code for the acculturation scale directory. 
A life version of the shiny dashboard is available here: https://acculturation-review.shinyapps.io/scale-directory/

The data files are currently not available in the GitHub repository but will be made public as part of our open data 
respositry for the associated academic publication.

## Key Files

The creation of this application has been approached in three main steps:
- Data preparation: 
  - the '*dataPrep.R*' and the '*scale-references.Rmd*' files prepare all necessary files for the dashboard
  - the '*www*' folder includes additional icon, and css files as well as image files used in the application
- Application: the main application code is available in the '*app.R*' file
- Deployment: a general example of how to publish the file to a (free) shinyapp.io account is available in the '*ShinyAppPublishExample.R*' file