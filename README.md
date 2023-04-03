# Acculturation Scales Directory [R Shiny Dashboard Application]
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Version](https://badge.fury.io/gh/tterb%2FHyde.svg)](https://badge.fury.io/gh/tterb%2FHyde)
[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://matt.dray.shinyapps.io/randoflag/)


This GitHub repository collects the code for the acculturation scale directory. 
A live version of the shiny dashboard is available here: https://acculturation-review.shinyapps.io/acculturation-directory/

The data files are currently not available in the GitHub repository but will be made public as part of our open data 
respositry for the associated academic publication.

## Key Files

The creation of this application has been approached in three main steps:
- Data preparation: 
  - the '*dataPrep.R*' and the '*scale-references.Rmd*' files prepare all necessary files for the dashboard
  - the '*www*' folder includes additional icon, and css files as well as image files used in the application
- Application: the main application code is available in the '*app.R*' file
- Deployment: a general example of how to publish the file to a (free) shinyapp.io account is available in the '*ShinyAppPublishExample.R*' file

## License
This data repository is licensed under the **CreativeCommons Attribution 4.0 International** License (CC BY 4.0). For more information see our '_LICENSE.txt_' file, the legal code on the [CreativeCommons website](https://creativecommons.org/licenses/by/4.0/legalcode), or for a pragmatic overview see the [CreativeCommons deed highlights](https://creativecommons.org/licenses/by/4.0/).
