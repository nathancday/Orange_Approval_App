
library(rsconnect)
setwd("~/future/President_Approval/Pres_Approval_App/")
rsconnect::deployApp(appName = "Presidential_Approval")
print("Y")
