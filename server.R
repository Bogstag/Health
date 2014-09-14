library(shiny)
library(rjson)
library(httr)
library(ggplot2)
library(reshape2)

if (!file.exists("data")) { dir.create("data") }

GetBodyMeasures <- function() {
        filename <- "BodyMeasures"
        destfile <- paste("data/", filename, ".rds", sep = "")
        if  ((!file.exists(destfile)) || (file.info(destfile)$mtime + 60*60 <= Sys.time())) {
                withings <- oauth_endpoint("request_token", "authorize", "access_token", base_url = "https://oauth.withings.com/account")
                withingsapp <- oauth_app("withings", key=key)
                withings_token <- oauth1.0_token(withings, withingsapp, cache = TRUE, as_header=FALSE)
                req <- GET("http://wbsapi.withings.net/measure"
                           , query = list(action = "getmeas", userid = withings_token$credentials$userid)
                           , config(token = withings_token))
                stop_for_status(req)
                x <- content(req)
                data <- fromJSON(x)
                df <- CreateBodyMeasuresdf(data)
                saveRDS(df, file=destfile)
        }
        readRDS(destfile) 
}

CreateBodyMeasuresdf <- function(data) {
        masterdf <- data.frame(fulldate = character(0), ymd = character(0), grpid = numeric(0), attrib = character(0), category = character(0), type = character(0), value = numeric(0),  stringsAsFactors = FALSE)
        for (i in 1:length(data$body$measuregrps)) {
                dfull <- as.POSIXct(data$body$measuregrps[[i]]$date, origin="1970-01-01")
                dymd <- as.Date(as.POSIXct(data$body$measuregrps[[i]]$date, origin="1970-01-01"))
                grpid <- data$body$measuregrps[[i]]$grpid
                
                if (data$body$measuregrps[[i]]$attrib == 0) {
                        attrib <- "Device"
                } else if (data$body$measuregrps[[i]]$attrib == 1) {
                        attrib <- "Strange device"
                } else if (data$body$measuregrps[[i]]$attrib == 2) {
                        attrib <- "Manually"
                } else if (data$body$measuregrps[[i]]$attrib == 4) {
                        attrib <- "Strange manually"
                } else {
                        attrib <- data$body$measuregrps[[i]]$attrib
                }
                
                if (data$body$measuregrps[[i]]$category == 1) {
                        category <- "Real"
                } else if (data$body$measuregrps[[i]]$category == 2) {
                        category <- "Objectives"
                } else {
                        category <- data$body$measuregrps[[i]]$category
                }
                
                # category 1 for real measurements, 2 for user objectives.
                for (z in 1:length(data$body$measuregrps[[i]]$measures)) {
                        
                        if (nrow(masterdf) == 0) {radnummer <- 0 } else {radnummer <- nrow(masterdf)+1}
                        
                        if (data$body$measuregrps[[i]]$measures[[z]]$type == 1) {
                                type <- "Weight (kg)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 4) {
                                type <- "Height (m)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 5) {
                                type <- "Fat Free Mass (kg)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 6) {
                                type <- "Fat Ratio (%)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 8) {
                                type <- "Fat Mass Weight (kg)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 9) {
                                type <- "Diastolic Blood Pressure (mmHg)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 10) {
                                type <- "Systolic Blood Pressure (mmHg)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 11) {
                                type <- "Heart Pulse (bpm)"
                        } else if (data$body$measuregrps[[i]]$measures[[z]]$type == 54) {
                                type <- "SP02(%)"
                        } else {
                                type <- data$body$measuregrps[[i]]$measures[[z]]$type
                        }
                        
                        value <-  data$body$measuregrps[[i]]$measures[[z]]$value
                        unit <-  data$body$measuregrps[[i]]$measures[[z]]$unit
                        value <- value * 10^unit
                        
                        newrow <- data.frame(dfull, dymd, grpid, attrib, category, type, value)
                        masterdf <- rbind(masterdf, newrow)
                        
                }
        }
        masterdf
}

# create_weight_df <- function(masterdf) {
#         masterdf <- masterdf[order(masterdf[, "grpid"]),]
#         groupids <- unique(masterdf$grpid)
#         #        weightdf <- data.frame(fulldate = character(0), ymd = character(0), grpid = numeric(0), attrib = character(0), category = character(0), "Weight (kg)" = numeric(0), "Fat Free Mass (kg)" = numeric(0), "Fat Ratio (%)" = numeric(0), "Fat Mass Weight (kg)" = numeric(0),  stringsAsFactors = FALSE)
#         weightdf <- data.frame(fulldate = character(0), wkg = numeric(0), ffm = numeric(0), fr = numeric(0), fmw = numeric(0),  stringsAsFactors = FALSE)
#         for (i in 1:length(groupids)) {
#                 tmp <- subset(masterdf, masterdf$grpid == groupids[i] & type %in% c("Weight (kg)", "Fat Free Mass (kg)", "Fat Ratio (%)", "Fat Mass Weight (kg)"))
#                 if (nrow(tmp) == 4) {
#                         wkg <- tmp[tmp$type == "Weight (kg)",]$value
#                         ffm <- tmp[tmp$type == "Fat Free Mass (kg)",]$value
#                         fr <- tmp[tmp$type == "Fat Ratio (%)",]$value
#                         fmw <- tmp[tmp$type == "Fat Mass Weight (kg)",]$value
#                         fulldate <- tmp$dfull[1]
#                         newrow <- data.frame(fulldate, wkg, ffm, fr, fmw)
#                         weightdf <- rbind(weightdf, newrow)
#                 }
#                 #                newrow <- data.frame(tmp$dfull[1], tmp$dymd[1], tmp$grpid[1], tmp$attrib[1], tmp$category[1], wkg, ffm, fr, fmw)
#         }
#         names(weightdf) <- c("fulldate", "Weight", "Fat Free Mass", "Fat Ratio", "Fat Mass Weight")
#         weightdf
# }

getheightcm <- function () {
        height <- subset(masterdf, masterdf$type == "Height (m)")
        height <- height[order(height[, "dfull"], decreasing = TRUE),]
        height <- height[1,]$value * 100
        height
}

get_ideal_weight <- function(height) {
        idealweight <- 100
        # Ideal body weight by the Hamwi method (men)
        # Allow 48 kg, Add 2.7 kg for every 2.54 cm over 150 cm
        idealweight$hamwi <- (((height - 152.4)/2.54)*2.7) + 48
        
        # Ideal body weight by the Devine formula (men)
        # Allow 50 kg, Add 2.3 kg for every 2.54 cm over 150 cm
        idealweight$devine <- (((height - 152.4)/2.54)*2.3) + 50
        idealweight
}

masterdf <- GetBodyMeasures()
masterdf <- masterdf[order(masterdf[, "type"], masterdf[, "dfull"]),]
height <- getheightcm()
idealweight <- get_ideal_weight(height)
#weightdf <- create_weight_df(masterdf)
# subsetvalue <- sort(unique(masterdf$type))
# subsetname <- subset(masterdf, masterdf$type == subsetvalue[i])
fattype <- c("Fat Free Mass (kg)", "Fat Mass Weight (kg)", "Fat Ratio (%)")


shinyServer(function(input, output) {
        
        output$unreadtext <- renderText({
                paste0("My ideal weight with Hamwi method is ", round(idealweight$hamwi, 1)
                      , "kg and with Devine formula it is ", round(idealweight$devine, 1), "kg.")
        })
        output$weightPlot <- renderPlot({
                ggplot(subset(masterdf, masterdf$type == "Weight (kg)"), aes(x=dfull, y=value)) + 
                labs(list(x = "Date", y = "kg")) + 
                geom_line()
                
        })
        output$fatPlot <- renderPlot({
                ggplot(subset(masterdf, masterdf$type == fattype) , aes(x=dfull, y=value, color=type)) + 
                        labs(list(x = "Date", y = "")) + 
                        geom_line()
                
        })
        
})