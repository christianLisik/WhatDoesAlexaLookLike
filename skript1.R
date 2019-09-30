df <- read.csv("./trials.csv")
SECONDS_IN_AN_HOUR <- 60*60
MILLISECONDS_IN_AN_HOUR <- SECONDS_IN_AN_HOUR * 1000
library(dplyr)
#rename A-F to actual agent names:
levels(df$latinSquare) <- c("AlexaF", "SiriF", "CortanaF", "SiriM", "GoogleF", "RobotM")
cat("Original number of entries: ", nrow(df))
#descriptive statistics
uniqueInColumn <- function(column){
  return(length(levels(as.factor(column))))
}


cat("Number of participants: ", uniqueInColumn(df$userUID))
cat("Min year of birth: ", min(df[!duplicated(df$userUID) & df$theAge != "0", "theAge"]))
cat("Max year of birth: ", max(df[!duplicated(df$userUID) & df$theAge != "0", "theAge"]))
cat("Median year of birth: ", median(df[!duplicated(df$userUID) & df$theAge != 0, ]$theAge))
cat("Year of birth SD: ", sd(df[!duplicated(df$userUID) & df$theAge != 0, ]$theAge))
cat("Number of female participants: ", uniqueInColumn(df$userUID[df$theGender == "female"]))
cat("Female median birth year: ", median(df[!duplicated(df$userUID) & df$theAge != 0 & df$theGender == "female", ]$theAge))
cat("Female birth year SD: ", sd(df[!duplicated(df$userUID) & df$theAge != 0 & df$theGender == "female", ]$theAge))
cat("Female Min year of birth: ", min(df[!duplicated(df$userUID) & df$theAge != "0" & df$theGender =="female", "theAge"]))
cat("Female Max year of birth: ", max(df[!duplicated(df$userUID) & df$theAge != "0" & df$theGender =="female", "theAge"]))
cat("Number of male participants: ", uniqueInColumn(df$userUID[df$theGender == "male"]))
cat("Male median birth year: ", median(df[!duplicated(df$userUID) & df$theAge != 0 & df$theGender == "male", ]$theAge))
cat("Male birth year SD: ", sd(df[!duplicated(df$userUID) & df$theAge != 0 & df$theGender == "male", ]$theAge))
cat("male Min year of birth: ", min(df[!duplicated(df$userUID) & df$theAge != "0" & df$theGender =="male", "theAge"]))
cat("male Max year of birth: ", max(df[!duplicated(df$userUID) & df$theAge != "0" & df$theGender =="male", "theAge"]))
cat("Number of third gender participants: ", uniqueInColumn(df$userUID[df$theGender == "other"]))
cat("Other median birth year: ", median(df[!duplicated(df$userUID) & df$theAge != 0 & df$theGender == "other", ]$theAge))
cat("Other birth year SD: ", sd(df[!duplicated(df$userUID) & df$theAge != 0 & df$theGender == "other", ]$theAge))
cat("other Min year of birth: ", min(df[!duplicated(df$userUID) & df$theAge != "0" & df$theGender =="other", "theAge"]))
cat("other Max year of birth: ", max(df[!duplicated(df$userUID) & df$theAge != "0" & df$theGender =="other", "theAge"]))
cat("Number of participants with no gender stated: ", uniqueInColumn(df$userUID[df$theGender == "0"]))
#end descriptive statistics
#drop rows we consider invalid based on Schwind paper's approach (gender and age related differences in designing the characteristics of stereotypical virtual faces)
#remove rows where session time exceeded 6 hours
df<-df[!(df$timeMainSessionTime > 6 * MILLISECONDS_IN_AN_HOUR),]
#remove rows where participant didn't click any sliders
cols = grep(pattern = "Clk$", names(df), value=TRUE)
sums = rowSums(df[cols])
df <- df[sums != 0, ]
remove(cols)
remove(sums)
#remove rows where screen resolution was below 1280p
df<-df[!(df$screenWidth < 1280),]

cat("Number of faces after filtering: ", nrow(df))

#remove columns we don't need
#df <- df %>%
#  select(-matches("Rst|MM|Clk"))
#remove_columns <- c("trialid", "sessions", "userUID", "theAge", "theGender", "theCountry", "hasKnowledgeSmartSpeaker", "playingGames", "watchingMovies", "acceptTOU", "finalQuestion1", "finalQuestion2", "finalQuestion3", "finalQuestion4", "finalQuestion5", "finalQuestion6", "ipadress", "screenWidth", "screenHeight", "systemLanguage", "userLanguage", "cookies", "platform", "appCodeName", "appVersion", "product", "browser", "webgl", "timeDate", "timeWelcome", "timeStart", "timeEnd", "timeMainSessionTime", "timeDuration", "faceGenderClk", "faceStyleClk", "skinDetailsClk", "skinColorClk", "hairColorClk", "eyeColorClk", "eyeShapeClk", "eyeOpeningClk", "eyeSizeClk", "eyeHeightClk", "eyeDistanceClk", "eyeDepthClk", "eyeRotationClk", "eyebrowsColorClk", "eyebrowsShapeClk", "eyebrowsLineClk", "noseShapeClk", "noseLengthClk", "noseWidthClk", "noseBridgeClk", "noseCartilageClk", "foreheadHeightClk", "cheeksBoneClk", "jawShapeClk", "jawChinClk", "jawLengthClk", "earSizeClk", "throatSizeClk", "mouthVolumeClk", "lipRatioClk", "mouthOverlapClk", "mouthWidthClk", "mouthHeightClk", "mouthDepthClk", "eyeShadowClk", "lipStickClk", "rougeClk", "generalMoreBtnClicked", "eyesMoreBtnClicked", "eyebrowsMoreBtnClicked", "noseMoreBtnClicked", "mouthMoreBtnClicked", "makeUpMoreBtnClicked", "outerfaceMoreBtnClicked", "jawMoreBtnClicked", "mainResetClicked", "interfaceResetClicked", "lightsClicked", "todoMoreBtnClicked", "sendClicked"
#)
#df <- df[, !colnames(df) %in% remove_columns, drop = FALSE]
#write.csv(df, "cleaned_trials_for_python.csv")


#logistic regression
df$latinSquare <- as.factor(df$latinSquare)
levels(df$latinSquare) <- c("AlexaF", "SiriF", "CortanaF", "SiriM", "GoogleF", "RobotM")
dforiginal <- df
df$latinSquare <- as.numeric(df$latinSquare) #need to do this cause I can't run a linear model on factors, but does this then still return valid results?

fit <- lm(latinSquare ~ (faceGender + faceStyle + skinDetails + skinColor + hairColor + eyeColor + 
                           eyeShape + eyeOpening + eyeSize + eyeHeight + eyeDistance + eyeDepth + 
                           eyeRotation + eyebrowsColor + eyebrowsShape + eyebrowsLine + noseShape + 
                           noseLength + noseWidth + noseBridge + noseCartilage + foreheadHeight + cheeksBone + 
                           jawShape + jawChin + jawLength + throatSize + earSize + mouthVolume + lipRatio + 
                           mouthOverlap + mouthWidth + mouthHeight + mouthDepth + eyeShadow + lipStick + 
                           rouge), data=df)

anova(fit)
summary(fit)
df <- dforiginal
remove(dforiginal)
#end logistic regression



#one way anova, pairwise ttests
#summary(aov(lipStick ~ latinSquare + Error(userUID), df)) #why are the results different from when I tried with Schwind??? expected p = 0.00151**, actual:0.0186* (0.0226* after filtering invalid data)
#pairwise.t.test(df$lipStick, df$latinSquare , p.adj = "bonf") #why are the results different from when I tried with Schwind??? expected p for lipstick: RobotM x GoogleF = 0.005, actual: 0.12 (same after filter)
#i dont know how to iterate properly here, so I let python create my r source code:

#print("-----------------------------------------------------------")
#cat("test for: ", feature)
#summary(aov(feature ~ latinSquare + Error(userUID), df))
#pairwise.t.test(df$feature, df$latinSquare, p.adj= "bonf")
#print("-----------------------------------------------------------")

 
cat("-----------------------------------------------------------")
cat("Test for: faceGender")
summary(aov(faceGender ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$faceGender, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: faceStyle")
summary(aov(faceStyle ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$faceStyle, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: skinDetails")
summary(aov(skinDetails ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$skinDetails, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: skinColor")
summary(aov(skinColor ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$skinColor, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: hairColor")
summary(aov(hairColor ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$hairColor, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeColor")
summary(aov(eyeColor ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeColor, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeShape")
summary(aov(eyeShape ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeShape, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeOpening")
summary(aov(eyeOpening ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeOpening, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeSize")
summary(aov(eyeSize ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeSize, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeHeight")
summary(aov(eyeHeight ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeHeight, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeDistance")
summary(aov(eyeDistance ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeDistance, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeDepth")
summary(aov(eyeDepth ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeDepth, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeRotation")
summary(aov(eyeRotation ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeRotation, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyebrowsColor")
summary(aov(eyebrowsColor ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyebrowsColor, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyebrowsShape")
summary(aov(eyebrowsShape ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyebrowsShape, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyebrowsLine")
summary(aov(eyebrowsLine ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyebrowsLine, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: noseShape")
summary(aov(noseShape ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$noseShape, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: noseLength")
summary(aov(noseLength ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$noseLength, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: noseWidth")
summary(aov(noseWidth ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$noseWidth, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: noseBridge")
summary(aov(noseBridge ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$noseBridge, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: noseCartilage")
summary(aov(noseCartilage ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$noseCartilage, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: foreheadHeight")
summary(aov(foreheadHeight ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$foreheadHeight, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: cheeksBone")
summary(aov(cheeksBone ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$cheeksBone, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: jawShape")
summary(aov(jawShape ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$jawShape, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: jawChin")
summary(aov(jawChin ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$jawChin, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: jawLength")
summary(aov(jawLength ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$jawLength, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: throatSize")
summary(aov(throatSize ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$throatSize, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: earSize")
summary(aov(earSize ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$earSize, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: mouthVolume")
summary(aov(mouthVolume ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$mouthVolume, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: lipRatio")
summary(aov(lipRatio ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$lipRatio, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: mouthOverlap")
summary(aov(mouthOverlap ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$mouthOverlap, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: mouthWidth")
summary(aov(mouthWidth ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$mouthWidth, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: mouthHeight")
summary(aov(mouthHeight ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$mouthHeight, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: mouthDepth")
summary(aov(mouthDepth ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$mouthDepth, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: eyeShadow")
summary(aov(eyeShadow ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$eyeShadow, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: lipStick")
summary(aov(lipStick ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$lipStick, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")
cat("Test for: rouge")
summary(aov(rouge ~ latinSquare + Error(userUID), df))
pairwise.t.test(df$rouge, df$latinSquare, p.adj= "bonf")
cat("-----------------------------------------------------------")

#end on way anova, pairwise ttests

##THIS MEANS SOMETHING IMPORTANT I THINK BUT I DONT KNOW WHAT:
summary(manova(cbind(faceGender ,  faceStyle ,  skinDetails ,  skinColor ,  hairColor ,  eyeColor ,  
                     eyeShape ,  eyeOpening ,  eyeSize ,  eyeHeight ,  eyeDistance ,  eyeDepth ,  
                     eyeRotation ,  eyebrowsColor ,  eyebrowsShape ,  eyebrowsLine ,  noseShape ,  
                     noseLength ,  noseWidth ,  noseBridge ,  noseCartilage ,  foreheadHeight ,  cheeksBone ,  
                     jawShape ,  jawChin ,  jawLength ,  throatSize ,  earSize ,  mouthVolume ,  lipRatio ,  
                     mouthOverlap ,  mouthWidth ,  mouthHeight ,  mouthDepth ,  eyeShadow ,  lipStick ,  
                     rouge)~latinSquare, df))
#END







AgentNames <- df$latinSquare
fit <- cmdscale(dist(df[, 36:72]), eig=TRUE, k=2) #it's not 37:73 because I don't add an extra country column from the merge with the country list
x <- fit$points[, 1]
y <- fit$points[, 2]

library(ggplot2)
points <- data.frame(fit$points)
shapes <- c(16, 17, 18, 19, 20, 15)
shapes <- shapes[as.numeric(AgentNames)]
colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF")
colors <- colors[as.numeric(AgentNames)]
ggplot(points, aes(x=X1, y=X2, shape=AgentNames, color=AgentNames)) +
  geom_point(size=3) + 
  theme_classic() +
  xlim(-1, 1) +
  ylim(-2, 2) #+ stat_ellipse()

points$agentNames <- AgentNames
summary(manova(cbind(X1, X2)~AgentNames, points))
means <- aggregate(cbind(X1,X2)~AgentNames, points, function(x) c(mean = mean(x), ci = qt(0.95,df=length(x)-1)*sd(x)/sqrt(length(x))))
means <- do.call(data.frame, means)
colnames(means) <- c("AgentNames","X1_mean","X1_ci","X2_mean","X2_ci")

ggplot(means, aes(x=X1_mean, y=X2_mean, color=AgentNames)) + 
  scale_x_continuous(limits = c(-1, 1), breaks = c(1,1)) +
  scale_y_continuous(limits = c(-1, 1),breaks = c(1,1)) +
  geom_point() + 
  geom_rect(aes(x=0,y=0,
                xmin=X1_mean-X1_ci,xmax=X1_mean+X1_ci,
                ymin=X2_mean-X2_ci,ymax=X2_mean+X2_ci), alpha=0.1)+
  geom_point(data=points, aes(x=X1, y=X2, color=AgentNames),alpha=.1)+
  theme_classic()# + stat_ellipse()











#Schwind's original code. note that this drops 4 entries with country = NA, leading to vastly different outcomes
#theData <- df
#
#nrow(theData)
#theCountry  <- 0:255
#country <- c("Afghanistan", "Akrotiri", "Albania", "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua and Barbuda", "Argentina", "Armenia", "Aruba", "Ashmore and Cartier I.", "Australia", "Austria", "Azerbaijan", "Bahamas, The", "Bahrain", "Bangladesh", "Barbados", "Bassas da India", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", "British Indian", "British Virgin I.", "Brunei", "Bulgaria", "Burkina Faso", "Burma", "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde", "Cayman I.", "Central African Rep.", "Chad", "Chile", "China", "Christmas Island", "Clipperton Island", "Cocos (Keeling) I.", "Colombia", "Comoros", "Congo", "Cook I.", "Coral Sea I.", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Rep.", "Denmark", "Dhekelia", "Djibouti", "Dominica", "Dominican Rep.", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Europa Island", "Falkland I.", "Faroe I.", "Fiji", "Finland", "France", "French Guiana", "French Polynesia", "French Southern", "Gabon", "Gambia, The", "Gaza Strip", "Georgia", "Germany", "Ghana", "Gibraltar", "Glorioso I.", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard Island", "Holy See (Vatican City)", "Honduras", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Isle of Man", "Israel", "Italy", "Jamaica", "Jan Mayen", "Japan", "Jersey", "Jordan", "Juan de Nova I.", "Kazakhstan", "Kenya", "Kiribati", "Korea, North", "Korea, South", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall I.", "Martinique", "Mauritania", "Mauritius", "Mayotte", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montserrat", "Morocco", "Mozambique", "Namibia", "Nauru", "Navassa Island", "Nepal", "Netherlands", "Netherlands Antilles", "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfolk Island", "Northern Mariana I.", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea", "Paracel I.", "Paraguay", "Peru", "Philippines", "Pitcairn I.", "Poland", "Portugal", "Puerto Rico", "Qatar", "Reunion", "Romania", "Russia", "Rwanda", "Saint Helena", "Saint Kitts and Nevis", "Saint Lucia", "Saint Pierre", "Saint Vincent", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia and Montenegro", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon I.", "Somalia", "South Africa", "South Georgia", "Spain", "Spratly I.", "Sri Lanka", "Sudan", "Suriname", "Svalbard", "Swaziland", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tromelin Island", "Tunisia", "Turkey", "Turkmenistan", "Turks and Caicos I.", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "United States of America", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", "Virgin I.", "Wake Island", "Wallis and Futuna", "West Bank", "Western Sahara", "Yemen", "Zambia", "Zimbabwe")
#countryMap <- data.frame(country, theCountry)
#theData <- merge(countryMap, theData, by = "theCountry")
#
#theData$latinSquare <- as.factor(theData$latinSquare)
##droplevels(theData$theGender)
##droplevels(theData$latinSquare)
#
#levels(theData$latinSquare) <- c("AlexaF", "SiriF", "CortanaF", "SiriM", "GoogleF", "RobotM")
#droplevels(theData$latinSquare)
#levels(theData$latinSquare)
#
#df <- theData
#AgentNames <- df$latinSquare
#df <- df %>%
#  select(-matches("latin"))
#
#nrow(theData)
#d <- dist(df)
#fit <- cmdscale(dist(theData[,37:73]), eig=TRUE, k=2) # k is the number of dim
#x <- fit$points[, 1]
#y <- fit$points[, 2] 
##plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
##     main="Metric MDS", type="n")
##text(x, y, labels = AgentNames, cex=.7)
#
#library(ggplot2)
#points <- data.frame(fit$points)
#shapes <- c(16, 17, 18, 19, 20, 15)
#shapes <- shapes[as.numeric(AgentNames)]
#colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF")
#colors <- colors[as.numeric(AgentNames)]
#ggplot(points, aes(x=X1, y=X2, shape=AgentNames, color=AgentNames)) + geom_point(size=3) + theme_classic()# + stat_ellipse()
#
#points$agentNames <- AgentNames
#summary(manova(cbind(X1,X2)~AgentNames, points ))
#
#means <- aggregate(cbind(X1,X2)~AgentNames, points, function(x) c(mean = mean(x), ci = qt(0.95,df=length(x)-1)*sd(x)/sqrt(length(x))))
#means <- do.call(data.frame, means)
#colnames(means) <- c("AgentNames","X1_mean","X1_ci","X2_mean","X2_ci")
#
#ggplot(means, aes(x=X1_mean, y=X2_mean, color=AgentNames)) + 
#  scale_x_continuous(limits = c(-1, 1), breaks = c(1,1)) +
#  scale_y_continuous(limits = c(-1, 1),breaks = c(1,1)) +
#  geom_point() + 
#  geom_rect(aes(x=0,y=0,
#                xmin=X1_mean-X1_ci,xmax=X1_mean+X1_ci,
#                ymin=X2_mean-X2_ci,ymax=X2_mean+X2_ci), alpha=0.1)+
#  geom_point(data=points, aes(x=X1, y=X2, color=AgentNames),alpha=.1)+
#  theme_classic()# + stat_ellipse()#












####average faces
AlexaF <- data.frame(colMeans(x=df[df$latinSquare=="AlexaF", 36:72], na.rm=TRUE))
colnames(AlexaF) <- "AlexaF"
SiriF <- data.frame(colMeans(x=df[df$latinSquare=="SiriF", 36:72], na.rm=TRUE))
colnames(SiriF) <- "SiriF"
CortanaF <- data.frame(colMeans(x=df[df$latinSquare=="CortanaF", 36:72], na.rm=TRUE))
colnames(CortanaF) <- "CortanaF"
SiriM <- data.frame(colMeans(x=df[df$latinSquare=="SiriM", 36:72], na.rm=TRUE))
colnames(SiriM) <- "SiriM"
GoogleF <- data.frame(colMeans(x=df[df$latinSquare=="GoogleF", 36:72], na.rm=TRUE))
colnames(GoogleF) <- "GoogleF"
RobotM <- data.frame(colMeans(x=df[df$latinSquare=="RobotM", 36:72], na.rm=TRUE))
colnames(RobotM) <- "RobotM"

averageFaces <- AlexaF
averageFaces$SiriF <- SiriF$SiriF
averageFaces$CortanaF <- CortanaF$CortanaF
averageFaces$SiriM <- SiriM$SiriM
averageFaces$GoogleF <- GoogleF$GoogleF
averageFaces$RobotM <- RobotM$RobotM
write.csv(averageFaces, "averageFaces.csv")
####average faces stop