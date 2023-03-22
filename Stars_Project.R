################################################################################
#                           Data of the stars
################################################################################
# Packages
# Install packages if they're not already installed
pkg <- installed.packages()[, "Package"]
if(!('Hmisc' %in% pkg)) {install.packages("Hmisc")}
if(!('ggplot2' %in% pkg)) {install.packages("ggplot2")}
if(!('ggrepel' %in% pkg)) {install.packages("ggrepel")}
if(!('rgl' %in% pkg)) {install.packages("rgl")}
if(!('fmsb' %in% pkg)) {install.packages("fmsb")}
if(!('dplyr' %in% pkg)) {install.packages("dplyr")}


# Use of the packages
library("Hmisc")
library("ggplot2")
library("ggrepel")
library("rgl")
library("fmsb")
library("dplyr")

################################################################################
# Import the data
setwd("/home/guest/R/Project_Stars")                                # NEED TO GO
# Read CSV into Data Frame - Dataset 1 (cleaned data without units)
# Na(me), Di(stance), Ma(ss), Ra(dius)
star_NaDiMaRa <- read.csv(file="StarFile1_NameDistanceMassRadius",
                          header = TRUE)

# Read CSV into Data Frame - Dataset 2 (data with units, but with extra columns)
# Na(me), Lu(minosity)
star_NaLu <- read.csv(file="StarFile2_NameDistanceMassRadiusLuminosity",
                      header = TRUE)

# Read CSV into Data Frame - Dataset 3 (data with extra columns)
# Na(me), Ty(pe), Ma(gnetude), Te(mp)
star_NaTyMaTe <- read.csv(file="StarFile3_NameMagTempType",
                          header = TRUE)

# Get information about the data -----------------------------------------------
# Data - star_NaDiMaRa
summary(star_NaDiMaRa)
dim(star_NaDiMaRa)
colnames(star_NaDiMaRa)
str(star_NaDiMaRa)

# Data - star_NaLu
summary(star_NaLu)
dim(star_NaLu)
colnames(star_NaLu)
str(star_NaLu)

# Data - star_NaTyMaTe
summary(star_NaTyMaTe)
dim(star_NaTyMaTe)
colnames(star_NaTyMaTe)
str(star_NaTyMaTe)
#-------------------------------------------------------------------------------

# Rearrange dataframes
# Remove a row out of the dataframe (so the datasets match each other)
star_NaLu <- star_NaLu[-63,]
# And reorganize the "X" colmun
star_NaLu$X <- 0:252
# Retain "X" and Luminosity
star_NaLu <- star_NaLu[,c(1,6)]

# Remove "X" column
star_NaTyMaTe <- star_NaTyMaTe[,c(2:5)]
# Rename the column name star --> Star_name
colnames(star_NaTyMaTe)[1] <- "Star_name"


# Now merge data together
# Merge datasets 1 and 2 together based on similar X
star_merge <- merge(star_NaDiMaRa, star_NaLu,
                    by = "X", all = TRUE)
colnames(star_merge)
# Merge the dataset from above with dataset 3, based on similar Star_name
filesmerged <- merge(star_merge, star_NaTyMaTe,
                     by = "Star_name", all = TRUE)
colnames(filesmerged)

# Hertzsprung-Russell ##########################################################
# Make a copy for this plot
HertzsprungRussell <- filesmerged

# Remove the <NA> of the star type for this plot
HertzsprungRussell <- HertzsprungRussell[complete.cases(HertzsprungRussell$
                                                          Luminosity),]
HertzsprungRussell <- HertzsprungRussell[complete.cases(HertzsprungRussell$
                                                          Distance),]
HertzsprungRussell <- HertzsprungRussell[complete.cases(HertzsprungRussell$
                                                          temp),]

dim(HertzsprungRussell)
# Remove commas
HertzsprungRussell$Luminosity <- gsub(",", "", HertzsprungRussell$Luminosity)

# Set all the used data as.numeric if they need to be numeric
HertzsprungRussell$Distance <- as.numeric(HertzsprungRussell$Distance)
HertzsprungRussell$Luminosity <- as.numeric(HertzsprungRussell$Luminosity)
HertzsprungRussell$Mass <- as.numeric(HertzsprungRussell$Mass)

#PERFORM THIS AGAIN, BUT NEED TO REMOVE THIS AGAIN
HertzsprungRussell <- HertzsprungRussell[complete.cases(HertzsprungRussell$
                                                          Distance),]
HertzsprungRussell$Distance <- as.numeric(HertzsprungRussell$Distance)

####### HertzsprungRussell$Luminosity <- sort(HertzsprungRussell$Luminosity)
head(HertzsprungRussell)
dim(HertzsprungRussell)
# Remove a star that is double in the file
HertzsprungRussell <- HertzsprungRussell[-8,]

colorsHertz = c("red", "orange", "yellow", "white", "skyblue1")

################################################################################
# Hertzsprung-Russell Diagram
HRDiagram <- ggplot(data = HertzsprungRussell,
                    aes(x = HertzsprungRussell$temp,
                        y = HertzsprungRussell$Luminosity,
                        color = HertzsprungRussell$temp,
                        size = HertzsprungRussell$Radius)) +

    ggtitle("Hertzsprung Russell Diagram") +

  geom_point(shape = 19) +
  geom_point(shape = 21, colour = "black") +
  geom_text_repel(aes(label = HertzsprungRussell$Star_name)) +

  theme(plot.title = element_text(size = 20, face="bold", hjust = 0.5),
        panel.background = element_rect(fill = "black"),
        panel.grid.major = element_line(color = "grey", size = 0.3),
        panel.grid.minor = element_line(color = "grey", linetype = "dotted"),
        # scale_x_discrete(position = "top"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.width = unit(9.5, "cm"),                                     # Can be same length as plot?
        plot.margin = unit(c(0.5, 0.5, 2, 0.5), "cm")) +

  xlab("Temperature (K)") +
  ylab("Luminosity (compared to the sun)")

HRDiagram +
  scale_y_log10() +
  scale_x_log10() +
  scale_x_reverse() +
  scale_color_gradientn(colors = c("red", "orange", "yellow",
                                   "white", "skyblue1"),
                        breaks = c(32000, 30000, 25000,
                                   20000, 50000, 10000, 3700),
                        labels = c("O", "B", "A",
                                   "F", "G", "K", "M")) +
  
  guides(size = FALSE,
    color = guide_colorbar(reverse = TRUE))

################################################################################
################################################################################
################################################################################
################################################################################
# plot2
# Make a copy of the dataset
lollipop <- HertzsprungRussell

# Change the distance from lightyears to parsec (r)
lollipop$DistanceLY <- HertzsprungRussell$Distance/3.26

# Then use formula M = m + 5 - 5log10(r)
# Reshape: m = M - 5 + 5log10(r)

# Calculate the relative magnitude for each Star
lollipop$RelativeMag <- lollipop$magnitude -5 + 5*log10(lollipop$DistanceLY)
dim(lollipop)
lollipop <- lollipop[-34,]

# Create data
X = lollipop$Star_name
y = lollipop$RelativeMag
z = lollipop$type

lollipop_df <- data.frame(X, y, z)

# Give each star his color depending on the type
lollipop_df$ColourClass <- ifelse(lollipop_df$z == "M", "red",
                               ifelse(lollipop_df$z == "K", "#FF5D00",
                                      ifelse(lollipop_df$z == "G", "orange",
                                                ifelse(lollipop_df$z == "F", "yellow",
                                                       ifelse(lollipop_df$z == "A", "white", 
                                                              ifelse(lollipop_df$z == "B", "lightblue1",
                                                                     ifelse(lollipop_df$z == "O", "blue",
                                                                            NA)))))))

# Make the plot
ggplot(lollipop_df, aes(x = X, y = y, label = round(y, 2),
                        color = lollipop_df$ColourClass)) +
  geom_segment(aes(x = X, xend = X, y = 1, yend = y), 
               color = lollipop_df$ColourClass, size = 0.5) +
  geom_point(color = lollipop_df$ColourClass, size = 4) +
  geom_text(nudge_y = ifelse(y < 1, -0.2, 0.2),
            colour = "white",
            size = 3.5) +
  theme_dark() +
  coord_flip() +
  
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(color = "white"),
    axis.title = element_text(colour = "white"),
    plot.title = element_text(hjust = 0.5, color = "white", size = 25)) +
  
  xlab("Star names") +
  ylab("The relative magnitude") +
  ggtitle("Lollipop Plot - Relative magnitude") +
  guides(color = guide_legend(title = "ColourClass"))
################################################################################
################################################################################
################################################################################
################################################################################
ggplot(data = lollipop_df, aes(x = X, y = y, color = ColourClass)) +
  geom_segment(aes(x = X, xend = X, y = 1, yend = y), 
               color = lollipop_df$ColourClass, size = 0.5) +
  geom_point(color = lollipop_df$ColourClass, size = 4) +
  theme_dark() +
  coord_flip() +
  ggtitle("Lollipop Plot - Relative magnitude") +
  xlab("Star names") +
  ylab("The relative magnitude") +
  ggtitle("Lollipop Plot - Relative magnitude") +
  guides(color = guide_legend(title = "ColourClass")) +
  scale_color_manual(values = colorlegend,
                     labels = legendtext) +

  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, color = "white", size = 25),
        legend.position = "right",
        legend.background = element_rect(fill = "gray"))
########
colorlegend <- c("red", "#FF5D00", "orange", "yellow", "white", "lightblue1", "blue")
legendtext <- c("M", "K", "G", "F", "A", "B", "O")


mylollipopplot <- ggplot(data = lollipop_df, aes(x = X, y = y, color = ColourClass)) +
  geom_segment(aes(x = X, xend = X, y = 1, yend = y), 
               color = lollipop_df$ColourClass, size = 0.5) +
  geom_point(size = 4) +
  theme_dark() +
  coord_flip() +
  guides(color = guide_legend(title = "Colour Class")) +
  scale_color_manual(values = colorlegend,
                     labels = legendtext) +
  
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(color = "white"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(hjust = 0.5, color = "white", size = 25),
        legend.position = "right")

legend <- cowplot::get_legend(mylollipopplot)

library(grid)
xleft <- 0.70  # x coordinate of left edge of viewport
ybottom <- 0.5  # y coordinate of bottom edge of viewport
width <- 0.4  # width of viewport
height <- 0.3  # height of viewport
vp <- viewport(x = xleft, y = ybottom, width = width, height = height, 
               just = c("left", "bottom"), name = "legend_vp")

# Push the new viewport onto the grid stack and draw the legend within it
pushViewport(vp)
grid.draw(legend)

# Pop the viewport off the grid stack
popViewport()
################################################################################
################################################################################
################################################################################
################################################################################

plotframe3d <- HertzsprungRussell
plotframe3d <- plotframe3d[-c(9, 14),]
plotframe3d$Radius <- as.numeric(plotframe3d$Radius)
plotframe3d <- plotframe3d[order(plotframe3d$Radius), ]

# Write an extra column to the dataframe with the colours of the classes
plotframe3d$ColourClass <- ifelse(plotframe3d$type == "M", "red",
ifelse(plotframe3d$type == "K", "#FF5D00",
       ifelse(plotframe3d$type == "G", "orange",
              ifelse(plotframe3d$type == "F", "yellow",
                     ifelse(plotframe3d$type == "A", "white", 
                            ifelse(plotframe3d$type == "B", "lightblue1",
                                   ifelse(plotframe3d$type == "O", "blue",
                                          NA)))))))

# Set the values in 3d
n = nrow(plotframe3d)
theta <- seq(0, 5*pi, length.out = n)
r <- seq(0, 10, length.out = n)
x <- r * cos(theta)
y <- r * sin(theta)
z <- seq(-10, 10, length.out = n)

# Select 35 points on this spiral
indices <- seq(1, n, length.out = n)
x <- x[indices]
y <- y[indices]
z <- z[indices]

# Create a blank plot with a specified size
open3d(windowRect=c(0, 0, 800, 600))

# Plot the datapoints
plot3d(x, y, z, type="s",
       col = (plotframe3d$ColourClass),
       size = plotframe3d$Radius/12)

# Adjust the plot settings
# Adjust the viewpoint
rgl.viewpoint(theta=180, phi=120, fov=60, zoom=0.6)
bg3d("black")
axes3d(col = "green", width = 2)
grid3d(side = c("x", "y", "z"), col = "green",
       lwd = 0.5, lty = 1)

# Add labels to the datapoints
labels <- plotframe3d$Star_name
text3d(x = x, y = y, z = z,
       texts = labels,
       col="white",
       pos = NULL,
       adj = c(1.5, 1, 25))

# Play the rotation
play3d(spin3d(axis=c(0,0,1), rpm = 3), duration = Inf)
################################################################################
################################################################################
################################################################################
################################################################################
# Featuresplot
Featuresplot <- HertzsprungRussell
Featuresplot$Radius <- as.numeric(Featuresplot$Radius)
Featuresplot$temp <- as.numeric(Featuresplot$temp)

# Make the plot
# Load packages
library(fmsb)
#install.packages("fmsb")
library(dplyr)
#install.packages("dplyr")

# Create sample data
starchart <- Featuresplot[,c(-1,-2)]

# Calculate means for each type
means <- starchart %>% group_by(type) %>% summarize_if(is.numeric,mean)
means$magnitude <- abs(means$magnitude)


means$Distance <- log10(means$Distance)
means$Mass <- log10(means$Mass)
means$Radius <- log10(means$Radius)
means$Luminosity <- log10(means$Luminosity)
means$magnitude <- log10(means$magnitude)
means$temp <- log10(means$temp)


library(fmsb)

# Create data: note in High school for several students
set.seed(99)
data <- means[,-1]
TypesNr <- c(1, 2, 3, 4, 5, 6, 7)
data <- cbind(TypesNr, data)

data <- as.data.frame(data)
colnames(data) <- c("Type", "Distance", "Mass", "Radius", "Luminosity",
                    "Magnitude", "Temperature")
rownames(data) <- means$type

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each variable to show on the plot!
data <- rbind(max(data) , min(data) , data)
# plot with default options:
par(bg = "white")
radarchart(data,
           # Custom the grid
           cglty = 1,                                     # the grid line type
           cglwd = 0.4,                                     # the grid width
           cglcol = "black",                          # the grid color
           title = "Main values of physical properties of Stars",
           vlcex = 1,
           
           # Custom lines of the data
           pcol = rev(c("blue", "lightblue3", "lightgrey", "yellow",
                        "orange", "#FF5D00", "red")),      # line colors data
           pfcol = scales::alpha(c("red", "#FF5D00", "orange", "yellow",# filling colors data
                     "lightgrey", "lightblue3", "blue"),0.10),
           plwd = 0.5)

# Add a legend
legend(x = 1.5, y = 1, legend = means$type, bty = "n", pch=20 ,
       col = c("red", "#FF5D00", "orange", "yellow",
               "lightgrey", "lightblue1", "blue"),
       text.col = "black", cex = 1, pt.cex = 3)



