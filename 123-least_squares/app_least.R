#least_squares_animation

#packages:
#install.packages('Rgb')
library('Rgb')
d <- data.frame( x = seq(11)-1,  y= c(1,3,4,3, 5, 6, 8, 8, 9, 8, 7))
colnames(d) = c('beers', 'smiles')
#define the names of the IV and the DV::
IV = colnames(d)[1]
DV = colnames(d)[2]
#define the names of the IV and the DV::
IV = colnames(d)[1]
DV = colnames(d)[2]
#variables:
point_color = rgb(0.2,0,1, 1)
line_color = rgb(1,0.2,0.2, 0.6)
line_width = 3
x1 = 5
y1 = 12
x2 = 2
y2 = 15
x3 = 7
y3 = 15
text_size = 1.6
value = 0
value_adjust = 10 
y_lim = c(-5, 15)
#parameters
par(mfrow = c(1,1), mar = c(1,1,1,1), oma = c(1,10,1,1)) #note: bottom, left, top and right . notes oma = outer margin region, mar = margins for ech specific plot
#simple dot plot:
plot(d, ylim = y_lim, type = 'n', 
     xlab = '#beers', 
     ylab = '#smiles')
#the regression line:
abline(lsfit(d[IV], d[DV]), 
       col = line_color, 
       lwd = line_width) #look inside the function + see what the code does
#plot a vertical line from each point up or down: this should connect to the glm line.
intercept = lsfit(d[IV], d[DV])[[1]][1]
B = lsfit(d[IV], d[DV])[[1]][2]
for (i in seq(1:length(d[,1])+1)){
  #value = intercept+(B*(i-1))
  segments(d[IV][i,], d[DV][i,], d[IV][i,],value/value_adjust, 
           col = line_color, 
           lwd = line_width)
}
#play with the intercept and B
#value = 1 #this has to be the value that goes into the slider
value_intercept = 0
new_intercept = (lsfit(d[IV], d[DV])[[1]][1]) + value_intercept
new_B = (lsfit(d[IV], d[DV])[[1]][2]) + value/value_adjust
old_midpoint = intercept+((length(d[DV][,1])/2)*B)
new_midpoint = new_intercept+((length(d[DV][,1])/2)*new_B)
new_intercept = intercept + (old_midpoint - new_midpoint)

#new regression line + segments to plot:
abline(new_intercept, new_B, 
       col = line_color,
       lwd = line_width)

value = 2
new_B = ((lsfit(d[IV], d[DV])[[1]][2]) + value/10)
new_B

total_diff_score = c()
for (i in seq(1:length(d[,1])+1)){
  value = new_intercept+(new_B*(i-1))
  segments(d[IV][i,], d[DV][i,], d[IV][i,],value, 
           col = line_color, 
           lwd = line_width)
  difference= abs((d[DV][i,]) - (value))
  total_diff_score = c(total_diff_score, difference)
  print ('----------------------')
  print (i)
  print (difference)
  print(total_diff_score)
}
total_diff_score_complete = sum(total_diff_score)
round(total_diff_score_complete,0)

#add the points, do this last to overlap the endpoints of the lines:
points(d, cex = 2, col = point_color, pch = 16)
#add labeling::
text('Y_hat = a + B(xy) * x(i)',
     x = x1, y = y1, 
     xpd = NA,
     cex = text_size)


#code above here is the same as the plot below, just here to test changes to the plot outside of the app
#code above here is the same as the plot below, just here to test changes to the plot outside of the app
#################shiny ap here:
ui <- fluidPage(
  # App title ----
  titlePanel("Illustration of least squares method"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "B",
                  label = "change the slope of the regression line, each step = 0.1",
                  min = -100,
                  max = 100,
                  value = 0)
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      verbatimTextOutput("summary")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    value = input$B
    #parameters
    par(mfrow = c(1,1), mar = c(1,1,1,1), oma = c(6,1,1,1)) #note: bottom, left, top and right . notes oma = outer margin region, mar = margins for ech specific plot
    #simple dot plot:
    plot(d, type = 'n', 
         xlab = '#beers', 
         ylab = '#smiles', 
         ylim = y_lim,
         xpd = NA)
    
    #variables:
    intercept = lsfit(d[IV], d[DV])[[1]][1]
    B = lsfit(d[IV], d[DV])[[1]][2]
    value_intercept = 0 #for now zero, add second slider to change the intercept
    new_intercept = (lsfit(d[IV], d[DV])[[1]][1]) + value_intercept
    new_B = (lsfit(d[IV], d[DV])[[1]][2]) + value/value_adjust
    old_midpoint = intercept+((length(d[DV][,1])/2)*B)
    new_midpoint = new_intercept+((length(d[DV][,1])/2)*new_B)
    new_intercept = intercept + (old_midpoint - new_midpoint)
    
    #regression line:
    abline(new_intercept, new_B, 
           col = line_color,
           lwd = line_width)
    
    #draw the lines from points to the regression line and calculate the summ of differences:
    total_diff_score = c()
    for (i in seq(1:length(d[,1])+1)){
      value = new_intercept+(new_B*(i-1))
      segments(d[IV][i,], d[DV][i,], d[IV][i,],value, #x,y starting point, x,y end point
               col = line_color, 
               lwd = line_width)
      difference= abs((d[DV][i,]) - (value))
      total_diff_score = c(total_diff_score, difference)
    }
    total_diff_score_complete = sum(total_diff_score)
    minimal_diff_score = sum(abs(lsfit(d[IV], d[DV])$residuals))
    
    #add the points, do this last to overlap the endpoints of the lines:
    points(d, cex = 2, col = point_color, 
           pch = 16, 
           ylim = y_lim
    )
    #add labeling::
    text('Y_hat = a + B(xy) * x(i)',
         x = x1, y = y1, 
         xpd = NA,
         cex = text_size)
    
    text(paste('minimal summ:', round(minimal_diff_score,0)),
         x = x2, y = y2, 
         xpd = NA,
         cex = text_size)
    #print the current sum of the residuals to the screen
    text(paste('current summ:', round(total_diff_score_complete,0)),
         x = x3, y = y3, 
         xpd = NA,
         cex = text_size)
    
  })
  output$summary <- renderPrint({
    'the starting point of the slider (zero) is the calculated B value that minimized the total sum of all the vertical line segments'
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)