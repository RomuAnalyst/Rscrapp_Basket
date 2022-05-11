test2 <- test |> filter(team == "Boston Celtics" | team == "Chicago Bulls" | team == "Philadelphia 76ers")




# Density plot of "Sepal.Length"
#::::::::::::::::::::::::::::::::::::::
density.p <- ggdensity(test2, x = "taille", 
                       fill = "team", palette = "jco")
# Draw the summary table of Sepal.Length
#::::::::::::::::::::::::::::::::::::::
# Compute descriptive statistics by groups
stable <- desc_statby(test2, measure.var = "taille",
                      grps = "team")
stable <- stable[, c("team", "length", "min", "max", "mean", "sd")]
# Summary table plot, medium orange theme
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mOrange"))
# Draw text
#::::::::::::::::::::::::::::::::::::::
text <- paste("iris data set gives the measurements in cm",
              "of the variables sepal length and width",
              "and petal length and width, respectively,",
              "for 50 flowers from each of 3 species of iris.",
              "The species are Iris setosa, versicolor, and virginica.", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")
# Arrange the plots on the same page
ggarrange(density.p, stable.p, text.p, 
          ncol = 1, nrow = 3,
          heights = c(1, 0.5, 0.3))


density.p + annotation_custom(ggplotGrob(stable.p),
                              xmin = 199, ymin = 0.06,
                              xmax = 205)




-------------------------------------------------------
  
  
  # Scatter plot colored by groups ("Species")
sp <- ggscatter(test2, x = "taille", y = "poids",
                  color = "team", palette = "jco",
                  size = 3, alpha = 0.6)+ 
  border()                                         
# Marginal density plot of x (top panel) and y (right panel)
xplot <- ggdensity(test2, "taille", fill = "team",
                   palette = "jco")
yplot <- ggdensity(test2, "poids", fill = "team", 
                   palette = "jco")+
  rotate()
# Cleaning the plots
yplot <- yplot + clean_theme() 
xplot <- xplot + clean_theme()
# Arranging the plot
ggarrange(xplot, NULL, sp, yplot, 
          ncol = 2, nrow = 2,  align = "hv", 
          widths = c(2, 1), heights = c(1, 2),
          common.legend = TRUE)

