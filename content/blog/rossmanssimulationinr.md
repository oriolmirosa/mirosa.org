+++
Categories = ["Diffusion", "R", "Simulations"]
date = "2015-01-27T10:15:06-06:00"
nopaging = true
noread = true
notoc = true
title = "Rossman's Simulation in R"
draft = false
blog = ["post"]

+++
I really enjoyed reading [Gabriel Rossman](http://www.sociology.ucla.edu/faculty/gabriel-rossman)'s recent article ["The Diffusion of the Legitimate and the Diffusion of Legitimacy"](http://www.sociologicalscience.com/the-diffusion-of-the-legitimate/) in [Sociological Science](http://www.sociologicalscience.com/). It is a great example of the use of simulations in social theory. In the article, Rossman argues that the way in which an innovation will diffuse depends on its categorical legitimacy. That is, if an innovation takes place in a new category which is not legitimate yet, people will not just adopt it once they become aware of it. They require many more people to have adopted it first in order to consider doing it themselves. In contrast, if an innovation takes place in a category that is highly legitimate, just becoming aware of it will trigger adoption.

The implication is that the patterns of adoption will differ depending on the categorical legitimacy of the innovation. If categorical legitimacy is low, we would expect to see the typical s-shaped pattern of adoption, in which adoptions do not 'take off' until quite a few people have adopted it already. When categorical legitimacy is high, we would expect to see a high level of initial adoption that then tapers off, producing a pattern in the shape of a concave curve.

Rossman then runs a simulation in order to prove his point. In the supplemental materials to the article, Rossman provides the [Stata code for the simulation](http://www.sociologicalscience.com/download/volume%201/march%282%29/supplementalmaterials/supplementalthediffusionofthelegitimateandthediffusionoflegitimacy.pdf). Since I don't use Stata, I decided to try to reproduce the simulation in R.

Now, I am very new both to simulations and to R, and so this is probably not the most efficient way of doing this. I am also not very familiar with Stata, so I might not have properly understood what Rossman's code was doing in the first place. In any case, feel free to let me know if you have any suggestions for improvement.

Besides the simulation, I have also tried to reproduce the two graphs with which Rossman presents his results. The first of the two, which is a 3D graph, I have done using R's basic ``persp()`` command, and therefore it is only an approximation (there are, of course, other options; you can find a good review [here](http://blog.revolutionanalytics.com/2014/02/3d-plots-in-r.html)). I have not even tried to modify it in any way. Using the ``scatterplot3D`` package we could get something that looks more similar to what's in the article in terms of perspective, but there are other problems because this package is really not meant to represent surfaces. 

Now, for no real good reason beyond the challenge, I tried to see if I could get the second graph to look exactly as it is in the article using ``ggplot2``. Of course, it is possible to get a similar graph with much less code (the simplest version can be found in the code below, although I do not provide the graph here). Anyways, here you have the two graphs, followed by the code for the simulation and the graphs.

<div style="height: 450px" align="center">
     <img src="/img/3Dplot.png" alt="3D Plot" style="max-height: 100%" />
</div>

<div style="height: 450px" align="center">
     <img src="/img/2Dplot.png" alt="3D Plot" style="max-height: 100%" />
</div>

	# Set working directory

	setwd("~/Documents/Research/R/Diffusion1")

	# Load the necessary libraries

	library(ggplot2)    
	library(scales)
	
	# Define the function that will do the simulation

	simdiffuse <- function(a, b, c, d) {
  
		# Define the parameters of the simulation
  	
		endo <- 1/a        # innovation endogenous effect
		endomacro <- 1/b   # category endogenous effect
		appeal <- c        # innovation's ex ante appeal
		ninnov <- d        # number of innovations in category

		# Prepare the data frame where we will enter the results (i.e., the 'adopt' value)

		results <- data.frame(catdensity = rep(0:ninnov, each = 25), t = 1:25, endo = endo, endomacro = endomacro, appeal = appeal, adopt = NA)    

		# Prepare the data frame with the 1000 individuals among whom the innovation will diffuse, including their adoption propensity (which follows a standardized normal)

		prop <- rnorm(1000)
		diff <- data.frame(prop)
		diff$adopt <- 0
		diff$adopt[1:5] <- 1

		# Do the iterations for the simulation (1525 iterations: 25 time periods x 61 categorical densities), and put the result of the mean number of adoptions for each combination of time period and categorical density in results$adopt

		for (catdensity in 0:ninnov) {
		  diff$adopt <- 0
		  diff$adopt[1:5] <- 1
  
		  for (t in 1:25) {
		    results[results$catdensity == catdensity & results$t == t,]$adopt <- mean(diff$adopt)
		    for (obs in 1:nrow(diff)) {
				if(appeal+(mean(diff$adopt)*endo)+(catdensity*endomacro) > rnorm(1, diff[obs,]$prop)) diff[obs,]$adopt <- 1
    		}
	 	 }
		}
	return(results)
	}

	# Now we call the function we just created with the parameters used by Grossman

	results <- simdiffuse(.2, 20, -3, 60)

	# Graph 1: 3D plot

	# Put the data in the necessary format for persp(). If we were using scatterplot3D, for instance, the data for x, y, and z could come directly from the 'results' data frame

	x <- c(1:25)
	y <- c(0:ninnov)
	z <- matrix(results$adopt, nrow=length(x), ncol=length(y))

	# Determine the format of the output of the graph (large and high res so that it looks pretty, but it also means a pretty large file, ~800kb)

	png("3Dplot.png", units="in", width=11, height=7.5, res=300) 

	# Create the graph

	persp(x, y, z, theta = 30, phi = 20, expand = 0.5, cex.lab = 0.7, xlab = "Time", ylab = "Category Density", zlab = "Saturation at Innovation Level")

	dev.off()

	# Graph 2: 2D plot

	# This would be the most bare-bones version (not shown above)

	ggplot(data=subset(results, results$catdensity == 0 | results$catdensity == 20 | results$catdensity == 40 | results$catdensity == 60), aes(x=t, y=adopt, linetype=factor(catdensity))) + geom_line()

	# Determine the format of the output of the graph

	png("2Dplot.png", units="in", width=11, height=7.5, res=300)

	# Create the graph with all the options that format it as in Rossman's article

	ggplot() +	
	 geom_line(data=subset(results, results$catdensity == 0), aes(x=t, y=adopt, linetype="Density==0"), size = 1.3) +
	 geom_line(data=subset(results, results$catdensity == 20), aes(x=t, y=adopt, linetype="Density==20"), size = 1.3) +
	 geom_line(data=subset(results, results$catdensity == 40), aes(x=t, y=adopt, linetype="Density==40"), size = 1.3) +
	 geom_line(data=subset(results, results$catdensity == 60), aes(x=t, y=adopt, linetype="Density==60"), size = 1.3) +
	 scale_linetype_manual("Category Density", values = c("Density==0" = 3, "Density==20" = 2, "Density==40" = 5, "Density==60" = 1)) +
	 guides(linetype=guide_legend(keywidth = 3, ncol=2, title.position="top", byrow=TRUE, title.hjust = 0.5)) +
	 theme(legend.position = "bottom",
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = .5),
        axis.text = element_text(colour = "black"),
        text = element_text(family="Times"),
        legend.background = element_rect(color="black", size=0.3),
        axis.text.y = element_text(angle=90, hjust=0.5)) +
     labs(x = "Time", y = "Saturation at Innovation Level") +
     scale_y_continuous(breaks=seq(0, 1, .2), labels=c("0", ".2", ".4", ".6", ".8", "1"))

	dev.off()
