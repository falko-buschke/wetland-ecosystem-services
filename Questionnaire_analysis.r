#Begin by changing the working directory
setwd("ADD OWN WORKING DIRECTORY HERE")   # Set working directory


# Install and load all the required packages

install.packages("psych")
library(psych)		# For tesing multidimensionality of variables
install.packages("granovaGG")
library(granovaGG)	# To visualise the dependent sample assessment plots (Figure 2)
install.packages("PMCMR")
library(PMCMR)		# For the non-parametric pair-wise comaprisons of ecosystem services (Figure 3)
install.packages("gridExtra")
library(gridExtra)	# To allow two plots to be group alongside each other (e.g. Figure 2)


# Open the survey data and attach the data.frame  to make data handling simpler 
ES <-  read.table("Survey_data.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)

# Flip the values for question 11, which asked about an ecosystem DISservice.
ES$Q11 <- 3-ES$Q11
attach(ES)

#########################################################


#############################################
#											#
#   Test responses for unidimensionality	#
#											#
#############################################		

# For odd-numbered question (Ecosystem services benefits)
# NOTE: questions along different scales have already been removed
omB <- omega(subset(ES, select=c(Q1,Q3,Q11,Q13,Q15)),poly=T,plot=F)
omB$omega_h		# Hierarchical omega
omB$ECV			# Expalined common variance
omB$alpha		# Cronbach's alpha


# For even numbered questions (damage-causing activities)
# NOTE: questions along different scales have already been removed
omD <- omega(subset(ES, select=c(Q4,Q6,Q10,Q12,Q14)),poly=T,plot=F)
omD$omega_h		# Hierarchical omega
omD$ECV			# Expalined common variance
omD$alpha		# Cronbach's alpha

#########################################################


#################################################################
#																#
# 	Comparing self-reported and community perceived responses	#
#																#
#################################################################

# (1) Benefits from wetlands
benefit.self <- apply(cbind(Q1,Q15,Q11),1,mean)		# self-reported benefits
benefit.group <- apply(cbind(Q3,Q13),1,mean)	 	# community-perceived benefit
wilcox.test(benefit.self, benefit.group, paired = TRUE, alternative = "less")

################################################

# (2) Damage-causing activities
damage.self <- apply(cbind(Q4,Q6,Q14),1,mean)  	# self-reported damage
damage.group <- apply(cbind(Q10,Q12),1,mean) 	# community-perceived damage
wilcox.test(damage.self, damage.group, paired = TRUE, alternative = "less")

#############################
#							#
# 	Figure 2 in manuscript	#
#							#
#############################

png(filename="Figure 2.png",width=12,height=12,units="cm",res=600)

plot1 <- granovagg.ds(cbind(benefit.group,benefit.self),main="",xlab="Self-reported benefit",ylab="Perceived group benefit") + theme(legend.position="bottom",legend.direction="horizontal")
plot2 <- granovagg.ds(cbind(damage.group,damage.self),main="",xlab="Self-reported damage",ylab="Perceived group damage") + theme(legend.position="bottom",legend.direction="horizontal")
grid.arrange(plot1, plot2, ncol=2)

dev.off()  # Figure should be saved in working directory


#########################################################


#############################################
#											#
# 	Comparing types of ecosyetm services	#
#											#
#############################################

# Assign resonses to ecosystem service category
# (Note: self-reported and community-expected are both included because they are strongly related)

provision <- apply(cbind(Q1,Q3,Q7,Q15),1,mean)	# provisioning ecosystem services
regulate <- apply(cbind(Q5,Q11),1,mean)			# regulating ecosystem services
culture <- apply(cbind(Q9,Q13),1,mean)			# cultural ecosystem services


# Carry of Friedman test
friedman.test(cbind(provision,regulate,culture))

# Carry out two types of post-hoc multiple comparison tests (both give the same results)
posthoc.friedman.conover.test(DV, p.adjust.method="bonferroni")	# Conover's test
posthoc.friedman.nemenyi.test(DV)								# Nemenyi's test


#############################
#							#
# 	Figure 3 in manuscript	#
#							#
#############################

# First,create a function to calculate the standard error
se <- function(x){sd(x)/sqrt(length(x))}

# Calculate the mean values and standard error for each ecosystem service type
means <- c(mean(provision), mean(regulate), mean(culture))
ses <- c(se(provision), se(regulate), se(culture))

# Create a dummy vector for the x-axis
xax <- c(1,2,3)


# Make plot for Figure 2
png(filename="Figure 3.png",width=12,height=12,units="cm",res=600)

par(mai=c(0.75,0.8,0.1,0.1))
plot(means~xax,pch=15,cex=2,ylab="Benefits from wetlands", ylim=c(1,1.8),xlim=c(0.4,3.6),xaxt = "n", xlab="",las=2)
axis(1,at=c(1,2,3),label=c("","",""))
mtext(side=1, at=c(1,2,3),line = c(2,2,2), text=c("Provisiong\nservices", "Regulating\nservices","Cultural\nservices"))
arrows(xax,means,xax,means+ses,angle=90)#,col="red")
arrows(xax,means,xax,means-ses,angle=90)#,col="red")

# Add annotations of significant groups from post-hoc test
text(c(1,2,3),c(1.4,1.65,1.33),c("a","b","a"))

dev.off()  # Figure should be saved in working directory


#################################################################


#####################################################
#													#
# 	Modeling the benefits obtained from wetlands	#
#													#
#####################################################


# Create new variables to denote the average damage and benefit from wetlands (note: only include unidimensional questions)
# Define demographic variables as either categorical or continuous variable

damage <- apply(cbind(Q4,Q6,Q10,Q12,Q14),1,mean)
benefit <- apply(cbind(Q1,Q3,Q11,Q13,Q15),1,mean)
education <- as.factor(ES$Education)
gender <- as.factor(ES$Gender)
employment <- as.factor(ES$Work)
role <- as.factor(ES$Position)
age <- ES$Age
residence <- ES$Residence

# Define the multiple linear regression
es.lm <- lm(benefit ~ damage + age + education + gender + employment + role + residence, na.action = "na.fail")

# This command reports the coefficents used in Table 2 
summary(es.lm)
# This command is for the F-test assocaited with anova to test the significance of variable (Table 2)
anova(es.lm)

#################################################################
