#Begin by changing the working directory
setwd("ADD OWN DIRECTORY HERE")   # Set working directory


# Install and load all the required packages

install.packages("psych")
library(psych)		# for tesing multidimensionality of variables
install.packages("nlme")
library(nlme)		# for the repeate measures ANOVA comparing ecosystem serice types (Figure 2)
install.packages("visreg")
library(visreg)		# To visualise the predictions from a linear regression model (Figure 3)

# Open the survey data and attach the data.frame  to make data handling simpler 
ES <-  read.table("Survey_data.txt",header=TRUE, sep="\t", na.strings="NA", dec=".", strip.white=TRUE)
attach(ES)


# Flip the values for question 11, which asked about an ecosystem DISservice.
Q11 <- 3-Q11

##########################################################################################


#################################
#								#
# 	Testing multidimensionality	#
#								#
#################################

wetland.benefits <- cbind(Q1,Q3,Q5,Q7,Q9,Q11,Q13,Q15)
omega (wetland.benefits)

damaging.activities <- cbind(Q2,Q4,Q6,Q8,Q10,Q12,Q14,Q16)
omega (damaging.activities)



#############################
#							#
# 	Table 2 in manuscript	#
#							#
#############################

# (1) Benefits from wetlands

benefit.self <- apply(cbind(Q1,Q9,Q15,Q5,Q11),1,mean)	# self-reported benefits
benefit.group <- apply(cbind(Q3,Q7,Q13,Q5, Q11),1,mean) # community-expected benefit

# Linear model comparing self-reported to community-expected benefit
summary(lm(benefit.self ~ benefit.group+0))

################################################

# (2) Damage-causing activities

damage.self <- apply(cbind(Q4,Q6,Q14,Q16),1,mean)  #self-reported damage
damage.group <- apply(cbind(Q2,Q8,Q10,Q12),1,mean) # community-expected damage

# Linear model comparing self-reported to community-expected damage
summary(lm(damage.self ~ damage.group+0))
                   
################################################


#############################
#							#
# 	Figure 2 in manuscript	#
#							#
#############################

# Assign resonses to ecosystem service category
# (Note: self-reported and community-expected are both included because they are strongly related)

provision <- apply(cbind(Q1,Q3,Q7,Q15),1,mean)	# provisioning ecosystem services
regulate <- apply(cbind(Q5,Q11),1,mean)			# regulating ecosystem services
culture <- apply(cbind(Q9,Q13),1,mean)			# cultural ecosystem services

# Create a vector called 'treatment' that defines the ecosystem service category
treatment <- c(rep("Provisioning",50),rep("Regulating",50),rep("Cultural",50))

# Combine the ecosystem services data into a single dependent variable (DV) vector
DV <- c(provision,regulate,culture)

# Repeated-measures ANOVA (to maintain data across respondents)

# First, create a dummy vector (to serve as a random factor) for the 50 respondents
id <- rep(1:50,3)

# Define model and perfomr anova
lme.rank <- lme(fixed = DV ~ treatment, random =~1|id)
anova(lme.rank)

# Carry out the post-hoc pair-wise t-test with Bonferroni correction
pairwise.t.test(x=DV, g=treatment, p.adj="bonf",paired=T)

####################################################################

# Make plot of data (Figure 2 in manuscript) 

# First,create a function to calculate the standard error
se <- function(x){sd(x)/sqrt(length(x))}

# Calculate the mean values and standard error for each ecosystem service type
means <- c(mean(provision), mean(regulate), mean(culture))
ses <- c(se(provision), se(regulate), se(culture))

# Create a dummy vector for the x-axis
xax <- c(1,2,3)


# Make plot for Figure 2
png(filename="Figure 2.png",width=12,height=12,units="cm",res=600)
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



#############################
#							#
# 	Table 2 in manuscript	#
#							#
#############################

# Calculate the means and standard error of self-reported damage activities 
dam.self <- c(mean(Q14), mean(Q6), mean(Q16),mean(Q4))
se.dam.self <- c(se(Q14), se(Q6), se(Q16),se(Q4))

# Comine the data in columns
self.reported <- cbind(dam.self,se.dam.self)
# Add row names
rownames(self.reported) <- c("Grazing", "Ploughing", "Dumping", "Burning")
self.reported




# Calculate the means and standard error of community-expected damage activities 
dam.group <- c(mean(Q2), mean(Q12), mean(Q10),mean(Q8))
se.dam.group <- c(se(Q2), se(Q12), se(Q10),se(Q8))

# Comine the data in columns
group.expected <- cbind(dam.group,se.dam.group)
# Add row names
rownames(group.expected) <- c("Grazing", "Ploughing", "Dumping", "Burning")
group.expected



#########################################################


#############################
#							#
# 	Table 4 in manuscript	#
#							#
#############################


# Create new variables to denote the average damage and benefit from wetalnds
# Define education, gender, employment (Work) and role in the commuity as discrete factors

damage <- apply(cbind(Q2,Q4,Q6,Q8,Q10,Q12,Q14,Q16),1,mean)
benefit <- apply(cbind(Q1,Q3,Q5,Q7,Q9,Q11,Q13,Q15),1,mean)
education <- as.factor(Education)
gender <- as.factor(Gender)
employment <- as.factor(Work)
role <- as.factor(Position)
age <- Age
residence <- Residence

#Explore collinearity
plot(age~role)
cor.test(age,residence,method="spearman")
# There is collinearity between 'age' and both 'role in community' and 'duration of residence'

# Define the multiple linear regression
es.lm <- lm(benefit ~ damage + age + education + gender + employment , na.action = "na.fail")

# This command reports the coefficents used in Table 4 
summary(es.lm)

# This command is for the F-test assocaited with anova to test the significance of variable (Table 4)
anova(es.lm)


#########################################################


#############################
#							#
# 	Table 4 in manuscript	#
#							#
#############################


png(filename="Figure 3.png",width=18,height=12,units="cm",res=600)
par(mfrow=c(2,3))


# Damage causing activities
par(mai=c(0.7,0.5,0.1,0))
visreg(es.lm,xvar="damage", ylim=c(0,2), xlim=c(0,2), xlab="Self-reported damage", ylab="Self-reported benefit")
text(1.75,0.25,"(a)",cex=1.5)

# Age
par(mai=c(0.7,0.5,0.1,0))
visreg(es.lm,xvar="age", ylim=c(0,2), xlim=c(10,70), xlab="Age", ylab="")
text(65,0.25,"(b)",cex=1.5)

# Education status
par(mai=c(0.7,0.5,0.1,0))
visreg(es.lm,xvar="education", ylim=c(0,2), xlab="Level of education", ylab="", xaxt = "n")
axis(1,at=c(0.105,0.37,0.635,0.895),label=c("","","",""))
mtext(side=1, cex=0.5, at=c(0.105,0.37,0.635,0.895), line = c(1.5,1.5,1.5,1.5),text=c("Secondary\nschool", "Primary\nschool","Other\nschooling", "Tertiary\nschooling"))
text(0.9,0.25,"(c)",cex=1.5)

# Gender
par(mai=c(0.7,0.5,0.1,0))
visreg(es.lm,xvar="gender", ylim=c(0,2), xlab="Gender", ylab="Self-reported benefit", xaxt = "n")
axis(1, at=c(0.22,0.78), labels=c("Female", "Male"))
text(0.9,0.25,"(d)",cex=1.5)

# Employment
par(mai=c(0.7,0.5,0.1,0))
visreg(es.lm,xvar="employment", ylim=c(0,2), xlab="Employment", ylab="", xaxt = "n")
axis(1,at=c(0.105,0.37,0.635,0.895),label=c("","","",""))
mtext(side=1, cex=0.5,at=c(0.105,0.37,0.635,0.895), line = c(1.5,1,1,1),text=c("Community\nleader", "Employed","Unemployed", "Pensioner"))
text(0.9,0.25,"(e)",cex=1.5)
dev.off() 		# Figure saved to working directory


####################################################################################








