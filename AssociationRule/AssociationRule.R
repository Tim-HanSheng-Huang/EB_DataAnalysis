
# import library
if(!require(arules)){install.packages("arules")}
library(arules)
if(!require(arulesViz)){install.packages("arulesViz")} 
library("arulesViz")
library(graphics)

# load R built-in dataset HairEyeColor 
data(HairEyeColor)  
str(HairEyeColor)

# draw mosai plot
mosaicplot(HairEyeColor, main = "Hair Eye Color Mosaiplot")

# transfer the datset into 592*3 dataframe
df=as.data.frame(HairEyeColor) 
HairEyeColorData=NULL
for (i in 1:3) HairEyeColorData=cbind(HairEyeColorData,rep(as.character(df[,i]),df$Freq))
HairEyeColorData=as.data.frame(HairEyeColorData)  
names(HairEyeColorData)=names(df)[1:3]
summary(HairEyeColorData)
head(HairEyeColorData) 

# find frequen items by eclat function
eclat_FrequentItems <- eclat(HairEyeColorData, parameter=list(minlen=1, maxlen=3, sup=0.2, target="frequent itemsets"))
inspect(eclat_FrequentItem) 

# get the rule by apriori function
rule <- apriori(HairEyeColorData,
                parameter=list(minlen=3, supp=0.01, conf=0.5),  
                appearance = list(default="lhs",rhs=c("Sex=Male", "Sex=Female")))  
inspect(rule)

# sort the rules by lift
rulesort=sort(rule,by="lift")
inspect(rulesort)

# remove redundant rules
subset.matrix=is.subset(rulesort,rulesort)
redundant=colSums(subset.matrix) > 1 ; which(redundant)
rulepruned=rulesort[!redundant] ; inspect(rulepruned)

# visualization
library(arulesViz)
plot(rulepruned) # Heat map
plot(rulepruned,method="grouped") # Balloon plot
plot(rulepruned,method="graph",control=list(type="items"))  # Network graph 
plot(rulepruned, method = "paracoord", control = list(reorder = TRUE)) # Parallel coordinates plot
