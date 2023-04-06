library(conjoint)

appearance=c("cute","miniature","funny")  
price=c("$15","$20","$25")
size=c("small","large")
battery=c("rechargeable","nonrechargeable")

levelnames = data.frame("levels" = c(appearance,price,size,battery))

experiment = expand.grid(appearance=appearance, price=price, size=size, battery=battery)
surveycards = caFactorialDesign(data=experiment,cards=9, type="fractional")

head(experiment)
print(surveycards)
cor(caEncodedDesign(surveycards))
profiles=caEncodedDesign(design=surveycards)

rownames(surveycards) <- NULL 
write.csv(surveycards, "C:\\Users\\Username\\Desktop\\survey_cards.csv") #<-----change dir here

preferences = read.csv(file="C:\\Users\\Username\\Downloads\\toy_data.csv") #<-----change to personal toy_data.csv file path here

print(profiles)

caModel(preferences[1,],profiles)
feature_importance=caImportance(y=preferences[1,],x=profiles)
barplot(feature_importance, names.arg = c("appearance","price","size","battery"))

u1 = caUtilities(y=preferences,x=profiles,z=levelnames)
prod_utility = data.frame(u1,row.names = c("intercept",as.list(levelnames)$levels))
print(prod_utility)

u2 = caImportance(y=preferences,x=profiles)
barplot(u2, names.arg = c("appearance","price","size","battery"))

prod_utility = data.frame(u1,row.names = c("intercept",as.list(levelnames)$levels))
print(prod_utility)

d1 = prod_utility['intercept',] + prod_utility['cute',] + prod_utility['small',] + prod_utility['nonrechargeable',] + prod_utility['$15',] 
print(d1)

r1 = d1*15.0

d2 = prod_utility['intercept',] + prod_utility['cute',] + prod_utility['small',] + prod_utility['nonrechargeable',] + prod_utility['$20',] 
r2 = d2*20.0

d3 = prod_utility['intercept',] + prod_utility['cute',] + prod_utility['small',] + prod_utility['nonrechargeable',] + prod_utility['$25',] 
r3 = d3*25.0

est_demand = c(d1,d2,d3)
est_rev = c(r1,r2,r3)
price_level = c(15.0, 20.0, 25.0)

revenue = data.frame(est_demand, est_rev, price_level)

plot(price_level, est_demand, type='o',xlab="price", ylab="demand")

plot(price_level, est_rev, type='o',xlab="price", ylab="revenue")
