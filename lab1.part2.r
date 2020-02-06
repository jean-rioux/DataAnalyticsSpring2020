library(dplyr)

multi=read.csv("C:/Users/Jean/Documents/2020 Spring/ITWS 6600/Lab1-Part2/multivariate.csv")
View(multi)
# attached dataframe to make it easier to use
attach(multi)
?lm
mm=lm(Homeowners~Immigrant)
mm
summary(mm)$coef

plot(Homeowners~Immigrant)
?abline
abline(mm,col=2,lwd=3)

newimmigrantdata=data.frame(Immigrant=c(0,20))
mm %>% predict(newimmigrantdata)
mm
abline(mm)
abline(mm,col=3,lwd=3)
attributes(mm)
mm$coefficients

detach(multi)

plot(mtcars$wt,mtcars$mpg)
library(ggplot2)
qplot(mtcars$wt,mtcars$mpg)
qplot(wt,mpg,data=mtcars)
ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

# linear plot
plot(pressure$temperature,pressure$pressure,type="l")
points(pressure$temperature,pressure$pressure)
lines(pressure$temperature,pressure$pressure/2,col="red")
points(pressure$temperature,pressure$pressure/2,col="blue")
qplot(pressure$temperature,pressure$pressure,geom="line")
qplot(temperature,pressure,data=pressure,geom="line")
ggplot(pressure,aes(x=temperature,y=pressure))+geom_line()+geom_point()

# barplots
barplot(BOD$demand,names.arg=BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))

qplot(factor(cyl),data=mtcars)
ggplot(mtcars,aes(x=factor(cyl)))+geom_bar()

#histogram
hist(mtcars$mpg)
hist(mtcars$mpg,breaks=10)
hist(mtcars$mpg,breaks=5)
hist(mtcars$mpg,breaks=12)
qplot(mpg,data=mtcars,binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=4)
ggplot(mtcars,aes(x=mpg))+geom_histogram(binwidth=5)

# boxplot
plot(ToothGrowth$supp,ToothGrowth$len)
boxplot(len~supp,data=ToothGrowth)
boxplot(len~supp+dose,data=ToothGrowth)
View(ToothGrowth)
qplot(ToothGrowth$supp,ToothGrowth$len,geom="boxplot")
qplot(supp,len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=supp,y=len))+geom_boxplot()
qplot(interaction(ToothGrowth$supp,ToothGrowth$dose),ToothGrowth$len,geom="boxplot")
qplot(interaction(supp,dose),len,data=ToothGrowth,geom="boxplot")
ggplot(ToothGrowth,aes(x=interaction(supp,dose),y=len))+geom_boxplot()



# Dplyr data manipulation snippet

library(dplyr)
library(nycflights13)

head(flights)
summary(flights)

filter(flights,month==10,day==4,carrier=='AA')
head(filter(flights,month==10,day==4,carrier=='AA'))

head(flights[flights$month==10 & flights$day==4 & flights$carrier=='AA',])

slice(flights, 1:15)

arrange(flights,year,month,day,arr_time)
head(arrange(flights,year,month,day,arr_time))
head(arrange(flights,year,month,day, desc(arr_time)))

select(flights,carrier)
head(select(flights,carrier))
 
head(select(flights,carrier,arr_time))
head(select(flights,carrier,arr_time,day))
head(rename(flights,airline.carrier=carrier))

distinct(select(flights, carrier))

head(mutate(flights, MyNewColumn = arr_delay - dep_delay))

head(transmute(flights, MyNewColumn = arr_delay - dep_delay))


summarise(flights, avg_air_time = mean(air_time, na.rm = TRUE))
summarise(flights, TotalFlightTime = sum(air_time, na.rm = TRUE))

sample_n(flights, 15)
sample_n(flights, 71)

sample_frac(flights,0.1)
sample_frac(flights, 0.3)
sample_n(flights, 30)
sample_frac(flights, 0.5)

df_mtcars=mtcars
head(df_mtcars)


filter(df_mtcars,mpg>20)
sample_n(filter(df_mtcars,mpg>20),10)

arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))

results_mpg=arrange(sample_n(filter(df_mtcars,mpg >20),10),desc(mpg))
results_mpg

# method 1
filter(df_mtcars,mpg>20)
sample_n(filter(df_mtcars,mpg>20),10)
arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
result_mpg=arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
result_mpg

# method 2
a1=filter(df_mtcars,mpg>20)
a2=sample_n(a1,5)
results_mpg_des=arrange(a2,desc(mpg))
results_mpg_des

# method 3
df_mtcars %>%filter(mpg>20) %>% sample_n(5) %>%arrange(desc(mpg))
results=df_mtcars %>%filter(mpg>20) %>% sample_n(5) %>%arrange(desc(mpg))