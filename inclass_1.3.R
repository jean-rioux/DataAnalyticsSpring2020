library(dplyr)
multiv=read.csv("C:\\Users\\Jean\\Documents\\2020 Spring\\ITWS 6600\\Lab1-Part2\\multivariate.csv")

attach(multiv)
names(multiv)

plot(Income,Immigrant,main="Scatterplot")
plot(Immigrant,Homeowners)

mm=lm(Homeowners~Immigrant)
mm
plot(Immigrant,Homeowners)
abline(mm,col=2,lwd=3)

summary(mm)
attributes(mm)
mm$coefficients

multiv$hp=Homeowners/Population
multiv$pd=Population/area

mlm=lm(Homeowners~Immigrant+Population+area)
mlm
mm

detach(multiv)


df_mtcars=mtcars
head(df_mtcars)

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
