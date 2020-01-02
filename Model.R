detach(Movie_Dataset)
attach(Movie_Dataset)
Movie_Dataset = na.omit(Movie_Dataset)

#install and require packages
install.packages("psych")
install.packages("car")
install.packages("olsrr")
install.packages("lmtest")
install.packages("plm")
install.packages("dplyr")
install.packages("data.table")
install.packages("caret")
install.packages("caTools")
install.packages("splines")
install.packages("stargazer")
require(stargazer)
require(splines)
require(caTools)
require(caret)
require(psych)
require(olsrr)
require(car)
require(lmtest)
require(plm)
require(dplyr)
require(data.table)
require(boot)

################
#Outlier Testing
################

#Individual Outlier Testing
reg_outliers1 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$duration)
outlierTest(reg_outliers1)
Movie_Dataset = Movie_Dataset[-c(2443, 2163), ]

reg_outliers2 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$director_facebook_likes)
outlierTest(reg_outliers2)
#Movie_Dataset = Movie_Dataset[-c(320), ]

reg_outliers3 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$actor_1_facebook_likes)
outlierTest(reg_outliers3)
Movie_Dataset = Movie_Dataset[-c(1140), ]

reg_outliers4 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$actor_2_facebook_likes)
outlierTest(reg_outliers4)
Movie_Dataset = Movie_Dataset[-c(114), ]

reg_outliers5 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$actor_3_facebook_likes)
outlierTest(reg_outliers5)
#Movie_Dataset = Movie_Dataset[-c(332), ]

reg_outliers6 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$total_actor_likes)
outlierTest(reg_outliers6)
#Movie_Dataset = Movie_Dataset[-c(1047), ]

reg_outliers7 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$movie_facebook_likes)
outlierTest(reg_outliers7)
#Movie_Dataset = Movie_Dataset[-c(707), ]

reg_outliers8 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$total_likes)
outlierTest(reg_outliers8)
#Movie_Dataset = Movie_Dataset[-c(878), ]

reg_outliers9 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$facenumber_in_poster)
outlierTest(reg_outliers9)
#Movie_Dataset = Movie_Dataset[-c(1656), ]

reg_outliers10 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$budget)
outlierTest(reg_outliers10)
#Movie_Dataset = Movie_Dataset[-c(445), ]

reg_outliers11 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$title_year)
outlierTest(reg_outliers11)
#Movie_Dataset = Movie_Dataset[-c(776), ]

reg_outliers12 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$aspect_ratio)
outlierTest(reg_outliers12)
Movie_Dataset = Movie_Dataset[-c(490), ]

reg_outliers13 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$movie_title_length)
outlierTest(reg_outliers13)
#Movie_Dataset = Movie_Dataset[-c(703), ]

reg_outliers14 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$country_code)
outlierTest(reg_outliers14)
#Movie_Dataset = Movie_Dataset[-c(142), ]

reg_outliers15 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$language_code)
outlierTest(reg_outliers15)
#Movie_Dataset = Movie_Dataset[-c(775), ]

reg_outliers16 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$plot_keyword_avg_length)
outlierTest(reg_outliers16)
#Movie_Dataset = Movie_Dataset[-c(826), ]

reg_outliers17 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$genre_count)
outlierTest(reg_outliers17)
#Movie_Dataset = Movie_Dataset[-c(2712), ]

reg_outliers18 = lm(Movie_Dataset$imdb_score ~ Movie_Dataset$content_rating_code)
outlierTest(reg_outliers18)
Movie_Dataset = Movie_Dataset[-c(1048), ]

#outlier regression model
reg_outliers = lm(imdb_score ~ duration + director_facebook_likes + actor_1_facebook_likes 
                  + actor_2_facebook_likes + actor_3_facebook_likes + total_actor_likes + movie_facebook_likes
                  + total_likes + facenumber_in_poster + budget + title_year + aspect_ratio + movie_title_length
                  + country_code + language_code + plot_keyword_avg_length + genre_count + content_rating_code)
#outlier test
outlierTest(reg_outliers)
#outlier visual test
qqPlot(reg_outliers, col="grey")

#remove outlier rows
Movie_Dataset <- Movie_Dataset[-c(1658, 2720, 2611), ]

#Test for colinearity
quantvars=Movie_Dataset[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
pairs.panels(quantvars)
#From this we know not to use total_actor_likes and total likes

####################
#Model Construction
####################

#relevel categorical variables

genre_1 = as.factor(genre_1)
genre_2 = as.factor(genre_2)
genre_3 = as.factor(genre_3)
genre_4 = as.factor(genre_4)
genre_5 = as.factor(genre_5)
genre_6 = as.factor(genre_6)
genre_7 = as.factor(genre_7)
genre_8 = as.factor(genre_8)
country = as.factor(country)
content_rating = as.factor(content_rating)
genre_1 = relevel(genre_1, ref="Other")
genre_2 = relevel(genre_2, ref='Other')
genre_3 = relevel(genre_3, ref='Other')
genre_4 = relevel(genre_4, ref='Other')
genre_5 = relevel(genre_5, ref='Other')
genre_6 = relevel(genre_6, ref='Other')
genre_7 = relevel(genre_7, ref='Other')
genre_8 = relevel(genre_8, ref='Other')
country = relevel(country, ref='USA')
content_rating = relevel(content_rating, ref='PG')

#Multiple linear reg with all variables
all_variable_reg = lm(imdb_score ~ duration + director_facebook_likes + actor_1_facebook_likes 
                      + actor_2_facebook_likes + actor_3_facebook_likes + movie_facebook_likes
                      + facenumber_in_poster + budget + title_year + aspect_ratio 
                      + movie_title_length + language_code + plot_keyword_avg_length + budget
                      + genre_count + content_rating + content_rating_code + genre_1 + genre_2 + genre_3          + genre_4  + genre_5 + genre_6 + genre_7 + genre_8 + country)
summary(all_variable_reg, type = "html")

#From this we keep the strongly correlated (1-2-3 stars) - excpet movie facebook likes
correlated_variable_reg = lm(imdb_score ~ duration + director_facebook_likes + actor_1_facebook_likes 
                             + facenumber_in_poster + title_year + movie_title_length
                             + language_code)
summary(correlated_variable_reg)

#Test for heteroskedasticity
ncvTest(correlated_variable_reg)
#since the p value is smaller than 0.05 then there is heteroskedasticity
#Correcting heteroskedasticity
coeftest(correlated_variable_reg, vcov = vcovHC(correlated_variable_reg, type = "HC1"))
#From this we realise title year is not actually important

#Ajusting for polynomials

#plot all to deicde what method to use
par (mfrow=c(2,3))
plot(duration, imdb_score, col=c("grey"))
plot(director_facebook_likes, imdb_score, col=c("grey"))
plot(actor_1_facebook_likes, imdb_score,col=c("grey"))
plot(facenumber_in_poster, imdb_score,col=c("grey"))
plot(movie_title_length, imdb_score,col=c("grey"))
plot(language_code, imdb_score,col=c("grey"))
par (mfrow=c(1,1))

#Conclusions: 

#language code - leave as linear from binary -- confired with tukey test
language = lm(imdb_score ~ language_code)
residualPlots (language)
par (mfrow=c(1,1))

#director facebook likes -- use knot as segmentaed data
quantile(director_facebook_likes, c(.20, .40, .60,.80))
#either at 6,33,96,287

#Actor 1 Facebooklikes
a11 = lm(imdb_score ~ poly(actor_1_facebook_likes, 1))
a12 = lm(imdb_score ~ poly(actor_1_facebook_likes, 2))
a13 = lm(imdb_score ~ poly(actor_1_facebook_likes, 3))
a14 = lm(imdb_score ~ poly(actor_1_facebook_likes, 4))
a15 = lm(imdb_score ~ poly(actor_1_facebook_likes, 5))
a16 = lm(imdb_score ~ poly(actor_1_facebook_likes, 6))
anova(a11,a12,a13,a14,a15,a16) #a1 is 5th

#movie title length
mtl1 = lm(imdb_score ~ poly(movie_title_length,1))
mtl2 = lm(imdb_score ~ poly(movie_title_length,2))
mtl3 = lm(imdb_score ~ poly(movie_title_length,3))
mtl4 = lm(imdb_score ~ poly(movie_title_length,4))
mtl5 = lm(imdb_score ~ poly(movie_title_length,5))
mtl6 = lm(imdb_score ~ poly(movie_title_length,6))
anova(mtl1,mtl2,mtl3,mtl4,mtl5,mtl6) ##mtl is 5th

#Duration
dur1 = lm(imdb_score ~ poly(duration,1))
dur2 = lm(imdb_score ~ poly(duration,2))
dur3 = lm(imdb_score ~ poly(duration,3))
dur4 = lm(imdb_score ~ poly(duration,4))
dur5 = lm(imdb_score ~ poly(duration,5))
dur10 = lm(imdb_score ~ poly(duration,10))
dur11 = lm(imdb_score ~ poly(duration,11))
anova(dur1,dur2,dur3,dur4,dur5,dur10, dur11) #use 10th degree

#Facenumber in poster
fnp1 = lm(imdb_score ~ poly(facenumber_in_poster,1))
fnp2 = lm(imdb_score ~ poly(facenumber_in_poster,2))
fnp3 = lm(imdb_score ~ poly(facenumber_in_poster,3))
fnp4 = lm(imdb_score ~ poly(facenumber_in_poster,4))
fnp5 = lm(imdb_score ~ poly(facenumber_in_poster,5))
fnp6 = lm(imdb_score ~ poly(facenumber_in_poster,6))
anova(fnp1,fnp2,fnp3,fnp4,fnp5,fnp6) # use 2nd degree

#Graphs with the fitted line
par (mfrow=c(2,3))
plot(duration, imdb_score, col=c("grey"))
lines(sort(Movie_Dataset$duration), predict(dur10)[order(Movie_Dataset$duration)], col="red")
plot(director_facebook_likes, imdb_score, col=c("grey"))
lines(sort(Movie_Dataset$director_facebook_likes), predict(dfb)[order(Movie_Dataset$director_facebook_likes)], col="green")
plot(actor_1_facebook_likes, imdb_score,col=c("grey"))
lines(sort(Movie_Dataset$actor_1_facebook_likes), predict(a15)[order(Movie_Dataset$actor_1_facebook_likes)], col="blue")
plot(facenumber_in_poster, imdb_score,col=c("grey"))
lines(sort(Movie_Dataset$facenumber_in_poster), predict(fnp2)[order(Movie_Dataset$facenumber_in_poster)], col="purple")
plot(movie_title_length, imdb_score,col=c("grey"))
lines(sort(Movie_Dataset$movie_title_length), predict(mtl5)[order(Movie_Dataset$movie_title_length)], col="yellow")
plot(language_code, imdb_score,col=c("grey"))
lines(sort(Movie_Dataset$language_code), predict(language)[order(Movie_Dataset$language_code)], col="orange")
par (mfrow=c(1,1))

#Final Model
final_variable_reg = lm(imdb_score ~ poly(duration, 10) + bs(director_facebook_likes, knots=c(6,33,96,287), degree=4)
                        + poly(actor_1_facebook_likes, 5) + poly(facenumber_in_poster,2)
                        + poly(movie_title_length, 5) + language_code + content_rating + country+ genre_1 + genre_2 + genre_3 + genre_4 
                        + genre_5 + genre_6 + genre_7 + genre_8)
summary(final_variable_reg)

########
#TESTING
########

#K-Fold test

Kfitlin=glm(imdb_score ~ poly(duration, 10) + bs(director_facebook_likes, knots=c(6,33,96,287), degree=4)
            + poly(actor_1_facebook_likes, 5) + poly(facenumber_in_poster,2)
            + poly(movie_title_length, 5) + language_code + content_rating + country+ genre_1 + genre_2 + genre_3 + genre_4 
            + genre_5 + genre_6 + genre_7 + genre_8)




cv.error_kf=rep(0,500) 
for (i in 1:500)
{
  cv.error_kf[i]=cv.glm(Movie_Dataset, Kfitlin, K=100)$delta[1]
}
mean(cv.error_kf)

############
#Predictions
############

value_Daddyshome2=data.frame(duration=100, director_facebook_likes=0, actor_1_facebook_likes=105518, facenumber_in_poster=10, movie_title_length=14,language_code=1, content_rating="PG-13", country="USA", genre_1="Comedy", genre_2="No Genre", genre_3="No Genre", genre_4="No Genre", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_Daddyshome2, type="response")

value_justiceleague=data.frame(duration=121, director_facebook_likes=183648, actor_1_facebook_likes=1527000, facenumber_in_poster=5, movie_title_length=14,language_code=1, content_rating="PG-13", country="USA", genre_1="Action", genre_2="Adventure", genre_3="Fantasy", genre_4="Other", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_justiceleague, type="response")

value_badmomsxmas=data.frame(duration=104, director_facebook_likes=0, actor_1_facebook_likes=0, facenumber_in_poster=6, movie_title_length=20,language_code=1, content_rating="PG-13", country="USA", genre_1="Action", genre_2="Adventure", genre_3="Comedy", genre_4="No Genre", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_badmomsxmas, type="response")

value_Thor=data.frame(duration=130, director_facebook_likes=0, actor_1_facebook_likes=0, facenumber_in_poster=9, movie_title_length=14,language_code=1, content_rating="PG", country="USA", genre_1="Action", genre_2="Adventure", genre_3="Comedy", genre_4="Other", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_Thor, type="response")

value_MotOE=data.frame(duration=114, director_facebook_likes=0, actor_1_facebook_likes=0, facenumber_in_poster=9, movie_title_length=28,language_code=1, content_rating="PG", country="USA", genre_1="Crime", genre_2="Drama", genre_3="Mystery", genre_4="No Genre", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_MotOE, type="response")

value_Wonder=data.frame(duration=113, director_facebook_likes=0, actor_1_facebook_likes=0, facenumber_in_poster=1, movie_title_length=6,language_code=1, content_rating="PG", country="USA", genre_1="Other", genre_2="No Genre", genre_3="No Genre", genre_4="No Genre", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_Wonder, type="response")

value_Coco=data.frame(duration=113, director_facebook_likes=0, actor_1_facebook_likes=23000, facenumber_in_poster=5, movie_title_length=4,language_code=1, content_rating="PG", country="USA", genre_1="Other", genre_2="Adventure", genre_3="Comedy", genre_4="Family", genre_5="Other", genre_6="Other", genre_7="Other", genre_8="No Genre")
predict(final_variable_reg, value_Coco, type="response")

value_Polaroid=data.frame(duration=88, director_facebook_likes=0, actor_1_facebook_likes=41030, facenumber_in_poster=0, movie_title_length=8,language_code=1, content_rating="PG-13", country="Canada", genre_1="Horror", genre_2="No Genre", genre_3="No Genre", genre_4="No Genre", genre_5="No Genre", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_Polaroid, type="response")

value_TheStar=data.frame(duration=86, director_facebook_likes=675, actor_1_facebook_likes=0,facenumber_in_poster=0, movie_title_length=8,language_code=1, content_rating="G", country="USA", genre_1="Other", genre_2="Adventure", genre_3="Comedy", genre_4="Family", genre_5="Other", genre_6="No Genre", genre_7="No Genre", genre_8="No Genre")
predict(final_variable_reg, value_TheStar, type="response")
