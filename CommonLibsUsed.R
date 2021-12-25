#Load common libraries
library(dplyr)
library(tidyverse)
library(data.table)
library(fixest)
library(car)
library(carData)



#Distinct
#Retain only unique/distinct rows from an input tbl
rm(list = ls())


df <- tibble(
  x = sample(10, 100, rep=TRUE),
  y = sample(10, 100, rep = TRUE)
)

nrow(df)
nrow(distinct(df))
nrow(distinct(df, x, y))

distinct(df, x)
distinct(df, y)


#Which
#Which indices are true

which(LETTERS == "C")

which(bl <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE))

names(ll) <- letters[seq(ll)]
which(ll)


#Paste
#Concatenate vectors after converting to character

paste0(1:12)

(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))

#Paste separates each input with a space.
(nth <- paste(1:12, c("st", "nd", "rd", rep("th", 9))))

#Collapse concats all in a single string
paste0(nth, collapse = ", ")


#Sink
#Sends output to R file

sink("sink-examp.txt")

#Array from 1 to 10
i <- 1:10

#Creates multiplication table - i * i
outer(i, i, "*")


sink()


#Merge
#Merge two dataframes

authors <- data.frame(
  ## I(*) : use character columns of names to get sensible sort order
  surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  deceased = c("yes", rep("no", 4)))


authorN <- within(authors, { name <- surname; rm(surname) })

books <- data.frame(
  name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis",
            "An Introduction to R"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA,
                   "Venables & Smith"))

(m0 <- merge(authorN, books))

(m1 <- merge(authors, books, by.x = "surname", by.y = "name"))



#Different Example for merge
(dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A"))
(dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A"))

merge(dt1, dt2)
merge(dt1, dt2, all = TRUE)


(dt1 <- data.table(A = letters[rep(1:3, 2)], X = 1:6, key = "A"))
(dt2 <- data.table(A = letters[rep(2:4, 2)], Y = 6:1, key = "A"))


merge(dt1, dt2, all = TRUE)


##Introduction to data.table

input <- if (file.exists("flights14.csv")) {
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}

#fread better than read.table
flights <- fread(input)
flights
dim(flights)

#Get all the flights with "JFK" as the origin airport in the month of June.
org_jfk_june <- flights[origin=="JFK" & month=="6"]
head(org_jfk_june)

#Get the first two rows from flights.
first_2r <- flights[1:2]
head(first_2r)

#Sort flights first by column origin in ascending order, and then by dest in descending order
sort_order <- flights[order(origin,-dest)]
head(sort_order)

#Select arr_delay column, but return it as a vector.
arr_delay_vec <- flights[, arr_delay]
head(arr_delay_vec)


#Select arr_delay column, but return as a data.table instead
flights[, list(arr_delay)]


#Select both arr_delay and dep_delay columns
flights[, .(arr_delay, dep_delay)]


#Select both arr_delay and dep_delay columns and rename them to delay_arr and delay_dep.
flights[, .(delay_arr = arr_delay, delay_dep=dep_delay)]



#How many trips have had total delay < 0?
flights[, sum( (arr_delay + dep_delay) < 0 )]


#Calculate the average arrival and departure delay for all flights with "JFK" as
#origin airport in the month of June.
flights[origin=="JFK" & month == 6,
        .(m_arr = mean(arr_delay), m_dep = mean(dep_delay))]


#How many trips have been made in 2014 from "JFK" airport in the month of June?
flights[origin=="JFK" & month==6, .N]



#Get the number of trips corresponding to each origin airport?
flights[, .(.N), by = .(origin)]
flights[, .N, by = origin]


#Calculate the number of trips for each origin airport for carrier code "AA"?
flights[carrier=="AA", .N , by = origin]


#Get the total number of trips for each origin, dest pair for carrier code "AA"
flights[carrier=="AA", .N, by = .(origin, dest)]


#Average arrival & departure delay for each orig,dest pair for each month for carrier code "AA"
flights[carrier == "AA",
        .(mean(arr_delay), mean(dep_delay)),
        by = .(origin, dest, month)]


#Order by all the grouping variables
#Using KeyBy
flights[carrier == "AA",
        .(mean(arr_delay), mean(dep_delay)),
        keyby = .(origin, dest, month)]


#Predict
#Function mainly used for linear models
mod <- lm(interlocks ~ log(assets), data=Ornstein)

newd <- data.frame(assets=exp(4:12))

(p1 <- predict(mod, newd, interval="prediction"))
p2 <- Predict(mod, newd, interval="prediction", vcov.=vcov)
all.equal(p1, p2)

(predict(mod, newd, se=TRUE))
(p3 <- Predict(mod, newd, se=TRUE, vcov.=hccm)) # larger SEs
p4 <- Predict(mod, newd, se=TRUE, vcov.=hccm(mod, type="hc3"))
all.equal(p3, p4)


#GLM model
#Fitting generalized linear models
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9) #Generate level factors
treatment <- gl(3,3)
print(d.AD <- data.frame(treatment, outcome, counts))
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
anova(glm.D93)


#CBind
#Take a sequence of vector, matrix or data-frame arguments and combine by columns or rows
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m
cbind(1:7, diag(3)) # vector is subset -> warning

cbind(0, rbind(1, 1:3))
cbind(I = 0, X = rbind(a = 1, b = 1:3))  # use some names
xx <- data.frame(I = rep(0,2))
cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently

cbind(0, matrix(1, nrow = 0, ncol = 4)) #> Warning (making sense)
dim(cbind(0, matrix(1, nrow = 2, ncol = 0))) #-> 2 x 1





















































