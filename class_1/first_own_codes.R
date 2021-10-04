#########################
## Getting familiar    ##
##  with R and Rstudio ##
##      CLASS 1        ##
##       CEU           ##
#########################

2+2

myString <- "Hello world!"
print(myString)

# We can define numbers
a <- 2
b <- 3

a+b-(a*b)^a

c <- a + b
d <- a*c/b*c

# Use of logical operators
a == b   # True or False
2 == 3
( a + 1 ) == b
a <- 2

a != b  # not equal

# other logical operators
2 == 2 & 3 == 2   #combine with "AND"
2 == 2 | 3 == 2   #combine with "OR"

# Remove variables from work space
rm(d)

##
# Create vectors
v <- c(2,5,10)
# Operations with vectors
z <- c(3,4,7)

v+z
v*z
a+v

# length() counts the number of elements
num_v <- length(v)
num_v

# Create vector from vectors
w <- c(v,z)
w
length(w)
# Gives an error
length(W)

# Note: be careful w operation
q <- c(2,3)
v+q
v+c(2,3,2)


## Extra:
null_vector <- c()
# NaN value
nan_vec <- c(NaN,1,2,3,4)
nan_vec + 3
# Inf values
inf_val <- Inf
5/0
sqrt(2)^2==2
round(sqrt(2)^2)==2

# Convention to name your variables
my_fav_var <- "bla"
myFavVar <- "bla"
# Rarely use long names such as
my_favourite_variable <- "bla"

# Difference between doubles and integers
int_val <- as.integer(1.9)
int_val
doub_val <- as.double(1)


#
typeof(int_val)
typeof(myString)
is.character(myString)

##
# INDEXING - goes w []
v[1]
v[2:3]
v[c(1,3)]   #ask for 1st and 3rd elements using connacating

# Fix the addition of v+q
v[1:2] + q   #take the first 2 elements of v
v[c(1,2)] + q

#rewriting specific values in a vector
t <- c(4,5,6.1,8.1)
t[ c(3,4)] <- as.integer( t[3:4])

####
# Lists
my_list <- list("a",2,0==1)
my_list2 <- list(c("a","b"),c(1,2,3),sqrt(2)^2==2)

# indexing with lists:
# you get the list's value - still a list (typeof(my_list2[1]))
my_list2[1]
typeof(my_list2[1])
# you get the vector's value - it is a character (typeof(my_list2[[1]]))
my_list2[[1]]
typeof(my_list2[[1]])
# you get the second element from the vector
my_list2[[1]][2]
typeof(my_list2[[1]][2])

## Practice and read R for Data Science Chapter 16
