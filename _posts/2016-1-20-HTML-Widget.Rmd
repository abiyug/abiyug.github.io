---
title: "HtmlWidge"
author: "Abiyu Giday"
date: "January 21, 2016"
output: html_document
---
##Summary 
To help facilitate tidying and presenting the data,  I will be using the Salaries dataset found in the _Companion to Applied Regression_ (car) package.  The question we will be seeking to answer from this analysis is: **For the same experience and education level is there parity  between  men and women Salaries in colleges?**

It is assumed some unerstanding of R prorgaming is ncessary inorder to understanding the codes. However, the content should be self explantory.

One of the key pillars for data analysis getting a messy data and manipulating it to make it ready for further analysis.  It is often said, even by more experienced data scientists, that **80%** of data analysis is spent on preparing and cleaning data.  R has several packages that can do the job.  The challenge with R is that there are too many different ways of performing the same tasks. That can can lead to cnfusion. From the more cryptic to none-comprehensible  to comprehensible.  Depending on your level of experience and inclination you have choices that can help you wrangle and clean the data.  For me, [Hadly Wickham’s work](https://twitter.com/hadleywickham/likes), especially  on [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html), is preferable.  With just few lines of codes, logically constructed, one can _wrangle_ or manipulate  data, large and small, and can whip a messy data in to shape very fast!  

To demonstrate the tidying techniques, I am going to use the **Salaries** dataset from the [Companion to Applied Regression package](https://cran.r-project.org/web/packages/car/index.html). The data is a 2008-09 nine-month academic salary for Assistant Professors, Associate Professors and Professors in a college through out the U.S. The data were collected as part of the on-going effort of the college's administration to monitor salary differences between male and female faculty members. 

##Explore Data
First order of business is to load the data into R memory:

```{r sdata,results="asis",  message=FALSE }
library(dplyr)
library(car)
library(knitr)
library(DT)
datatable(Salaries)
#kable(head(arrange(Salaries, desc(as.numeric(salary))), 10))
```

As we can see the top 10 salaries belong to men. Infact, the highest earning women comes in at 33 place with $161,101.  Next, we want to see the internal structure of the data frame.

```{r structure,  message=FALSE}
str(Salaries)
#Alwas check if there is missing data
sum(is.na(Salaries))
#summary of the salary quartile distribution
summary(Salaries$salary)
```

Next we want to see the  men vs women brakedown, and count numbers of  of Professors, Associate Professors and Assitance Professors. 

# Leaflet Maps

```{r}
library(leaflet)
m = leaflet() %>% addTiles()
m  # a map with the default OSM tile layer

m = m %>% setView(-93.65, 42.0285, zoom = 17)
# popup
m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')
rand_lng = function(n = 10) rnorm(n, -93.65, .01)
rand_lat = function(n = 10) rnorm(n, 42.0285, .01)

# use automatic bounds derived from lng/lat data
m = m %>% clearBounds()
# marker
m %>% addMarkers(rand_lng(), rand_lat())
```


```{r count,  message=FALSE}
# How many men vs women
table(Salaries$sex)

#How many prof, assoc, assit
table(Salaries$rank)

```

A quick plot (qplot), from *ggplot2* package, of the data will reveal the distribution of salary for both men and women seprately. Box plot graph shows a five number summary. The horizontal line in the middle of each box shows the **median**, and the box top/bottom lines show the first and third quartile.

```{r plot1,  message=FALSE}
library(ggplot2)
#box plot shows  
qplot(sex, salary, data = Salaries, geom=c("boxplot", "jitter"), colour = rank, main="2008 US college Gender based Salary distribution.", xlab = "Gender", ylab = "Salary in USD $" )
```

From the Graph, we can see that there are more men professors than there are women, and more men professor earn higher salary than associate or assitant professor.  Inorder to make an apple to apple comaprison and answer our stated question we have to **manipulate** the data in a way tha we can compare the averages of men and women Assitant Professors, Associate Professors and Professors with the respective rank.  The next step uses dplyr to get the data ready. 

##Tidy data with dplyr
From here on out we will be using the **dplyr** functions to  tidy the data. The  symbol **%>%**, otherwise known as 'pipe',is used to pass data frame along the sequency of dplyr functions and allow several the functions to modify, sumerize and calulate values out of the data and render a result as an outcome. [Here is tutorial for dplyr from the man himself](http://datascience.la/hadley-wickhams-dplyr-tutorial-at-user-2014-part-1/).  Here the  'filter' function is used to select rows and 'select' fucntion is used to select columns.   The following code will render a data frame that contains  the average salary for men and wome with thir respective ranks. 

```{r avgsalary, message=FALSE}
library(dplyr)                  #Load a dplyr
#Average overall salary of men 
Salaries %>%  
        filter( sex == "Male" ) %>% 
          select(salary) %>% 
            summarise_each(funs(mean))

#Average overall salary for women 
Salaries %>%  
        filter( sex == "Female") %>% 
           select(salary) %>% 
              summarise_each(funs(mean)) 
```

The following code will provide the average salary for each professorial rank, store it in a data frame named tt. This new data frame will be used in the follwoing graph. 
```{r plot2, message = FALSE}
tt <- Salaries %>% 
    group_by(rank, salary, sex) %>% 
           select(sex, rank, salary, yrs.service) 
head(tt)
```

##Graphs
First, we will be using the quick plot function to genrate a faceted plot for each gender and their respective counts.  
```{r plot3, message = FALSE}
qplot(yrs.service, salary, data = tt, colour = sex, size = salary, facets = sex ~ rank ,main="2008 US college Gender Salary brakedown.", xlab = "No. of yrs in Service", ylab = "Salary in $US" )

```
The graph shows that there are more men in all rank categories. The bubble size indicate the size of the salary. This shows that there are more men earning higher salary than women in as professors for example. 

Because there are more men than women, we have to find the average salary per rank and gender.  The following code uses dplyr and wrangle the original data to render a dataframe named 'pp'.  This new data frame will be used to plot the final comparison charts. 
  
```{r plot4, message = FALSE}
pp <- Salaries %>% 
        group_by(sex, rank) %>% 
           select(sex, rank, salary, yrs.service) %>% 
                                summarise_each(funs(mean))  
pp
```

The following graph uses the new data frame (pp) and to plot the graph that will answer our question.

```{r plot5, message = FALSE}
ggplot(pp, aes(x = rank, y = salary, fill=sex)) + 
        geom_bar(stat="identity", position=position_dodge()) +
        ggtitle("2008 US college Gender based Salary distribution.") +
        xlab("Professor's Rank") + 
        ylab("Salary in USD") +
        theme(plot.title = element_text(size = 20), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
        geom_text(aes(label = paste("$",round(salary)), y = salary), position = position_dodge(width=1), size = 5)   
```

The plot clearly shows the salary comparison of men vs women in their respective teaching ranks.  

##Conclusion
The answer to our question as to who earns more salary in colleges between 2008-09 is answered from the graph.  **Men, on the average, earn more than women on every category.**  Going one step further, utlizing the yrs.service feature in the dataset, what if we want to find out what the pay comparison was for women after serviing five, ten or fifteen year.  Did it get better or gotten worse?  

The following ggplot plot will answer that question.  
```{r plot6, message = FALSE}
ggplot(data=pp, aes(x=yrs.service, y=salary, group=sex, colour=sex, size=4)) +
        geom_line() +
        geom_point() +
        ggtitle("2008 US college Gender based Salary distribution.") +
        #facet_grid(sex ~ .) +
        annotate("text", x = 18, y = 122000, label = "Femal") +
        annotate("text", x = 24, y = 128000, label = "Male") +
        annotate("pointrange", x = 13, y = 97000, ymin = 85000, ymax = 115000, colour = "purple", size = 2)
```

On this plot we see that after more than 13.5 years of service , women earning jumps to pass that of men's! So there is improvment, once women serv more than a decade. 

<br><br><br>
~EOF~




