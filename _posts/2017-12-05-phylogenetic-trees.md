---
layout: post
title: "Phylogenetic Trees"
fb-img: https://tykiww.github.io/img/Narrative phylogeny.png
tags: [hard-coding, ape, base R, evolutionary biology]
---

Several month ago, I was cruising on the packages folder of my Rstudio when I "accidentally" clicked on the "ape" package. At first, I saw nothing interesting.... 

Turns out, "ape" happened to be an acronym for Analysis of Phylogenetic and Evolution (how creative). Of course, that meant nothing to me but I decided to look up what this was. Soon after, I ran into [this](https://image.slidesharecdn.com/phylogeneticanalysis-111117220939-phpapp01/95/phylogenetic-analysis-7-728.jpg?cb=1321568578).

![](https://tykiww.github.io/img/phyl/phyl%20check.png)

If you want to read up on more detail about this event, click [here](http://www.nytimes.com/1993/06/06/weekinreview/aids-and-a-dentist-s-secrets.html?pagewanted=all).

At first, I laughed. This photo made absolutely no sense. How does this simple tree diagram represent whether or not this dentist infected his patients with HIV? Yet, as I kept reading more about phylogenetic trees, I realized how this was not a method to infer conclusions but a case to make correlation. 

It turns out that the this analysis was performed by laying out the viral traits of each patient and creating an analysis of correlation through a maximum likelihood sampling with replacement (bootstrap procedure). Although the technique depends on the assumption of independence of the original sample, the bootstrap can be used to estimate bias and variance for confidence intervals, and for hypothesis testing in many situations. After sampling, ordinal values are created based on the correlation of each attribute and distributed amongst the clades (branches) of the tree. That was a mouthful. 

Simply said, this analysis takes like values and groups them together based on how similar they are! As for the case of the dentist infecting HIV to the patient, samples of the blood were taken from 6 of the patients and compared with each other (along with other control groups) and found that the characteristics of the virus in the patients and the dentist correlated very closely in the same branch. This type of analysis may be useful in areas such as linguistics, evolutionary biology, and even detective work.


_Can we say for certain that the doctor did malignantly infect his patients? No. Yet, there is potential evidence!_


After reading all this, I decided that I wanted to make my own tree diagram even though I knew nothing about how to go about it. After realizing how difficult this task was going to be, I resorted to create just one tree that simply showed how closely linked a group of interest was. I am sure that I can explore other packages. Someone is bound to have already created something similar.

So... Let's get started!

Just for fun, I decided to create a csv with a supposed "top ten" coding languages. I just randomly looked this up on the internet and found a list, removed the last one, and inserted R (you can see how conceited I am about R). The csv is at the bottom for reference. To get the data, I simply took a random sample of 15 computing language characteristics on wikipedia without replacement and displayed Y or N values to each one. 

We first start out by installing the "ape" package and creating a function to read the data.

```r
library("ape")

#Set up original functions
  
readData <- function(path) {
  table <- read.csv(path, stringsAsFactors=FALSE)
  return(table)
} #1st
```

Next, is a function that takes the table and creates a matrix that returns the correlating values of each row. Probably could have done it more simple, but just hardcoded. Also, I found out for the first time that I did not need curly brackets to complete a for loop.. Just shows my inexperience. Go figure.

```r
Relationship <- function(table) {
  rows <- nrow(table)
  cols <- ncol(table)
  rMatrix <- matrix(0,nrow = rows, ncol = rows) 
  for (i in 1:(rows - 1))
    for (j in (i+1):rows)
      for (k in 2:cols)
        if (table[i,k] == table[j,k]) {
          rMatrix[i,j] <- rMatrix[i,j] + 1
        #May adjust to add weighted value
        return(rMatrix)}
}	
```

I hope you ignore this next portion of code. I don't think I can even explain it well enough. Simply, I transformed the correlation matrix and attached names to connect ordinal values so it would match the "Newick" or "New Hampshire" format.

```r
makeTreeText <- function(matrix, table) {   #Longer function

  #offset function used to take two values and take the
  #first value - 1 unless the second is bigger,
  #then return second value.
    offset <- function(val, vec) {
      offset <- 0
      for(item in vec) {
        if (val > item)
          offset <- offset + 1
      }
      return(val - offset)
    }
    #function creates a new matrix 
    averageMatrix <- function(matrix, vec) {
      newMatrix <- matrix(0, nrow=nrow(matrix) - (length(vec) - 1),
                         ncol=ncol(matrix) - (length(vec) - 1))
      combine <- vec[1]
      remove <- vec[2:length(vec)]
      for (col in 2:ncol(matrix)) {
        for (row in 1:(col-1)) {
          if ((row %in% remove) || (col %in% remove)){
            next
          }
          else if (row == combine) {
            newCol <- offset(col, remove)
            sum <- matrix[row,col]
            for(r in remove) {
              sum <- sum + matrix[min(r,col), max(r,col)]
            }
            newMatrix[row,newCol] <- sum/length(vec)
          }
          else if (col == combine){
            newRow <- offset(row, remove)
            sum <- matrix[row,col]
            for(c in remove) {
              sum <- sum + matrix[min(c,row), max(c,row)]
            }
            newMatrix[newRow,col] <- sum/length(vec)
          }
          else {
            newRow <- offset(row, remove)
            newCol <- offset(col, remove)
            newMatrix[newRow, newCol] <- matrix[row,col]
          }
        }
      }
      return(newMatrix)
    }
    
    #These titles are merged to pass through the plot Treefunction!
    mTitles <- function(titles, indices) {
      merged <- paste(titles[indices], collapse=",")
      merged <- paste("(",merged,")",sep="")
      remove <- titles[indices[2:length(indices)]]
      dTitles <- titles[!titles %in% remove]
      dTitles[indices[1]] <- merged
      return(dTitles)
    }
    
    #Just a find and choose function
    uniqueAppend <- function(vec, val) {
      if(val %in% vec)
        return(vec)
      return(append(vec, val))
    }
    
    #Finally combining the titles with the new average matrix
    text <- ""
    titles <- table[,1]
    while(length(titles) > 1) {
      highVal <- 0
      highVec <- c()
      for (col in 2:ncol(matrix)) {
        for (row in 1:(col-1)) {
          if(matrix[row,col] > highVal) {
            highVal <- matrix[row,col]
            highVec <- c(row,col)
          }
          else if(matrix[row,col] == highVal &&
                  (row %in% highVec || col %in% highVec)) {
            highVec <- uniqueAppend(highVec, row)
            highVec <- uniqueAppend(highVec, col)
          }
        }
      }
      titles <- mTitles(titles, highVec)
      matrix <- averageMatrix(matrix, highVec)
    }
    text <- titles[1]
    text <- paste(text,";",sep=" ")
    return(text)
 ```

Aaand finally, this is the best part. Combining the functions and plotting the tree. Turns out, the ape package only uses the read.tree function inside. All this setup for one measly function. Yet, I am sure this took a lot of work so I won't bash on it.

```r
  #PLOT TREE FUNCTION BELOW
  plotTree <- function(path) {
	  table <- readData(path)
	  matrix <- buildRelationshipMatrix(table)
  	phylogenyText <- makeTreeText(matrix,table)
	  tree <- read.tree(text=phylogenyText)
	  plot(tree)
  }

plotTree("prog.csv")
```

![](https://tykiww.github.io/img/phyl/phylcode.png)

ALL THAT FOR ONE TREE!? 
Regardless, how neat is that? 


This may not have been how you would categorize the languages, as I only took a random sample of the explanatory variables. Some variables carry more weight than others do, so you can see how different explanatory variables affect the tree in different ways.

I guess there is one thing that I am not completely satisfied with in my code. I can't seem to create spaces in betwen the titles no matter how hard I try!! I guess I shouldn't sweat it. No need to get caught up in insignificant details. 

Regardless, the neat thing about this tree is how it it also works with several categorical values. So, in essence, you could categorize each explanatory variable by a subset of the data (I have another example csv right here if you want to try it on your own.).

This is less of an analysis post, but more of a coding project that I created for myself. Regardless, I finally finished after sifting through the internetand piecing together details. This definitely took me a long time to finish and had been put aside for a while. Even after this, I still don't consider myself a great coder but there is a certain satisfaction to know, with invested time, I<sub/> can figure most things out<sub>. I hope you may find this useful!

[download](https://tykiww.github.io/assets/Phyl/prog.csv) the original matrix

[download](https://tykiww.github.io/assets/Phyl/examp_matrix.csv) an example matrix
