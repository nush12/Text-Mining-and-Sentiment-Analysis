---
author: "Anusha Chintakunta Manjunatha"
output: 
  html_document:
     keep_md: true
     theme: lumen
---





## Text Mining and Sentiment Analysis of English  Song Lyrics


### Introduction 


*****************************************************************

**What are we doing?**

******************************************************************
In this exercise we have used some Text mining and Sentiment analysis packages available in R to derive key insights from the lyrics dataset obtained from Kaggle. The aspects analyzed are as follows.  

* Who are the artists with the most number of songs?
* What are artists singing about?
* How long are the songs?
* What sentiments do the songs evoke?
* What words appear together in the songs written by the Killers?  
  
  
********************************************************************  

**Data**  

********************************************************************  

The data was obtained from Kaggle. There was some amount of cleaning already done on the data in the form of removing inconvenient data like removing non-English lyrics, extremely short, extremely long lyrics and lyrics with non-ASCII symbols. The original data contains lyrics for 57650 songs. We have used the entire dataset for text mining, however used only 30% of the songs for sentiment analysis
  
  The dataset contains 4 variables:  
  
Variable | Desctiption  
-------- | -----------  
ARTIST   | Name of the artist  
SONG     | Name of the song  
LINK     | It is a link to a webpage containing the song  
LYRICS   | It is the lyrics of the song identified in the song column

********************************************************************

**Packages Required**

********************************************************************

Following are the packages required to analyse the given datasets:


* `readr` - It provides a fast and friendly way to read rectangular (csv, tsv) data 
* `dplyr` - dplyr provides verbs that helps in data manipulation of objects like data frame
* `tidyr` - It is designed specifically for data tidying and works well with 'dplyr' data pipeline
* `stringr` - It is used for character manipulation, pattern matching in strings, etc.
* `tm` - A framework for text mining applications within R.
* `plotly` - plotly is used for creating interactive webpages in R
* `qdap` - package designed to assist in quantitative discourse analysis
* `SnowballC` - for word stemming
* `widyr` -  package wraps the pattern of un-tidying data into a wide matrix
* `quanteda` - Provides functionality for corpus management,creating and 
               manipulating tokens and ngrams
* `knitr` - knitr is used for dynamic report generation
* `wordcloud` - To create a frequent wordcloud
* `tidytext` - create texts in a tidy format
* `ggraph` - to create graphs 
* `igraph` - to handle large graphs, with millions of vertices and edges


 
Please refer to http://rpubs.com/Nush12/textmining for more details
