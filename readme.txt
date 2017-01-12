#####################################
# DSL Project: Opinion Mining
# Group-2
# -----------------------------------
# Alok Mani Singh 15BM6JP03 (alokms2017@email.iimcal.ac.in)
# Bodhisattwa Majumder 15BM6JP11 (bodhisattwapm2017@email.iimcal.ac.in)
# Gautam Kumar 15BM6JP17 (gautamk2017@email.iimcal.ac.in)
# Pranita Khandelwal 15BM6JP31 (pranitak2017@email.iimcal.ac.in)
# Ramakrishna Ronanki 15BM6JP34 (ramakrishnar2017@email.iimcal.ac.in)
######################################
----------------------------------------------------------------------------------------------------------------------------
* The projects talks about a comlete opinion mining model whichs comprises all type of semantic comprehension of online reviews.
* Only the group memebers have the copyrights reserved to all the codes. 
* For further use, you are requested to refer any of the authors. 
* For any problem, please contact authors. All codes have been tested and they are running perfectly.
----------------------------------------------------------------------------------------------------------------------------
Each code requires certain packages. If not installed, while calling it will throw messages to install. Install and run.


1. Bootstrapped_Aspect_Segmenter.py

File takes in reviews as input in json format (filename: 218778.json) 

Output: Tokenized reviews into sentences along with their assigned aspect in csv format

Runtime: This code will take a significant amount of time to run as it runs for several iterations over all the reviews. (45-60 min)


2. doc_summarization.R

This is the code for LexRank approach.

File takes in the aspect tagged sentences of reviews (Aspect_tagged_sentences_218778 or the generated file by Bootstrapped_Aspect_Segmenter.py) derived from Aspect Segmenter algorithm and computes the most important sentences about a particular aspect using LexRank. 

Chnage the aspect name for which you want to generate LExRank sentences in the code.

Output: prints top-5 important sentences for an aspect on the console

Runtime: <5 min

3. aspect_score_calc.R

This is a function file. This needs to be run before running aspect_score.R in the environment

4. aspect_score.R

Requirement:    1. Install coreNLP package in R.
	        2. Download "stanford-corenlp-full-2015-12-09" (538 MB) from "http://stanfordnlp.github.io/CoreNLP/index.html#download"
		3. Unzip the downloaded folder and copy it to "Documents>R>win-library>3.2>coreNLP>extdata" folder		
		4. Code requires assignment of 6 GB memory

Input: score_cleaned.Rdata as R environment data, Review_rating_218778.csv as file input, aspect.csv as file imput

Output: The file generates overall sentiment for all the reviews and saves a vector into a csv named "aspect_score.csv"

Runtime ~ 10 mins

5. LARA_aspect_segmenter.R

Input: This file takes the review file the review file Review_rating_218778.csv as file input

Output: It generates a list of lists called list_wd

Runtime ~ 5 mins

6. LARA_LRR.R

The file needs to be run in the same environment where LARA_aspect_segmenter.R has been run

Output: It generates aspect weights in a csv named "aspect_weights.csv"

Taking the dot product of vectors generated in aspect_score.csv and aspect_weights.csv, the CSI can be generated and the graph can be plotted.

Runtime ~ 10 mins