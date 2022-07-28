# README
## Project BAWKOS
Predicting the outcome of a football match (or result of any sporting event) is a complex and challenging task due to the large variety of factors and randomness involved. Project BAWKOS strives to create and compare models and algorithms that predict professional English Premier League football match outcomes (win, loss, or draw) using artificial intelligence. The project aims to build models that are as accurate as possible, and at minimum better than a random or informed guess.

## Design Overview
<img width="634" alt="image" src="https://user-images.githubusercontent.com/58704773/181637497-109c3e5f-9d7a-4467-a6f2-1ab52a2a0266.png">

## Tech Stack
* R version 4.1.2

## Setup and Running
Prerequisites
* R Version 4.1.2 (prefered) or 4.1.1
  * Ensure all packages are installed correctly
  * Models - each run a, b and c will obtain results for their respective models:
* KNN:
  * Run preprossessing_knn.r
  * Run knn_classifier.r
* NN:
  * Run preprossessing_nn.r
  * Run nn_classifier.r
* RF:
  * Run preprossessing_rf.r
  * Run rf_classifier.
