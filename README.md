# BRR-MBD
The files in this repository can be used to fit Bayesian models for estimating Bayesian Rate Ratio (BRR) for a multiple baseline design (MBD). Please download all the files into the same folder. This folder needs to be set as the working directory in the main file which is titled MBD_analysis.R.
The file titled plot_SSD_mbd.R contains the function that can be used to plot the figure for a MBD with 3 participants. If there are more participants, changes need to be made to the grid.arrange command line on lines 60-61 in this file.
The file titled fit_denom_MBD.R contains the function that can be used to fit a BRR model for a MBD.
The main file called MBD_analysis.R reads the data from trauma_mbd.csv and then calls the fit_denom_MBD function to fit the model and out put it. This file also contains syntax to estimate non-overlap of all pairs (NAP) effect size.
