# Automated cell detection and analysis using machine learning.

For Windows Users:
Double click "Run App (Windows User).bat" to run the analysis.

'Training.R' file is used to train a neural network. The trained neural network is then saved as 'nn1.rds'.

'Counting.R' file is used to load 'nn1.rds' and make predictions using images from 'Counting_Images' folder. The counted and filtered images are saved in 'Counted_Images' folder. The counted areas of fibrosis are saved in 'Counting_Images/Results.csv'.

Some training examples are provided in 'Training_Images' folder.
Two renal section images containing fibrosis are provided in 'Counting_Images' folder.
