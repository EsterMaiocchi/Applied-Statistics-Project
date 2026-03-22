# Applied Statistics Project: Personalized Air Pollution Exposure and Heart Rate Analysis

## Overview
This project was developed as part of a university course in Applied Statistics.  
It analyzes the relationship between environmental pollution and physiological responses, using data collected from wearable devices during a real-world experiment in Milan.

## Objective
The goal of the project is to investigate how exposure to air pollutants affects heart rate, and to identify the main drivers of physiological responses in different environmental conditions.

## Dataset
The dataset is based on an experimental study involving 20 healthy subjects who followed a predefined walking route in the Città Studi district (Milan).

For each subject, the following were recorded:
- Environmental variables: PM2.5, CO₂, VOC, temperature, humidity, pressure
- Physiological signals: heart rate (HR) and breathing frequency (FB)
- Measurements collected during both dynamic (walking) and static (standing) phases

Additional engineered variables include:
- Cumulative distance traveled
- Time spent at each location

## Methods
The project applies a range of statistical techniques, including:

- Exploratory data analysis and data preprocessing
- ANOVA for group comparisons
- Data transformations (Box-Cox, Yeo-Johnson)
- Linear Mixed Models (LMM) with random effects
- Interaction effects analysis
- Heteroskedasticity modeling (variance functions)

## Key Results
- Pollution levels across locations are not significantly different, likely due to the limited spatial scale of the experiment.
- Pollutants alone do not significantly affect heart rate, but become relevant when interacting with exposure time.
- The interaction between time and PM2.5 suggests that prolonged exposure slows down physiological recovery.
- Individual variability plays a major role, highlighting the importance of subject-specific effects.

## Tools
- R
- Statistical modeling and data analysis techniques

## Project Structure
- `Poster Applied Statistics Group 09.pdf`: summary of methodology and results
- `Modelli.R`, 'Process_V2.0 .R': data processing and modeling scripts (if included)

## Authors
- Ester Maiocchi, Marianna Mazza, Matteo Montanari, Alessandro Morra
