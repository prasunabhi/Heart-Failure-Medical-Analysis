# Heart Failure Medical Analysis - Machine Learning for Business

## Overview
This project focused on identifying the key factors influencing heart failure using machine learning techniques. Through an in-depth analysis of clinical records, the study demonstrated proficiency in data analysis and advanced classification modeling, providing valuable insights for improving predictive accuracy in healthcare applications.

## Key Features
* Analyzed heart failure predictors using visualizations, perceptrons, and Support Vector Machines (SVM).
* Achieved high predictive accuracy, with SVM outperforming perceptrons and attaining an accuracy of 91.11%.
* Provided actionable insights to enhance predictive accuracy in healthcare, potentially aiding in early diagnosis and treatment of heart failure.

## Methodology
* **Data Source:** Heart Failure dataset from the University of California Irvine machine learning dataset repository.
* **Data Processing:** Conducted exploratory data analysis and visualization to identify key patterns and relationships.
* **Modeling Techniques:** Utilized Python with libraries such as Pandas, NumPy, and scikit-learn.
    * Perceptrons: Used for initial classification with varying predictor variables.
    * Support Vector Machines (SVM): Employed for improved accuracy, handling non-linear relationships and reducing sensitivity to outliers.

## Key Findings
* **Predictors:**
    * Age and Time: Identified as the most significant predictors, showing distinct patterns in survival and death events.
    * Ejection Fraction: High levels correlated with increased chances of death.
    * Serum Creatinine: Lower levels with less frequent follow-ups increased death risks, while more frequent follow-ups improved survival rates.
* **Model Performance:**
    * Perceptrons:
        * With 2 variables (Age and Time): 78% accuracy.
        * With 3 variables (Age, Ejection Fraction, and Platelets): 75.56% accuracy.
    * SVM:
        * With 2 variables (Age and Time): 86.67% accuracy.
        * With 3 variables (Age, Ejection Fraction, and Time): 91.11% accuracy.
        * With 5 variables (Age, Ejection Fraction, Time, Platelets, and Creatinine Phosphokinase): 86.67% accuracy.

## Lessons Learnt
* **Visualization:** Effective in identifying distinct clusters and patterns in data, though noisy data can obscure predictive variable clarity.
* **Model Comparison:** SVM outperformed perceptrons due to its ability to handle non-linear relationships and reduced sensitivity to outliers.
* **Alternative Techniques:** Neural networks and logistic regression could also be explored for classification tasks.

## Conclusion
The project highlighted the importance of age and follow-up time as key predictors for heart failure. By leveraging SVM and perceptrons, the analysis provided significant insights that can aid in early diagnosis and treatment planning, potentially improving patient outcomes in healthcare settings.

## Reference
* Dataset: Heart Failure Clinical Records dataset from the University of California Irvine machine learning repository. 
Available at: [UCI Heart Failure Clinical Records Dataset](https://archive.ics.uci.edu/ml/datasets/Heart+failure+clinical+records)
