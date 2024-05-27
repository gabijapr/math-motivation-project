# Math motivation project
This repository is part the project **"A study on motivation and motivation for learning mathematics in grades 9-12"**, funded by the Research Council of Lithuania.

The abstract for this project is here: https://lmt.lrv.lt/media/viesa/saugykla/2024/5/vMp1sCb2EaM.pdf

Note that the `data`, `figures` and `results` folders are missing as the data is confidential and results are not fully published yet.

## Short description of the scripts
### 1. `variable_calc.R`

This R script is used for data preparation and transformation. It performs various calculations for plotting and analysis, then saves some of these results into excel. Statistical methods used (and saved) were: Kruskal-Wallis test, Cohen's d, regression.

The file calculates the variables:
1. `df_long_student` and `df_long_teacher`: data frame that contains student/teacher data in a "long" format, where each row is a single observation.

2. `avgs_student` and `avgs_teacher`: data frame or vector that contains average values of some metrics (Usage and effectivenes, namely N (Naudojimas) and V (Veiksmingumas)) for students / teachers. `avgs_student_merged` and `avgs_teacher_merged` is a version of avgs_student with additional information about the variables N and V.

3. `avg_merged_teach_stud` merged student and teacher data that was described previously. There is a melted version `avg_merged_teach_stud_melted`

4. `correlation_df` and `correlation_df_merged`: data fram that contains correlation values between GPA and V variables.


### 2. `graphs.R`

This R script is used for generating visualizations with  several variables calculated in `variable_calc.R`.

1. **Average bar plots**: plots the average values (bar plots, both horizontal and vertical) of Likert items (N and V) for various factor groups: 
    * Respondent type (i.e., teachers and students)
    * Location group
    * Gender
    * Class (students)
    * GPA group (students)
    * Age Category (teachers)
    * etc
2. **Likert 'distribution'**: i.e., percentage plot for likert values (how many % does each value take up), and each likert item as its own bar plot.
3. **Heatmaps of averages for factor groups**: since there are many variables (N, V go up to 39), these averages are also displayed as a heatmap.
4. **Correlation bar plot**: correlation values between student GPA and Likert values displayed in a vertical bar chart.


### 3. `analysis.R`

The script performs factor analysis and PCA on a dataset (`df_teacher` or `df_student`)

1. For Factor analysis, the script:
    *  tests the hypothesis that the correlation matrix is an identity matrix. (Bartlett spherical test)
    * calculates the Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy.
    * calculates how many factors should be retained by:
        *  creating a scree plot
        * performing parallel analysis (`fa.parallel`) 
    * performs factor analysis (here - with 3 factors and oblimin rotation).
    * creates a path diagram of the factor analysis.

2. For PCA, the script:

    * performs Principal Component Analysis (PCA) on the data with `prcomp()`.
    * calculates the variances and proportion of variances for the first 20 principal components.
    * creates a bar chart of the variances of the first 20 principal components using `ggplot()`, where the bars represent the variance explained by each principal component.

3. To be updated: cluster analysis