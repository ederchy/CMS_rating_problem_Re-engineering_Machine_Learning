# CMS_rating_problem_Re-engineering_Machine_Learning

Reference: https://www.cms.gov/medicare/quality-initiatives-patient-assessment-instruments/hospitalqualityinits/hospitalcompare

Hospital Compare is a consumer-oriented website that provides information on how well hospitals provide recommended care to their patients. This information can help consumers make informed decisions about where to go for health care. Hospital Compare allows consumers to select multiple hospitals and directly compare performance measure information related to heart attack, heart failure, pneumonia, surgery and other conditions. These results are organized by:

General information
Survey of patients' experiences
Timely & effective care
Complications
Readmissions & deaths
Use of medical imaging
Payment & value of care
Access the Hospital Compare Web site at www.medicare.gov/hospitalcompare
 

Hospital Compare was created through the efforts of Medicare and the Hospital Quality Alliance (HQA). The HQA: Improving Care Through Information was created in December 2002. 

_______________________________________

Business understanding
CMS rates hospitals in the US on a scale of 1-5 with the objective to make it easier for patients and consumers to compare the quality of hospitals.

 

The ratings directly influence the choice of the hospital made by consumers and may have a significant impact on the revenue earned by hospitals. Thus, it is extremely important for hospitals to understand the methodology used by CMS for calculating the ratings so that they can work on improving the factors that influence them.

 

This project is focused on developing an approach to calculate hospital ratings and using it to identify areas of improvement for certain hospitals. It will also require a thorough understanding of the rating system developed by CMS.

 

You can read the details of the CMS rating project here.

 

In this project, you will approach the ‘CMS rating problem’ from different angles. You can think of it as ‘re-engineering the CMS rating system’.

 

There are three main parts of the project as described below. Note that you are expected to conduct suitable EDA for each part of the problem and clearly mention the insights in your report.

 

You can download a template of the report to be submitted from the bottom of this page. 

 

Data understanding  
The data can be downloaded from the hospital compare website. You need to download 'Hospital_Revised_FlatFiles_20161110'. 

 

Modelling 
Part 1 - Supervised Learning-Based Rating

Think about the types of supervised models you will choose for predicting the star ratings (1-5, five-class classification). Consider all the practical as well as technical aspects involved. In the report, mention the types of models you will consider, the pros and cons of each, and the reasons for choosing certain models for this problem. Note that you have to build only one type of model (as mentioned in part 2 below). 
Create a random forest to predict the hospital ratings (1-5) using the 64 measures specified by the CMS.  
Evaluate your model using relevant techniques. Report the relative importance of measures according to their predictive power. Finally, report the overall accuracy of predictions using the actual star ratings.
 

Part 2 - Clustering-Based Rating (Unsupervised)

The methodology followed by CMS to assign ratings is described in the report here. It is an unsupervised method. Create an unsupervised clustering model to assign hospital ratings (1-5) using the 64 measures specified by the CMS. The main problem is to calculate the 7 group scores for each hospital. Various methods exist to conduct this analysis, two of which are mentioned below. The second method is similar to the one used by CMS. You can choose either of these ways to complete this part of the project.  

 

Method 1: Figure out your own way to assign weights to the measures in respective groups. The method may or may not be similar to the one used by the CMS, though it should be based on sound reasoning. Using the weights, calculate 7 group scores for each hospital. You can then take the weighted average of group scores to calculate a final score of each hospital, perform clustering, and assign a star rating to each cluster.
    

Method 2: This method uses factor analysis (or latent variable analysis) to find the weight of each measure in respective groups. Using factor analysis, find the group score for each hospital. You can then take the weighted average of group scores to calculate the final score of each hospital, perform clustering, and assign a star rating to each cluster.
Note: You can learn factor analysis from the additional content provided below

 

Using the method of your choice, develop a procedure to:

Calculate and report the weights of all the measures within the respective groups
Compare the weights obtained using this method with the ones obtained from the random forest
Report the group score and the final score of each hospital
Report the final star rating of each hospital  
Compare the rating assigned by you with the actual ratings assigned by CMS and report the analysis using relevant evaluation/comparison metrics, i.e. evaluate your unsupervised model
 

Part 3 - Recommendations for Hospitals

Suppose you work for a healthcare consulting company. The hospital with Provider ID = 140010 (EVANSTON HOSPITAL) is your client. 
