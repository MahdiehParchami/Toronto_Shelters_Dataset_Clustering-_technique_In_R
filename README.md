# Toronto_Shelters_Dataset_Clustering-_technique_In_R
Implementing Clustering technique on Toronto Shelters Dataset In R

The data set contains 37781 observations with 32 variables. it included a combination of numeric, character, and integer variables that can be used to 
find the availability and capacity of the shelters with usage. Dataset sourced from Toronto.ca/ Open data.

I want to perform a Clustering technique to find groups with similarities in the data set.

Interpretation Via Descriptive Statistics: 

After running the algorithm and selecting three clusters, we can interpret the clusters by running a summary on each cluster. 
Based on these results, it seems Cluster 1 consists of shelters with high levels of emergency programs model and COVID-19 Response program, Temporary 
Refugee Response and Winter Programs area, medium levels of capacity available beds and occupied beds and smaller levels of unoccupied beds and Hour Women's
Drop-in service type. 
Also, Cluster 2 consists of shelters with high levels of Families and Mixed Adult users, 24-Hour Respite Site services and unoccupied beds, medium levels 
of emergency programs model and lowest levels of men users, COVID-19 Response program and Temporary Refugee Response area and capacity available beds.
Finally, Cluster 3 includes the highest levels of men, women, and youth users, The Transitional program model, shelter type service, Base Shelter and 
Overnight Services System program areas, and capacity actual beds. Also, it has the smallest winter Programs and warming center services.


Interpretation Via Visualization:

To visualize the cluster plot we applied t-distributed stochastic neighborhood embedding or t-SNE. 
Using this technique, local structures are preserved so clusters can be visualized in a 2D or 3D environment. Typically, it utilizes Euclidean distance, 
but it can also handle a custom distance metric, like the one we created.
We can see the plot shows the three well-separated clusters that PAM was able to detect. It seems there are small groups that are split between clusters.
Based on the interpretation, we identified similar groups and find the patterns behind using shelter by users. Therefore, we can recommend which shelter 
programs need to be improved by looking at the behavior patterns of users and which ones are not used by users and can be eliminated.
