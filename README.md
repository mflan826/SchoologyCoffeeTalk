# Identifying and Categorizing Schoology Coffee Talk Participants By LEA & Regional Office
This script is used by the Edu Consultants to identify the LEA of each participant in the "Coffee Talks" (Padlets) in the I13 Schoology courses. There are 500+ individuals and 7 different Padlets they are using inside of their I13 course (they are all embedded as iframes). The data is exported from the Padlets, run through the script and a csv is exported containing the following fields - participation ID, the title of their post, body of the post, date that they last updated their post, LEA, Region. They want to view them in sheets on their own and use them for different ACT48 purposes, that's the reason for the exports. 

**How it works**:
Grepl is used to find the sub-strings of the LEA names within the subject and body fields of the orginal data. Most referred to their school with a partial name or nickname so the script changes all of the variations for each LEA to a standarized name. A region is assigned to the case when the name matches with three regional lists of LEAs participating. 

**Requirements**: Tidyverse for piping, tibbles, mutating, and manipulating sub-strings. 
