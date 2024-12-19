# MOU_Index


THIS REPO IS CURRENTLY UNDER MAINTENANCE AS FILES ARE CONSOLIDATED INTO ONE FINAL CODE DOCUMENT, WHICH WILL EVENTUALLY BE MOU_INDEX_FINAL. FOR NOW, USE MOU_INDEX_PAPER TO REPLICATE DATA.

This code generates the MOU Index, a metric for the direct comparison of soccer managers based on their relative performance in the DAVIES statistic. 

The major code can be found in MOU_Index_Paper.Rmd. This file, combined with the data in the repository, will generate the index values for managers.

When run, the total manager values of the index can be found in the ordered dataframe managers_index_all_seasons.

The manager values separated by position group can be found under the "corrected_index" variable in the dataframe man3. The code is inefficient but successfully generates these values. It will be tidied for proceding versions. 
