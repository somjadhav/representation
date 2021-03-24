Easiest Way to View the Content: Download the html file and open it in your favorite browser.

Last fall, in the midst of the most heated election in American history, I completed a data analysis and visualization project on the Electoral College (EC). What motivated me to do this was the dialogue surrounding the EC. For years, liberals have claimed that the EC is an unfair system that has a bias towards rural areas, and therefore favors Republicans. Conservatives on the other hand, respond that the institution protects rural areas. I wanted to understand what was true and what was not.

To get an understanding of how the allocation of votes occurs, the first thing I did was predict the composition of the EC for the 2020s. As it turns out, each state’s EC votes is equal to its number of House and Senate seats, so all I had to do was simulate the apportionment of House seats after the 2020 Census (and then add 2 to each state to get the number of EC votes). To do this, first I forecasted population changes until 2020. After that, I used the Huntington-Hill method to calculate how many House seats each state would get and then accounted for Senate seats and DC’s votes. To make the analysis easier to understand, I visualized all of this data using a color-coded map of the United States.

To analyze how this rule favored some states over others, the next step was to allocate all 538 EC votes proportionally. I did this for all years going back to 1960. I visualized this data, then calculated the difference between each state’s actual EC votes for each decade and their ‘theoretical’ number of votes (referring to the # of votes each state would get if all 538 votes were allocated perfectly proportionally); I also mapped this data onto a US state map. 

To quantify each state’s advantage or disadvantage, I calculated each state’s population per actual electoral vote and plotted that, using a red-green scale to give visual cues. I then compared the results to the population per ‘theoretical’ electoral vote to see if there was any widespread unfairness (there was not). 

However, it was still difficult to quantify the advantage one state had over another, so I divided each state’s population per electoral vote (PPEV) by the smallest PPEV out of all the states (I did this in each decade for both actual EC votes and ‘theoretical’ EC votes). This normalized/standardized the data, making it easier to calculate a state’s voting power relative to another state. Just as before, I plotted this data onto a US map and created a sliding scale to see the results over time.

At the end, I looked at the 2000 Presidential election to see if the EC's disproportionate allocation of votes affected the results of any close presidential elections; using both the actual vote counts and the theoretical vote counts, I found that Gore would have won the election if votes were allocated perfectly proportionally. 

All in all, liberals have a point when they say that the EC favors Republicans. As we saw, the EC favors states with small populations, and a majority of those states are solidly Republican. Republicans have won 2 elections in the past 20 years even when they lost the popular vote and have only won the popular vote once in the past 30 years. However, Democrats do get the advantage that comes from the winner-take-all system (with the exception of Nebraska and Maine) since more big states (talking about population here) vote Democratic. It is not clear as of yet whether the two forces cancel each other out.
