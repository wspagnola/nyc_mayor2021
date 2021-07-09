# NYC 2021 Mayoral Election: Democratic Primary

## Scripts 

#### data_extract.R
1. Pulls data from NYC Board of Elections (BOE)
2. Creates main file with data from each round
3. Creates rcv_links.csv and rcv.nodes.csv with data from rounds 6-8 for network Sankey Diagram

#### nyc_mayor_sankey.R
1. Creates interactive of rounds 6-8 of the Ranked Choice Voting (RCV) results 

## Notes
1. Results are preliminary though data should be updated as soon NYC BOE updates their website
2. Only includes rounds 6- 8 because more than one candidate was eliminated in round 5.  Currently, impossible to tell how these votes were reallocated. 

## To - Do List
1. Create networkSankey tutorial
2. Write about RCV process
