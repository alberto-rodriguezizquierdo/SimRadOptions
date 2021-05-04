# SimRadOptions

#Objective

SimRADOptions is a little package to find the best restriction enzyme (in a selection of more than 400 restriction enzymes) in a whole genome in FASTA format.

#Requirements

- R environment 4.0.2 and Bioconductor Software
- > 8 GB RAM
- Whole Genome of an organism (in FASTA format)

#How to use SimRADOptions

Before starting:

Please generate repository in your server or computer using git bash and command:

git clone git@github.com:alberto-rodriguezizquierdo/SimRadOptions.git

After that, it's necessary to generate directories into the repository:

-/REPOSITORY_PATH/data
-/REPOSITORY_PATH/output

Please put into the data repository the whole genome of the organism of interest.

Then, open the file /REPOSITORY_PATH/main/main.R and change the root path putting into the variable your local repository path.

Finally, run the script main.R and enjoy it!