#Download plink from: https://www.cog-genomics.org/plink/
#put plink at your working directory

# Quality Control control exercise

#start by recoding the .bed file format to .ped, and check the size
#system("plink --file blackface --make-bed --chr-set 26 --geno 0.05 --maf 0.05 --out blackfaceC")

#system("./plink --file blackface --make-bed --chr-set 26 --out blackfaceC")

####convert the data from .ped to .bed format
system("./plink --file Sheep05_724 --make-bed --chr-set 26 --out Sheep05_724")

#missing at SNP level
system("./plink --bfile Sheep05_724 --chr-set 26  --make-bed --geno 0.01 --out Sheep05_afterQC")

#minor allele frequency
system("./plink --bfile Sheep05_724 --chr-set 26  --make-bed --geno 0.050 --mind 0.05 --maf 0.010 --out xxSheep05_afterQC")

#hwe
system("./plink --bfile Sheep05_724 --chr-set 26  --make-bed --geno 0.10 --maf 0.10 --hwe 0.000000001 --out Sheep05_afterQC")

#missing at animal level
system("./plink --bfile Sheep05_724 --chr-set 26  --make-bed --geno 0.10 --maf 0.10 --mind 0.05 --out Sheep05_afterQC")


#ld-pruning

system("./plink --bfile Sheep05_afterQC --chr-set 26 --indep-pairwise 50 10 0.5 ")

system("./plink --bfile Sheep05_afterQC --extract plink.prune.in --make-bed --chr-set 26 --out Sheep05_afterQC_pruned")


#to generate structure format

system("./plink --bfile Sheep05_afterQC_pruned --recode-structure --chr-set 26 --out Sheep05_afterQC_pruned_structure")


#Note: For the admixture, you can still use the .bed format

#To generate other population genetics parameters:

#To generate the MAF from bed file: 
system("./plink --bfile Sheep05_afterQC  --chr-set 26  --freq --out Sheep05_afterQC_maf") 

#To generate the PCA: 
system("./plink --bfile Sheep05_afterQC_pruned  --chr-set 26  --pca --out Sheep05_afterQC_pruned_pca ")

# To generated expected and observed heterozygosity, and hwe: 
system("./plink --bfile Sheep05_afterQC --hardy  --chr-set 26   --out Sheep05_afterQC_het") 

#To generate coefficient of inbreeding: 
system("./plink --bfile Sheep05_afterQC --chr-set 26 --het --out Sheep05_afterQC_Fx")




 