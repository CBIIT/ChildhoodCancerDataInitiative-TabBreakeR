#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - Tab BreakeR v1.0.0


##################
#
# USAGE
#
##################

#This takes a CCDI Metadata template file as input and creates an output TSV file for each of the Metadata tabs.

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI-TabBreakeR.R --help

##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("dplyr","tidyr","readr","stringi","janitor","openxlsx","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(readr,verbose = F))
suppressMessages(library(tidyr,verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(openxlsx,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))

#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="A validated CCDI submission template workbook file (.xlsx)", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI-TabBreakeR v1.0.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)){
  print_help(opt_parser)
  cat("Please supply the input file (-f), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}

#Data file pathway
file_path=file_path_as_absolute(opt$file)

#A start message for the user that the validation is underway.
cat("The data file is being broken down to single tsv submissions per tab.\n")


###############
#
# Start write out
#
###############

#Rework the file path to obtain a file name, this will be used for the output file.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output folder name based on input file name and date/time stamped.
output_folder=paste(file_name,
                  "_TabBreak",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")

dir.create(path = paste(path,output_folder,sep = ""), showWarnings = FALSE)


##############
#
# Read in each tab and apply to a data frame list
#
##############

#Read in Dictionary page to obtain the required properties.
df_dict=suppressMessages(read.xlsx(xlsxFile = file_path,sheet = "Dictionary"))
df_dict=remove_empty(df_dict,c('rows','cols'))
#Pull out nodes to read in respective tabs
dict_nodes=unique(df_dict$Node)

# A bank of NA terms to make sure NAs are brought in correctly
NA_bank=c("NA","na","N/A","n/a")

#Establish the list
workbook_list=list()

#read in readme file to establish the version
df_readme=suppressMessages(read.xlsx(xlsxFile = file_path,sheet = "README and INSTRUCTIONS"))
github_col=grep(pattern = "github", x = df_readme)
github_rows=grep(pattern = "github", x = df_readme[,github_col])
github_vers=basename(df_readme[github_rows,github_col])
github_curr_ver=github_vers[length(github_vers)]


df_study=suppressMessages(read.xlsx(xlsxFile = file_path,sheet = "study"))
project_id=df_study$study_id[1]

#create a list of all node pages with data
for (node in dict_nodes){
  #read the sheet
  df=readWorkbook(xlsxFile = file_path,sheet = node, na.strings = NA_bank)
  
  df$type=node
  
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #Capture the version of the template in the data frames
  df$template_version=github_curr_ver
  df$project_id=project_id
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    #if the only columns in the resulting data frame are only linking properties (node.node_id), do not add it.
    if (any(!grepl(pattern = "\\.",x = colnames(df_empty_test)))){
      #add the data frame to the workbook
      workbook_list=append(x = workbook_list,values = list(df))
      names(workbook_list)[length(workbook_list)]<-node
    }
  }
}

nodes_present=names(workbook_list)


#################
#
# Write out of individual files
#
################

#create new name for internal files
acl=workbook_list["study"][[1]]["acl"][[1]][1]
if (!is.na(acl)){
  acl=gsub(pattern = "\\[\\'",replacement = "",x = acl)
  acl=gsub(pattern = "\\'\\]",replacement = "",x = acl)
}else{
  acl="phsxxxxxx"
}

#for each tab, write out a file to the output directory
for (node in nodes_present){
  df=workbook_list[node][[1]]
  output_file=paste(path,
                    output_folder,
                    "/",
                    acl,
                    "-",
                    node,
                    stri_replace_all_fixed(
                      str = Sys.Date(),
                      pattern = "-",
                      replacement = ""),
                    ".tsv",
                    sep = "")
  
  write_tsv(x = df,file = output_file, na="")
}

cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,output_folder,"\n\n",sep = "")) 
