# ChildrensCancerDataInitiative-TabBreakeR
This takes a CCDI Metadata template file as input and creates an output TSV file for each of the Metadata tabs.

This R Script takes a validated data file that is formatted to the [submission template for CCDI](https://github.com/CBIIT/ccdi-model/tree/main/metadata-manifest) as input.

To run the script on a validated CCDI template, run the following command in a terminal where R is installed for help.

```
Rscript --vanilla CCDI-TabBreakeR.R -h
```

```
Usage: CCDI-TabBreakeR.R [options]

CCDI-TabBreakeR v1.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		A validated CCDI submission template workbook file (.xlsx)

	-h, --help
		Show this help message and exit
```

To test the script on one of the provided test files:

```
Rscript --vanilla CCDI-TabBreakeR.R -f test_files/a_with_acl_CCDI_Submission_Template_v1.0.1.xlsx 
```

```
The data file is being validated at this time.


Process Complete.

The output file can be found here: ChildrensCancerDataInitiative-TabBreakeR/test_files/a_with_acl_CCDI_Submission_Template_v1.0.1_TabBreak20221212
```
