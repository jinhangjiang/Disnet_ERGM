/******************************************************************/
/* Title:       Chronic Condition Indicator Load Program          */
/*                                                                */
/* Program:     CCI_ICD10CM_Mapping_Program_v2021-1.sas           */
/*                                                                */
/* Description: This is the SAS mapping program to add the        */
/*              Chronic Condition Indicator (CCI) flags to        */
/*              the user’s ICD-10-CM-coded data.                  */
/*                                                                */
/*              There are two general sections to this program:   */
/*                                                                */
/*              1) The first section creates a temporary SAS      */
/*                 format using the Chronic Condition Indicator   */
/*                 tool file. This format is used in step 2 to    */
/*                 create the Chronic Condition Indicator         */
/*                 variables.                                     */
/*              2) The second section loops through the diagnosis */
/*                 array in your SAS dataset and creates an array */
/*                 of Chronic Condition Indicator flags on the    */
/*                 output file.                                   */
/*                                                                */
/******************************************************************/


/*******************************************************************/
/*      THE SAS MACRO FLAGS BELOW MUST BE UPDATED BY THE USER      */ 
/*  These macro variables must be set to define the locations,     */
/*  names, and characteristics of your input SAS formatted data.   */
/*******************************************************************/

/**************************************/
/*          FILE LOCATIONS            */
/**************************************/
FILENAME INRAW1  'c:\ccs\cci_icd10cm_v2021-1.csv';                 * Location of Chronic Condition Indicator tool file. <===USER MUST MODIFY;
LIBNAME  IN1     'c:\sasdata\';                                    * Location of input discharge data.                  <===USER MUST MODIFY;
LIBNAME  OUT1    'c:\sasdata\';                                    * Location of output data.                           <===USER MUST MODIFY;


/**************************************/
/*            FILE NAMES              */
/**************************************/  
* Input SAS file member name;                                      %LET CORE = YOUR_SAS_INPUT_FILE_HERE;           *<===USER MUST MODIFY;
* Output SAS file member name;                                     %LET OUT1 = YOUR_SAS_OUTPUT_FILE_HERE;          *<===USER MUST MODIFY;


/**************************************/
/*   INPUT FILE CHARACTERISTICS       */
/**************************************/ 
* Specify the prefix used to name the ICD-10-CM
  diagnosis data element array in the input 
  dataset. ;                                                       %LET DXPREFIX=I10_DX;                       *<===USER MUST MODIFY;

* Specify the maximum number of diagnosis codes
  on any record in the input file. ;                               %LET NUMDX = 15;                            *<===USER MUST MODIFY;

* Set the number of observations to use from 
  your dataset (use MAX for all observations,
  other values for testing);                                       %LET OBS    = MAX;                          *<===USER MAY MODIFY; 


TITLE1 'CREATE CHRONIC CONDITION INDICATOR FLAGS';
TITLE2 'USE WITH ADMINISTRATIVE DATA THAT HAVE ICD-10-CM DIAGNOSIS CODES';


%macro cci;
%if &NUMDX > 0 %then %do; 
options obs=max;

/*******************  SECTION 1: CREATE INFORMAT   ****************/
/*  SAS Load the Chronic Condition Indicator tool and convert     */
/*  it into a temporary SAS format used to assign the             */
/*  Chronic Condition Indicator variables in the next step.       */
/******************************************************************/
data cci;
   infile INRAW1 dsd dlm=',' firstobs=4 end=eof;
   input
      start       : $char7.    
      code_label  : $char224.
      label       : $1.
      ;

   retain hlo " ";
   fmtname = "$cci" ;
   type    = "c" ;
   output ;
   if eof then do ;
      start = " " ;
      label = " " ;
      hlo   = "o" ;
      output ;
   end ;
run;

proc format lib=work cntlin = cci;
run;


options obs=&obs.;

/******************* SECTION 2: CREATE CHRONIC CONDITION INDICATOR FLAGS ******************/
/*  Create Chronic Condition Indicator flags for ICD-10-CM diagnosis codes using          */
/*  the SAS format created in Step 1 and the SAS input file you wish to augment. Users    */
/*  can change the names of the output Chronic Condition Indicator variables if needed    */
/*  here. The ICD-10-CM diagnosis code variable names use the prefix specified at the     */
/*  top of the program.                                                                   */
/******************************************************************************************/  
data out1.&OUT1. (drop = i);  
   set in1.&CORE.;

   array ccis  (*)   $1 CCI1-CCI&NUMDX;                             * Name for Chronic Condition Indicator variables.        <===USER MAY MODIFY;
   array dxs   (*)   $ &DXPREFIX.1-&DXPREFIX.&NUMDX;                * Name of Diagnosis variables on your file using 
                                                                     the prefix specified at the top of the program;        
 
   /***************************************************/
   /*  Loop through the ICD-10-CM diagnosis code      */
   /*  array on your SAS dataset and create the       */
   /*  Chronic Condition Indicator variables.         */
   /***************************************************/
   do i = 1 to &NUMDX;
      ccis(i) = put(dxs(i),$cci.);
   end;  

   %do i = 1 %to &NUMDX;
      label CCI&i. = "Chronic Condition Indicator &i.";    * Labels for CCI Variables                               <===USER MAY MODIFY;    
   %end;
run;

proc print data=out1.&OUT1. (obs=20);                            * Change the number of observations if you wish to display more or less than 10.   <=== USER MAY MODIFY;
   var &DXPREFIX.1-&DXPREFIX.&NUMDX CCI1-CCI&NUMDX.  ;
   TITLE3 "PARTIAL PRINT OF THE OUTPUT CHRONIC CONDITION INDICATOR FILE";
run;
%end;
%else %do;
   %put;
   %put 'ERROR: NO DIAGNOSIS CODES SPECIFIED FOR MACRO VARIABLE NUMDX, PROGRAM ENDING';
   %put;
%end;

%mend cci;
%cci;
