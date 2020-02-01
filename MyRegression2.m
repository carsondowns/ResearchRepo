(* ::Package:: *)

(*this version tries to use just forcibly connected inputs to train*)
(*total number of training records: 216+783+2843+10535+39232+147457+556859=757925*)
SetDirectory["C:\\Users\\acars\\Desktop\\CS Courses\\Research\\For_Carson_Blaine"];
featureVectorLength=6;
dataMatrix=ConstantArray[0,{757925,featureVectorLength}];(*number of columns is number of features*)
respVec=ConstantArray[0,757925];
recordLenVec={216, 999, 3842, 14377, 53609, 201066, 757925};
On[Assert];(*turn on assertion in this Mathematica session*)
RowIndex=1;
For[fileIndex=7,fileIndex<=13,fileIndex++,
	filename=ToString@StringForm["processing training seq len ``", fileIndex];
	Print[filename];
	readDegrees=ConstantArray[0,fileIndex];
	filename=ToString@StringForm["all``pf.txt", fileIndex];
	(*Print[filename];	Print[StringLength[filename]];*)
	filestr=OpenRead[filename];
	linestr=ReadLine[filestr];
	While[linestr=!=EndOfFile,(*Print[linestr];*)
	slist=StringSplit[linestr,"\t"];(*For[i=1,i<Length[slist],i++,WriteString["stdout",slist[[i]]];WriteString["stdout"," "]];WriteString["stdout","\n"];*)
	If[Length[slist]<fileIndex+2,Break[],Null];(*break out of while loop*)
	Assert[Length[slist]==fileIndex+2];
	If[slist[[fileIndex+2]]==="fc",
	(*now read in a degree sequence that is forcibly connected*)
	For[ColIndex=1,ColIndex<Length[slist]-1,ColIndex++,
		readDegrees[[ColIndex]]=ToExpression[slist[[ColIndex]]]];
	(*now read in the number of realizations for the current degree sequence*)
	respVec[[RowIndex]]=ToExpression[slist[[ColIndex]]];
	(*now set up the feature vector based on degree sequence*)
	dataMatrix[[RowIndex,1]]=readDegrees[[1]]-fileIndex/2;(*feature 1: centered largest term*)
	dataMatrix[[RowIndex,2]]=readDegrees[[fileIndex]]-fileIndex/2;(*feature 2: centered smallest term*)
	dataMatrix[[RowIndex,3]]=fileIndex;(*feature 3: degree sequence length*)
	dataMatrix[[RowIndex,4]]=Mean[readDegrees]-fileIndex/2;(*feature 4: centered average*)
	dataMatrix[[RowIndex,5]]=StandardDeviation[readDegrees]-fileIndex/4;(*feature 5: centered standard deviation*)
	dataMatrix[[RowIndex,6]]=Count[readDegrees,1];(*feature 6: number of 1s*)
	(*For[i=fileIndex,i>=1,i--,If[readDegrees[[i]]==1,dataMatrix[[RowIndex,6]]++,Break[]]];*)
	RowIndex++,
	Null];
	linestr=ReadLine[filestr]];(*end of While*)
	Close[filestr];
	filename=ToString@StringForm["done processing all``pf.txt", fileIndex];
	Print[filename];
	Assert[RowIndex==recordLenVec[[fileIndex-6]]+1];
](*end of For*)
Assert[Length[respVec]==757925];
Assert[RowIndex==757926];

(*dataMatrix//MatrixForm *)
lm = LinearModelFit[{dataMatrix, respVec}];
Print[Normal[lm]];

(*now evaluate performance*)
Print["begin to evaluate"];
readDegrees=ConstantArray[0,14];
filestr=OpenRead["all14_noisolated_36orless_fc.txt"];
linestr=ReadLine[filestr];
While[linestr=!=EndOfFile,
slist=StringSplit[linestr,"\t"];
Assert[Length[slist]==15];
(*now read in a degree sequence*)
For[ColIndex=1,ColIndex<15,ColIndex++,
	readDegrees[[ColIndex]]=ToExpression[slist[[ColIndex]]]];
(*now read in the number of realizations for the current degree sequence*)
numRealizations=ToExpression[slist[[ColIndex]]];
(*now calculate the feature vector based on degree sequence*)
featureVector=ConstantArray[0,featureVectorLength];
featureVector[[1]]=readDegrees[[1]]-7;(*feature 1: centered largest term*)
featureVector[[2]]=readDegrees[[14]]-7;(*feature 2: centered smallest term*)
featureVector[[3]]=14;(*feature 3: degree sequence length*)
featureVector[[4]]=Mean[readDegrees]-7;(*feature 4: centered average*)
featureVector[[5]]=StandardDeviation[readDegrees]-7/2;(*feature 5: centered standard deviation*)
featureVector[[6]]=Count[readDegrees,1];(*feature 6: number of 1s*)
(*For[i=14,i>=1,i--,If[readDegrees[[i]]==1,featureVector[[6]]++,Break[]]];*)
numPredictedRealizations=lm[featureVector[[1]],featureVector[[2]],featureVector[[3]],featureVector[[4]],featureVector[[5]],featureVector[[6]]];
errPercentage=1.0*(numPredictedRealizations-numRealizations)/numRealizations;
If[Abs[errPercentage]>1.0,(*Abs[errPercentage]<0.1 || Abs[errPercentage]>50,*)
For[i=1,i<=14,i++,WriteString["stdout", readDegrees[[i]], " "]];
WriteString["stdout", "npr=",TextString[numPredictedRealizations], " "];
WriteString["stdout", "nr=",TextString[numRealizations], " "];
WriteString["stdout", "ep=",TextString[errPercentage], "\n"],
For[i=1,i<=14,i++,WriteString["stdout", readDegrees[[i]], " "]];
WriteString["stdout", "NPRGG=",TextString[numPredictedRealizations], " "];
WriteString["stdout", "NRGG=",TextString[numRealizations], " "];
WriteString["stdout", "EPGG=",TextString[errPercentage], "\n"]];
linestr=ReadLine[filestr];
];(*end of While*)
(*Close[filestr];*)
Off[Assert];(*turn off assertion in this Mathematica session*)
Print["end of evaluation"];
