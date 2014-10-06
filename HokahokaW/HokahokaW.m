(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Sep 11, 2014 *)

BeginPackage["HokahokaW`",{"JLink`"}]
(* Exported symbols added here with SymbolName::usage *) 


General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";


(* ::Subsubsection::Closed:: *)
(*Git and date messages*)


HHNewestFileDate::usage="Prints the newest file change date for all files within the given package directory.";
HHGitRemotes::usage="Prints a list of git remotes for either the given package or the current NotebookDirectory[]";
HHGitHEADHash::usage="Prints the git HEAD hash for either the given directory or the current NotebookDirectory[]";
HHPackageMessage::usage"Prints standard package message.";
HHNotebookMessage::usage"Prints standard notebook message.";


(* ::Subsubsection::Closed:: *)
(*Rule List and Option Handling*)


HHRuleListQ::usage=
	"HHRuleListQ[ruleList_List]... returns whether the argument ruleList is a list of rules.";
HHRuleQ::usage=
	"HHRuleQ[rule_]... returns whether the argument ruleList is a list of Rule or RuleDelayed objects.";


HHJoinOptionLists::usage=
"HHJoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y]]...   joins the two option lists, "<>
"and if an option specified in x is repeated in y, the specification in y is dropped.

HHJoinOptionLists[x_/;RuleQ[x], y_/;RuleQ[y], z_/;RuleQ[z]]...    joins the three option lists, "<>
"and if an option specified in x is repeated in y, the specification in y is dropped, etc.

HHJoinOptionLists[symbol_Symbol, x_/;RuleQ[x], y_/;RuleQ[y],... ]...    Does the "<>
"same as above, but filters the rules for Option[Symbol] before returning.";


HHAddOptions::usage=
"HHAddOptions[object, opts]...     returns the original object (e.g. NNMData[<<>>, opts]), "<>
"but with the specified option(s) appended or replaced. opts can be specified either as a Sequence "<>
"or a List of rules (i.e., brackets {opts} are optional).";


HHOptionValue::usage= "Can be used to extract options from an object, such as a Graphic[..., opt->optval]."
HHAbsoluteOptionValue::usage= "Can be used to extract absolute options from an object, such as a Graphic[..., opt->optval]."


(* ::Subsubsection::Closed:: *)
(*HHPadZeros*)


HHPadZeros::usage =
"HHPadZeros[n] gives the numeral n string padded to 3 digits with zeros. " <>
"HHPadZeros[n,m] gives the numeral n string padded to m digits with zeros.";


(* ::Subsubsection::Closed:: *)
(*HHFunctionQ*)


NNFunctionQ::usage=
"returns whether a given symbol is a pure function, or a function rule.";


(* ::Subsubsection::Closed:: *)
(*HHJavaObjectQ*)


HHJavaObjectQ::usage="Checks whether something is a Java object and an instance of the given class/interface.";


(* ::Subsubsection:: *)
(*HHMedianStandardDeviation*)


HHStandardDeviationMedianEstimate::usage="Makes a standard deviation estimate based on medians (less sensitive to outliers).";


(* ::Subsubsection:: *)
(*HHThreshold*)


HHThreshold::usage="Takes a trace and returns timepoints at which it crosses threshold.";


HHThresholdLevel::usage="What level to use for threshold.";
HHThresholdDirection::usage="What direction to use for threshold.";


Options[HHThreshold]={HHThresholdLevel->Automatic, HHThresholdDirection->Automatic};


(* ::Subsubsection:: *)
(*HHDetectTrain*)


HHDetectTrain::usage="Detect a train.";


HHTrainPulseLengthMinimum::usage="Number of different pulse lengths to accept.";
HHTrainBlackout::usage="Number of samples from beginning (or {beginning, end}) to reject for thresholding.";


Options[HHDetectTrain]={HHTrainPulseLengthMinimum->0, HHTrainBlackout->None};


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*Git and date messages*)


HHNewestFileDate[package_String]:=
Module[{tempdir},
	tempdir = FileNames[ "*",
				ParentDirectory[DirectoryName[ FindFile[package] ]],
				Infinity
				];
	If[ Length[tempdir]==0,
		Message[HHNewestFileDate::noFilesFound, package];
		" ",
		DateString[Max @@ AbsoluteTime /@ FileDate /@ tempdir ]
	]
];

HHNewestFileDate[args___]:=Message[HHNewestFileDate::invalidArgs,{args}];

HHNewestFileDate::noFilesFound = "No files were found for package string `1`.";


HHGitRemotes[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHGitRemotes::noFilesFound, package];
		" ",
		HHGitRemotesImpl[ ParentDirectory[DirectoryName[ tempFile ]] ]
	]
];
HHGitRemotes[]:= HHGitRemotesImpl[ NotebookDirectory[] ];

HHGitRemotesImpl[directory_String]:=
Module[{tempret},
	SetDirectory[ directory ];
	Run["git remote -v > HHTempGitRemotes.txt"];
	tempret= Import["HHTempGitRemotes.txt"];
	DeleteFile["HHTempGitRemotes.txt"];
	ResetDirectory[];
	tempret
];

HHGitRemotes[args___]:=Message[HHGitRemotes::invalidArgs,{args}];

HHGitRemotes::noFilesFound = HHNewestFileDate::noFilesFound;


HHGitHEADHash[package_String]:= Module[{tempFile},
	tempFile=FindFile[package];
	If[ tempFile === $Failed,
		Message[HHGitHEADHash::noFilesFound, package];
		" ",
		HHGitHEADHashImpl[ ParentDirectory[DirectoryName[ tempFile ]] ]
	]
];
HHGitHEADHash[]:= HHGitHEADHashImpl[ NotebookDirectory[] ];

HHGitHEADHashImpl[directory_String]:=
Module[{tempret},
	SetDirectory[ directory ];
	Run["git rev-parse HEAD > HHTempGitCurrentHEADHash.txt"];
	tempret= Import["HHTempGitCurrentHEADHash.txt"];
	DeleteFile["HHTempGitCurrentHEADHash.txt"];
	ResetDirectory[];
	tempret
];

HHGitHEADHash[args___]:=Message[HHGitHEADHash::invalidArgs,{args}];

HHGitHEADHash::noFilesFound = HHNewestFileDate::noFilesFound;


HHPackageMessage[package_String]:=
Module[{remotes},
	remotes=HHGitRemotes[package];
	If[ remotes === Null || remotes==" " || remotes=="",
		Message[HHPackageMessage::remoteNotFound, remotes];,
		CellPrint[TextCell[Row[{
			Style[package, FontWeight -> "Bold", FontVariations -> {"Underline" -> True}], "\n" ,
			Style[StringJoin@@Riffle[
					"("<> #[[1]]<>")[" <> #[[2]] <>"]"& /@
					Union[ImportString[remotes][[All, 1;;2]]]
				,"\n"],Small, FontFamily->"Courier"], 
		"\n",
		Style[
			"current Git HEAD:  "<> HHGitHEADHash[package]<>"\n" <>
			"newest file:  "<> HHNewestFileDate[package]<>" ", Small, FontFamily->"Courier"]
		}],"Text", Background -> LightGray]]
	]
];
HHPackageMessage::remoteNotFound="HHGitRemotes result \"`1`\" is not a valid remote list, the package is not tracked with git, or was probably not found.";


HHNotebookMessage[]:=
CellPrint[TextCell[Row[{
Style[NotebookFileName[], FontWeight -> "Bold", FontVariations -> {"Underline" -> True}], "\n" ,
Style[StringJoin@@Riffle[
					"("<> #[[1]]<>")[" <> #[[2]] <>"]"& /@
					Union[ImportString[HHGitRemotes[]][[All, 1;;2]]]
				,"\n"],Small,FontFamily->"Courier"], 
"\n",
Style[
	"current Git HEAD:  "<> HHGitHEADHash[]<>"\n" <>
	"last saved:  "<> DateString[FileDate[NotebookFileName[]]]<>" ", Small, FontFamily->"Courier"]
}],"Text", Background -> LightGray]];


(* ::Subsubsection::Closed:: *)
(*Rule List and Option Handling*)


HHRuleListQ[ruleList_List] := And @@ (HHRuleListQ /@ ruleList);
HHRuleListQ[rule_Rule] := True;
HHRuleListQ[rule_RuleDelayed] := True;
HHRuleListQ[rules__] := HHRuleListQ[{rules}];
HHRuleListQ[] := True;
HHRuleListQ[{}] := True;
HHRuleListQ[_] := False;
HHRuleListQ[args___]:=Message[HHRuleListQ::invalidArgs,{args}];


HHRuleQ[rule_Rule] := True;
HHRuleQ[rule_RuleDelayed] := True;
HHRuleQ[_] := False;
HHRuleQ[args___]:=Message[HHRuleQ::invalidArgs,{args}];


HHJoinOptionLists[x_/;HHRuleListQ[x], y_/;HHRuleListQ[y]]:=
	Module[{tempretNN},
		tempretNN=Join[Flatten[x], FilterRules[Flatten[y], Except[x]]];
		Return[tempretNN]
	];


HHJoinOptionLists[x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Module[{tempretNN},
		tempretNN=x;
		Do[tempretNN=HHJoinOptionLists[tempretNN, zz],{zz,{y}}];
		Return[tempretNN]
	];


HHJoinOptionLists[symbol_Symbol, x_/;HHRuleListQ[x]]:=
	Module[{},
		Return[FilterRules[x, Options[symbol]]]
	];


HHJoinOptionLists[symbol_Symbol, x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Module[{tempretNN},
		tempretNN = HHJoinOptionLists[x, y];
		Return[FilterRules[tempretNN, Options[symbol]]]
	];


HHJoinOptionLists[symbol_[contents_], x_/;HHRuleListQ[x], y__/;(And@@(HHRuleListQ /@ {y}))]:=
	Module[{tempretNN},
		tempretNN = HHJoinOptionLists[x, y];
		Return[FilterRules[tempretNN, Options[symbol]]]
	];


HHJoinOptionLists[args___]:=Message[HHJoinOptionLists::invalidArgs,{args}];


HHAddOptions[symbol_[contents___], opts___]:=HHAddOptions[symbol[contents], {opts}];
HHAddOptions[symbol_[contents___], {opts___}]:=
	Module[{tempretNN(*,oldRules*)},
		(*The following will strip off rules from the end.*)
		tempretNN = Select[{contents}, !HHRuleQ[#]&];
		(*Append old rules which are not given in opts.*)

		tempretNN = Append[tempretNN, Hold[Sequence@@HHJoinOptionLists[symbol, {opts}, Select[{contents}, RuleQ]]] ];
		Return[ReleaseHold[ symbol[Sequence@@tempretNN] ]]
		(*oldRules=FilterRules[Options[x], Except[opts]];
			If[Length[oldRules]>0, tempretNN= Append[tempretNN, Hold[Sequence@@oldRules]]];
		(*Append new rules given in opts*)
		tempretNN= Append[tempretNN, Hold[opts]];
		Return[ReleaseHold[tempretNN]]*)
	];

HHAddOptions[args___]:=Message[HHAddOptions::invalidArgs,{args}];


(*HHExtractRules[x_[arg___]]:=Flatten[If[HHRuleQ[#],#,{}]& /@ {arg}];
HHExtractRules[args___]:=Message[HHExtractRules::invalidArgs,{args}];*)
HHOptionValue[x_, optionSymbol_]:=Module[{tempOpts},
	tempOpts=Join[Options[x],Options[Head[x]]];
	OptionValue[ tempOpts ,optionSymbol ]
];
HHOptionValue[args___]:=Message[HHOptionValue::invalidArgs,{args}];

HHAbsoluteOptionValue[x_, optionSymbol_]:=Module[{tempOpts},
	tempOpts=Join[AbsoluteOptions[x],Options[Head[x]]];
	OptionValue[ tempOpts ,optionSymbol ]
];


(* ::Subsubsection::Closed:: *)
(*HHPadZeros*)


HHPadZeros[n_]:=HHPadZeros[n,3];
HHPadZeros[n_,m_]:=Apply[StringJoin,Map[ToString,IntegerDigits[n, 10, m] ]];


HHPadZeros[args___]:=Message[HHPadZeros::invalidArgs,{args}];


(* ::Subsubsection::Closed:: *)
(*HHFunctionQ*)


(*Tests whether the symbol is a function or not*)
(*FunctionQ[x_String]:=FunctionQ[ToExpression[x]];*)
HHFunctionQ[x_Function]:=True;
HHFunctionQ[x_Symbol]:=MemberQ[Attributes[x],NumericFunction] || (Length[Flatten[#[x]&/@{DownValues,UpValues}]]>0);
HHFunctionQ[x_, sampleArgs_]:=HHFunctionQ[x, sampleArgs, NumericQ];
HHFunctionQ[x_, sampleArgs_, questionFunc_]:=Quiet[Check[questionFunc[  x @@ sampleArgs],False]];
HHFunctionQ[_]:=False;


HHFunctionQ[args___]:=Message[HHFunctionQ::invalidArgs, {args}];


(* ::Subsubsection::Closed:: *)
(*HHJavaObjectQ*)


HHJavaObjectQ[x_/;JavaObjectQ[x]]:= True;
HHJavaObjectQ[x_/;JavaObjectQ[x], className_String]:= InstanceOf[x, className];
HHJavaObjectQ[___]:= False;


(* ::Subsubsection:: *)
(*HHMedianStandardDeviation*)


HHStandardDeviationMedianEstimate[data_List] := MedianDeviation[data]/0.6745;


(* ::Subsubsection:: *)
(*HHThreshold*)


HHThreshold[data_List, opts:OptionsPattern[]] :=  HHThreshold[data, Automatic, opts];


HHThreshold[data_List, Automatic, opts:OptionsPattern[]] := HHThresholdImplSimpleAutoMedian[data];


HHThreshold[args___]:=Message[HHThreshold::invalidArgs,{args}];


HHThresholdImplSimpleAutoMedian[data_List]:=
Module[{tempData, tempThresh, tempRes},
	HHThresholdImplSimple[
		tempData=data-Median[data],
		HHStandardDeviationMedianEstimate[tempData]*4
	]
];


HHThresholdImplSimple[data_List, threshValue_]:=
Module[{tempRes},
	tempRes=FoldList[Plus, 1, Length /@ Split[If[# < threshValue, 0, 1]& /@ data]];
	tempRes=If[ Length[tempRes]>2 && data[[1]]>threshValue, tempRes[[3 ;; ]], tempRes[[2 ;; ]] ];
	tempRes=If[ Length[tempRes]>2 && data[[-1]]>threshValue, tempRes[[ ;; -3]], tempRes[[ ;; -2]] ];
	tempRes	
];


(* ::Subsubsection:: *)
(*HHDetectTrain*)


HHDetectTrain[data_List, opts:OptionsPattern[]] := HHDetectTrain[data, Automatic, opts]


HHDetectTrain[data_List, Automatic, opts:OptionsPattern[]] := 
Module[{threshed, tempRes,
		optBlackout,optPulseMin},

	threshed=HHThreshold[data, Automatic];
	If[ Length[threshed]==0, Message[HHDetectTrain::noThresholdCrosses]];
	If[ !EvenQ[Length[threshed]], Message[HHDetectTrain::oddThresholdCounts]];
	tempRes= (#-{0,1})& /@ Partition[threshed,2];
	
	optBlackout = OptionValue[HHTrainBlackout];
	If[ Head[optBlackout] === List && Length[optBlackout]==2,
		tempRes = Select[ tempRes, (#[[2]]<optBlackout[[1]] && #[[1]]>optBlackout[[2]])&],
		If[ Head[optBlackout]===Integer || Head[optBlackout] ===Real,
			tempRes = Select[ tempRes, (#[[2]]>optBlackout)&]
	]];
	
	optPulseMin = OptionValue[HHTrainPulseLengthMinimum];
	If[ (Head[optPulseMin] === Integer || Head[optPulseMin] === Real),
		If[ optPulseMin >= 1, tempRes = Select[ tempRes, (#[[2]]-#[[1]] >= optPulseMin)& ] ]
	];

	tempRes
	
];


HHDetectTrain::noThresholdCrosses="No threshold crosses detected";
HHDetectTrain::oddThresholdCounts="Some error, odd number of thresholds should not occur with HHThreshold[]";


HHDetectTrain[args___]:=Message[HHDetectTrain::invalidArgs,{args}];


(* ::Subsection:: *)
(*Ending*)


End[];


HHPackageMessage["HokahokaW`"];


(*CellPrint[TextCell[Row[{
Style["HokahokaW  (http://github.org/ktakagaki/HokahokaW)", 
    FontWeight -> "Bold", FontVariations -> {"Underline" -> True}], "\n" ,
"    ( current Git HEAD:  "<> HHGitHEADHash["HokahokaW`"]<>" )\n" <>
"    ( newest file:  "<> HHNewestFileDate["HokahokaW`"]<>" )"
}],"Text"]];*)


EndPackage[]
