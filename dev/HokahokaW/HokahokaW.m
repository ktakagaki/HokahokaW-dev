(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Sep 11, 2014 *)

BeginPackage["HokahokaW`"]
(* Exported symbols added here with SymbolName::usage *) 


General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";


(* ::Subsection:: *)
(*Git and date messages*)


HHNewestFileDate::usage="Returns newest file date in package.";
HHGitRemotes::usage="Prints a list of git remotes for either the given directory or the current NotebookDirectory[]";


$PackageDirectoryNounouW = ParentDirectory[DirectoryName[FindFile["NounouW`"]]];
$PackageNewestFileDateNounouW = DateString[Max @@ AbsoluteTime /@ FileDate /@ FileNames[ "*",$PackageDirectoryNounouW,Infinity] ];
$GitCurrentHeadNounouW = Module[{tempretNN},
	SetDirectory[ ParentDirectory[DirectoryName[ FindFile["NounouW`"] ]] ];
	Run["git rev-parse HEAD > GitCurrentHEADHash.txt"];
	tempretNN = Import["GitCurrentHEADHash.txt"];
	ResetDirectory[];
	tempretNN
];


Begin["`Private`"]
(* Implementation of the package *)



$Path


(* ::Subsection:: *)
(*Git and date messages*)


HHNewestFileDate[package_String]:=DateString[Max @@ AbsoluteTime /@ FileDate /@ FileNames[ "*",package,Infinity] ];
HHNewestFileDate[args___]:=Message[HHNewestFileDate::invalidArgs,{args}];


HHGitRemotes[package_String]:=
Module[{tempret},
	SetDirectory[ ParentDirectory[DirectoryName[ FindFile[package] ]] ];
	Run["git remote -v > HHTempGitRemotes.txt"];
	tempret= Import["HHTempGitRemotes.txt"];
	DeleteFile["HHTempGitRemotes.txt"];
	ResetDirectory[];
	tempret
];
HHGitRemotes[args___]:=Message[HHGitRemotes::invalidArgs,{args}];


End[]

EndPackage[]

