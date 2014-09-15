(* ::Package:: *)

(* Wolfram Language Package *)


BeginPackage["HokahokaW`Graphics`"];


(* ::Subsubsection:: *)
(*HHImageMean*)


HHImageMean::usage="Gives the mean of a series of images. Image data must have the same dimensions and depths.";


(* ::Subsubsection:: *)
(*HHImageStack*)


HHGraphicsColumn::usage="Stacks images vertically. In contrast to the standard GraphicsColumn, adjusts widths to be equal.";


(* ::Subsection:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsubsection::Closed:: *)
(*HHImageMean*)


HHImageMean[x:{__Image}]:=
Module[{tempImageData},
	tempImageData=ImageData /@ x;
	If[ Length[Union[  Dimensions/@tempImageData ]]!=1,
		Message[HHImageMean::dimensionsMustBeSame];,
		Image[ Mean[tempImageData] ]
	]
];
HHImageMean::dimensionsMustBeSame = "Input list of Image objects must all have the same dimensions and color depths!";


HHImageMean[args___]:=Message[HHImageMean::invalidArgs, {args}];


(* ::Subsubsection:: *)
(*HHGraphicsColumn*)


HHGraphicsColumn[list:{__}]:= Module[{tempPlotRange, tempPlotWidth, tailHeightAccumulate},

	tempPlotRange = HHOptionValue[list[[1]],PlotRange];

	If[Dimensions[tempPlotRange]!={2,2},
		Message[ HHGraphicsColumn::headNotGraphicsObject, list[[1]] ],
		If[ Length[list]==1,
			list[[1]],
			tempPlotWidth = tempPlotRange[[1,2]]-tempPlotRange[[1,1]];
			tailHeightAccumulate = Accumulate[tempPlotWidth * (HHOptionValue[#, AspectRatio]& /@ list[[2;;]])];

			Graphics[
				Prepend[
					Table[ Inset[list[[n]], 
							{tempPlotRange[[1,1]], tempPlotRange[[2,1]]-tailHeightAccumulate[[n-1]]},
							{Left, Bottom}, tempPlotWidth ],
						{n,2,Length[list]}
					],
					Inset[list[[1]],{tempPlotRange[[1,1]], tempPlotRange[[2,1]]},{Left, Bottom}, tempPlotWidth] 
				],
				PlotRange->{tempPlotRange[[1]],
					  {tempPlotRange[[2,1]]-tailHeightAccumulate[[-1]],
						tempPlotRange[[2,1]]+tempPlotWidth*HHOptionValue[list[[1]],AspectRatio]}}
			]
		]
	]
];
HHGraphicsColumn::headNotGraphicsObject="The first list element `1` must be a Graphics object with a PlotRange specification!";


(* ::Subsection:: *)
(*Ending*)


End[];


EndPackage[];
