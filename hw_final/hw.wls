#!/usr/bin/env wolframscript
(* ::Package:: *)

f[x_, y_] = 7 * x^2 + 3 * x^4 + 28 * x * y + 14 * y^2 +12 * x^2 * y^2 + 11 * y^4;


g1[x_, y_] = 1 - x^2 -x^4 - 4 * x^2 * y - 4 * y^2 - 4 * x * y^3 - 2 * y^6;
g2[x_, y_] = 2 - 2 * x^2 - 5 * x^6 -8 * x * y^2 + 20 * x^3 * y^3 - 8 * y^4 -12 * y^6;
g3[x_, y_] = 1 - x^2 - x^4 - 4 * x^2 * y^2 - 4 * x * y^3 - 2 * y^4 - y^6;


candidates = Solve[{D[f[x, y], x] == 0, D[f[x, y], y] == 0}, {x, y}, Reals];


eigenSumList = {};
For[
	i = 1, i <= Length[candidates], i++,
	a11 = N[D[f[x, y], {x, 2}] /. candidates[[i]], 20];
	a12 = N[D[f[x, y], x, y] /. candidates[[i]], 20];
	a21 = N[D[f[x, y], y, x] /. candidates[[i]], 20];
	a22 = N[D[f[x, y], {y, 2}] /. candidates[[i]], 20];
	mat = {{a11, a12}, {a21, a22}};
	eigenSumList = Append[eigenSumList, Total[Sign /@ Eigenvalues[mat]]];
]
newCandidates = {};
For[
	i = 1, i <= Length[candidates], i++,
	newCandidates = If[eigenSumList[[i]] == 2, Append[newCandidates, candidates[[i]]], newCandidates];
]
candidates = newCandidates;	


L1[x_, y_, l_] = f[x, y] - l * g1[x, y];
L2[x_, y_, l_] = f[x, y] - l * g2[x, y];
L3[x_, y_, l_] = f[x, y] - l * g3[x, y];

condCandidates = {};
condCandidates = Join[
	condCandidates,
	Solve[{D[L1[x, y, l], x] == 0, D[L1[x, y, l], y] == 0, D[L1[x, y, l], l] == 0}, {x, y}, Reals]
];
condCandidates = Join[
	condCandidates,
	Solve[{D[L2[x, y, l], x] == 0, D[L2[x, y, l], y] == 0, D[L2[x, y, l], l] == 0}, {x, y}, Reals]
];
condCandidates = Join[
	condCandidates,
	Solve[{D[L3[x, y, l], x] == 0, D[L3[x, y, l], y] == 0, D[L3[x, y, l], l] == 0}, {x, y}, Reals]
];

candidates = Join[candidates, condCandidates];

candidates = Cases[candidates, {xr_, yr_} /; g1[x /. xr, y /. yr] >= 0];
candidates = Cases[candidates, {xr_, yr_} /; g2[x /. xr, y /. yr] >= 0];
candidates = Cases[candidates, {xr_, yr_} /; g3[x /. xr, y /. yr] >= 0];


g12List = Solve[{g1[x, y] == 0, g2[x, y] == 0}, {x, y}, Reals];
g13List = Solve[{g1[x, y] == 0, g3[x, y] == 0}, {x, y}, Reals];
g23List = Solve[{g2[x, y] == 0, g3[x, y] == 0}, {x, y}, Reals];

g12List = Cases[g12List, {xr_, yr_} /; g3[x /. xr, y /. yr] >= 0];
g13List = Cases[g13List, {xr_, yr_} /; g2[x /. xr, y /. yr] >= 0];
g23List = Cases[g23List, {xr_, yr_} /; g1[x /. xr, y /. yr] >= 0];

candidates = Join[candidates,g12List];
candidates = Join[candidates,g13List];
candidates = Join[candidates,g23List];
Length[candidates]


results = {};
For[
	i = 1, i <= Length[candidates], i++,
	results = Append[results, f[x, y] /. candidates[[i]]];
]
Print[N[Min[results], 20]]




