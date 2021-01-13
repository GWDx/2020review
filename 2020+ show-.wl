(* ::Package:: *)

(* ::Subsection::Closed:: *)
(*Function*)


select2020[t_List, f_: FromUnixTime] := Select[t, First@DateList@f@# == 2020 &]


countByMonth[time_] := With[{counts = CountsBy[time, DateValue[#, "Month"] &]}, Lookup[counts, Range[12], 0]];
countByWeek[time_] := With[{counts = CountsBy[time, DateValue[#, "ISOWeekDay"] &]}, Lookup[counts, Range[7], 0]];
countByHour[time_] := With[{counts = CountsBy[time, DateValue[#, "Hour24"] &]}, Lookup[counts, Range[0, 23], 0]];
listStepPlot[data_, {l_, r_}, rules___] := Block[{d = (r - l)/48 + .5},
  	ListStepPlot[data, Center,Normal@Association@Flatten@{Ticks -> {Range[l, r], Automatic}, AxesOrigin -> {l - d, 0},
      	Filling -> Bottom, ImageSize -> Medium, PlotRange -> {{l - 1.1d, r +  d}, All},
      	GridLines -> {Range[l - .5, r + .5], None}, GridLinesStyle -> Directive[LightGray, Dashed], rules}]]
genData[ken_, {l_, r_}] := Join[{{l - 1, 0}}, Transpose@{Range[l, r], ken}, {{r + 1, 0}}];


Options[timeListStepPlot]=Flatten@{"Probability"->False,"RulesAt"->{},"ScalingAt"->{},Options[ListStepPlot]};
timeListStepPlot[time_, opts:OptionsPattern[]] := 
Block[{prob, rules, rule, dataByMonth, dataByHour, dataByWeek, normalize, yLabel, scale,scaleFunction,n,m},
n=Count[time,{___DateObject}];
prob = OptionValue["Probability"];
rule[i_]:=ReplaceList[i , Flatten@OptionValue["RulesAt"]];
scale=ReplacePart[Table[1,3,n],OptionValue["ScalingAt"]];
scaleFunction[i_,j_]:={#1,scale[[j,i]]#2}&;
rules = FilterRules[{opts},Options[ListStepPlot]];yLabel = If[prob, "\:9891\:7387", "\:9891\:5ea6"];
normalize[l_] := If[prob, If[Total@l==0, Table[0, Length@l], Normalize[l, Total]], l];

m=0;dataByMonth = time /. {a___DateObject} :>scaleFunction[1,++m]@@@genData[normalize@countByMonth@{a}, {1,12}];
m=0;dataByWeek = time /. {a___DateObject} :>scaleFunction[2,++m]@@@genData[normalize@countByWeek@{a}, {1, 7}];
m=0;dataByHour = time /. {a___DateObject} :>scaleFunction[3,++m]@@@genData[normalize@countByHour@{a}, {0, 23}];
{listStepPlot[dataByMonth, {1, 12}, AxesLabel -> {"\:6708", yLabel}, rules, rule[1]],
listStepPlot[dataByWeek, {1, 7}, AxesLabel -> {"\:661f\:671f", yLabel}, rules, rule[2]],
listStepPlot[dataByHour, {0, 23}, AxesLabel -> {"\:65f6", yLabel}, rules, rule[3]]}]


(* ::Section:: *)
(*Zhihu*)


<<(NotebookDirectory[]<>"ZhihuData.mx");


voteNames[data_,n_:4]:=SortBy[Select[First/@data//Tally,#[[2]]>=n&],Last];


zhihuYearData[2020] = Select[ZhihuData, First@DateList@FromUnixTime@ \!\(\*
TagBox[
RowBox[{
RowBox[{"#", " ", "[", "\"\<created_time\>\"", "]"}], " "}],
Short]\) == 2020 &];
zhihuYearData[2019] = Select[ZhihuData, #[[1]]==2019 && #[[2]]>6 & @DateList@FromUnixTime@ \!\(\*
TagBox[
RowBox[{"#", " ", "[", "\"\<created_time\>\"", " ", "]"}],
Short]\)  &];
voteYear[i_]:=Select[zhihuYearData[i], MemberQ[{"ANSWER_VOTE_UP","MEMBER_VOTEUP_ARTICLE"},#["verb"]]&];
voteYearData[i_]:={#["target","author","name"],FromUnixTime@#["created_time"]}&/@voteYear[i]//Reverse;
voteYearTime[i_]:=Last/@voteYearData[i];


in=Intersection@@(First/@voteNames[#1,#2]&@@@({
 {voteYearData[2019], 5},
 {voteYearData[2020], 10}
}));
highlightRule:=a_/;MemberQ[in,a]:>Highlighted[a,FrameMargins->0,Background->
ColorData["TemperatureMap"][SeedRandom@ToString@a ;RandomReal[]]]


barChart[data_,min_,label_,scale_]:=BarChart[Labeled[#2,Style[#1,4Sqrt[scale #2]//N],Before]&@@@
voteNames[data,min],BarOrigin->Left,GridLines->Automatic,ImageSize->{Automatic,330},AxesLabel->(Style[#,15]&/@{"\:7528\:6237","\:8d5e\:540c\:6570"}),PlotLabel->Style[label,20]];


(* ::Section:: *)
(*Bilibili*)


rawBilibiliData=Reverse@URLExecute["https://api.bilibili.com/x/relation/followings?vmid="<>id,"RawJSON"][["data","list"]];


dateStringFormat={"Year","-","Month","-","Day","(","DayNameShort",") ","Hour",":","Minute"};
get[i_]:=With[{a=rawBilibiliData[[i]]},{i,DateString[FromUnixTime@a[["mtime"]],dateStringFormat],Show[Import@a[["face"]],ImageSize->50],
Style[a[["uname"]],15],a[["sign"]],a[["official_verify","desc"]]}]
BilibiliData=Select[get/@Range@Length@rawBilibiliData,StringContainsQ[#[[2]],"2019"|"2020"]&];


(* ::Section:: *)
(*Go*)


SetDirectory["D:\\goreviewpartner-v0.15-Leela-Zero\\sgf"];


fileNames=FileNames["*.sgf"];
goTimeData=ToExpression@StringTake[StringDelete[#,{__~~"]",".sgf"}],10]&@FileNames@{"win\\*.sgf","lose\\*.sgf"};
goTimeData2020=FromUnixTime/@select2020@Sort@goTimeData;


select2019[t_List, f_: FromUnixTime] := Select[t, First@DateList@f@# == 2019 &]
goTimeData2019=FromUnixTime/@select2019@Sort@goTimeData;


(* ::Section:: *)
(*nb*)


<<(NotebookDirectory[]<>"nbTimeData.mx")


timeTransform[l_]:=l/.a_Real:>DateList[FromAbsoluteTime@a,TimeZone->16]
runTimeData=Flatten@Replace[nbTimeData,_List->Nothing,{3}];
runData=Select[timeTransform@runTimeData,#[[1]]==2020&];


editTimeData=Flatten@Replace[nbTimeData,{a_List:>Mean@a,_Real->Nothing},{3}];
editData=Select[timeTransform@editTimeData,#[[1]]==2020&];


nbData={DateObject/@runData,DateObject/@editData};


color={Lighter@Red,Directive[Green,Opacity@.5],Lighter@Blue};


(* ::Section:: *)
(*Mathematica*)


(* ::Subsection:: *)
(*1-23	image3.nb*)


k=Graphics3D[{EdgeForm[Opacity[0]], {FaceForm[Opacity[0.2], Opacity[0.5]],
 Triangle[{{{1 + 5^Rational[1, 2], 2, 0}, {1 + 5^Rational[1, 2], -2, 0}, {2, 0, 1 + 5^Rational[1, 2]}}, 
 {{1 + 5^Rational[1, 2], 2, 0}, {1 + 5^Rational[1, 2], -2, 0}, {2, 0, -1 - 5^Rational[1, 2]}}, 
 {{1 + 5^Rational[1, 2], 2, 0}, {0, 1 + 5^Rational[1, 2], 2}, {0, 1 + 5^Rational[1, 2], -2}}, 
 {{1 + 5^Rational[1, 2], 2, 0}, {0, 1 + 5^Rational[1, 2], 2}, {2, 0, 1 + 5^Rational[1, 2]}}, 
 {{1 + 5^Rational[1, 2], 2, 0}, {0, 1 + 5^Rational[1, 2], -2}, {2, 0, -1 - 5^Rational[1, 2]}}, 
 {{1 + 5^Rational[1, 2], -2, 0}, {0, -1 - 5^Rational[1, 2], -2}, {0, -1 - 5^Rational[1, 2], 2}}, 
 {{1 + 5^Rational[1, 2], -2, 0}, {0, -1 - 5^Rational[1, 2], -2}, {2, 0, -1 - 5^Rational[1, 2]}}, 
 {{1 + 5^Rational[1, 2], -2, 0}, {0, -1 - 5^Rational[1, 2], 2}, {2, 0, 1 + 5^Rational[1, 2]}}, 
 {{-1 - 5^Rational[1, 2], -2, 0}, {-1 - 5^Rational[1, 2], 2, 0}, {-2, 0, 1 + 5^Rational[1, 2]}}, 
 {{-1 - 5^Rational[1, 2], -2, 0}, {-1 - 5^Rational[1, 2], 2, 0}, {-2, 0, -1 - 5^Rational[1, 2]}}, 
 {{-1 - 5^Rational[1, 2], -2, 0}, {0, -1 - 5^Rational[1, 2], -2}, {0, -1 - 5^Rational[1, 2], 2}}, 
 {{-1 - 5^Rational[1, 2], -2, 0}, {0, -1 - 5^Rational[1, 2], -2}, {-2, 0, -1 - 5^Rational[1, 2]}}, 
 {{-1 - 5^Rational[1, 2], -2, 0}, {0, -1 - 5^Rational[1, 2], 2}, {-2, 0, 1 + 5^Rational[1, 2]}}, 
 {{-1 - 5^Rational[1, 2], 2, 0}, {0, 1 + 5^Rational[1, 2], 2}, {0, 1 + 5^Rational[1, 2], -2}}, 
 {{-1 - 5^Rational[1, 2], 2, 0}, {0, 1 + 5^Rational[1, 2], 2}, {-2, 0, 1 + 5^Rational[1, 2]}}, 
 {{-1 - 5^Rational[1, 2], 2, 0}, {0, 1 + 5^Rational[1, 2], -2}, {-2, 0, -1 - 5^Rational[1, 2]}}, 
 {{0, 1 + 5^Rational[1, 2], 2}, {2, 0, 1 + 5^Rational[1, 2]}, {-2, 0, 1 + 5^Rational[1, 2]}}, 
 {{0, 1 + 5^Rational[1, 2], -2}, {-2, 0, -1 - 5^Rational[1, 2]}, {2, 0, -1 - 5^Rational[1, 2]}}, 
 {{0, -1 - 5^Rational[1, 2], -2}, {-2, 0, -1 - 5^Rational[1, 2]}, {2, 0, -1 - 5^Rational[1, 2]}}, 
 {{0, -1 - 5^Rational[1, 2], 2}, {2, 0, 1 + 5^Rational[1, 2]}, {-2, 0, 1 + 5^Rational[1, 2]}}}]}, 
 Polygon[{{{1 + 5^Rational[1, 2], 2, 0}, {1 + 5^Rational[1, 2], -2, 0}, {-1 - 5^Rational[1, 2], -2, 0}, {-1 - 5^Rational[1, 2], 2, 0}}, 
 {{0, 1 + 5^Rational[1, 2], 2}, {0, 1 + 5^Rational[1, 2], -2}, {0, -1 - 5^Rational[1, 2], -2}, {0, -1 - 5^Rational[1, 2], 2}}, 
 {{2, 0, 1 + 5^Rational[1, 2]}, {-2, 0, 1 + 5^Rational[1, 2]}, {-2, 0, -1 - 5^Rational[1, 2]}, {2, 0, -1 - 5^Rational[1, 2]}}}]}, 
 Background -> RGBColor[1., 0.84, 0.92], Boxed -> False, ImageSize -> {100, 100},
  ViewPoint -> {1.7799378875996972`, 1.7799378875996972`, 1.7799378875996972`}, ViewVector -> {{12, 12, 12}, {2, 2, 2}}, ViewVertical -> {0., 0., 1.}];
i=Nest[Darker,ImageResize[k,50],5];
l=Nest[Darker,ImageResize[k,50],5];
f[n_]:=f[n]=ColorSeparate@Nest[Lighter,l,n];


(* ::Subsection:: *)
(*4-5	icon+.nb*)


r=5/Sqrt[50-10 Sqrt[5]];x=Sqrt[2/(5-Sqrt[5])];
v=PolyhedronData["Icosahedron","Vertices"];
po=#[[Last@FindShortestTour@#]]&/@Partition[v,4];
vv=RotationTransform[-ArcSin[x/(2r)] ,{0,1,0}]@v//Simplify;


s=Lighter@{Blue,Green,Red};
g[x_]:=s[[Ceiling@(x/4)]]
h[x_]:=Lighter@Lighter@s[[Ceiling@(x/4)]]


(* ::Subsection:: *)
(*12-16	Physical-Experiment\*)


plot[f_,{s_:None,po_:Above},{a_,b_},range_,grid_,axes_,ticks_,data_List,rules___Rule]:=
Block[{d=(b-a)/20},Show[Plot[Callout[If[a<=x<=b,f],s,po],{x,a-d,b+d},
PlotRange->range,GridLines->grid,GridLinesStyle->{LightGray},
AxesStyle->Arrowheads[.015],AxesLabel->axes,Ticks->ticks,rules],
ListPlot@data]]


holdRule[x_,n_:4]:=HoldPattern@x->NumberForm[N@x,n,ScientificNotationThreshold->{-3,4}];
Attributes[holdRule]={HoldAll};
holdRule[{x_},n_:4]:={holdRule[x,n]};
holdRule[{y__,x_},n_:4]:=Append[holdRule[{y},n],holdRule[x,n]];


export[a_,b_,r_holdRule]:={Defer@a,Defer@b,Defer@b/.r,NumberForm[N@a,3,ScientificNotationThreshold->{-3,3}]}
Attributes[export]={HoldAll};
format[{a_,b_,c_,d_}]:=Defer[a=b=c=d]


(* ::Section:: *)
(*Python*)


(* ::ExternalLanguage:: *)
(*def brainfuck(code,inp):*)
(*    n = len(code)*)
(*    mem = [0 for i in range(100)]*)
(*    ptr = 0*)
(*    i = -1*)
(*    inpn = 0*)
(*    ans=""*)
(*    while True:*)
(*        i+=1*)
(*        if i>=n:*)
(*            break*)
(*        e=code[i]*)
(*        if e=='>':*)
(*            ptr+=1*)
(*        elif e=='<':*)
(*            ptr-=1*)
(*        elif e=='+':*)
(*            mem[ptr]+=1*)
(*        elif e=='-':*)
(*            mem[ptr]-=1*)
(*        elif e=='.':*)
(*            ch=chr(mem[ptr])*)
(*            ans += ch*)
(*        elif e==',':*)
(*            mem[ptr]=ord(inp[inpn])*)
(*            inpn+=1*)
(*        elif e=='[':*)
(*            if mem[ptr]==0:*)
(*                pas=1*)
(*                while pas>0:*)
(*                    i+=1*)
(*                    if code[i]=='[':*)
(*                        pas+=1*)
(*                    elif code[i]==']':*)
(*                        pas-=1*)
(*        elif e==']':*)
(*            if mem[ptr]!=0:*)
(*                ret=1*)
(*                while ret>0:*)
(*                    i-=1*)
(*                    if code[i]==']':*)
(*                        ret+=1*)
(*                    elif code[i]=='[':*)
(*                        ret-=1*)
(*    return ans*)


py=ExternalSessions[][[1]];
