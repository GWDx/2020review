(* ::Package:: *)

(* ::Subsection:: *)
(*ZhihuData*)


(* ::Input:: *)
(*ZhihuData={};*)
(*firstURL="https://www.zhihu.com/api/v3/feed/members/gan-wen-di/activities";*)


(* ::Input:: *)
(*crawl[URL_]:=HTTPRequest[URL,TimeConstraint->5];*)


(* ::Input:: *)
(*crawl[URL_]:=Block[{ans,data,next},ans=URLExecute[URL,"RawJSON"];*)
(*ZhihuData=Join[ZhihuData,ans[["data"]]];Pause@RandomReal[2];ans[["paging","next"]]]*)


(* ::Input:: *)
(*Nest[crawl,firstURL,155]*)


(* ::Input:: *)
(*DumpSave[NotebookDirectory[]<>"mxs\\ZhihuData.mx",ZhihuData];*)


(* ::Subsection:: *)
(*nbTimeData*)


(* ::Input:: *)
(*SetDirectory[dir="C:\\Users\\GWDx\\Desktop\\2020\\NB\\DATA\\"];*)
(*filenames=FileNames[];*)


(* ::Input:: *)
(*rawEditTime[nbName_]:=Block[{nb=NotebookOpen[nbName,Visible->False],allCell,ans},*)
(*SelectionMove[nb,All,Notebook];*)
(*allCell=SelectedCells[nb];*)
(*ans=CurrentValue[#,CellChangeTimes]&/@allCell;*)
(*NotebookClose@nb;*)
(*ans]*)


(* ::Input:: *)
(*nbTimeData=Table[rawEditTime[dir<>name],{name,filenames}];*)


(* ::Input:: *)
(*DumpSave[NotebookDirectory[]<>"mxs\\nbTimeData.mx",nbTimeData];*)
