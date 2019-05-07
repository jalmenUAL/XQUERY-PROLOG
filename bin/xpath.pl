%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XQUERY implementation by means of SWI-Prolog
% Jesus Almendros (April 2009)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XPATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD-XPATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_xpath(XPath,ListPath):-concat_atom(ListPath,'/',XPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XPATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 

xpath([''],Elem,Elem):-!.

xpath([text],[Elem|RElem],[Elem|RElem2]):-atomic(Elem),!,
                                           xpath([text],RElem,RElem2).

xpath([text],[_|RElem],RElem2):-!,xpath([text],RElem,RElem2).

xpath([point],Elem,Elem):-!.

xpath([point,''],Elem,Elem):-!.
			 
xpath([point|XPath],[element(Label,Attrs,Element)|SubElement],Values):-!,
			xpath([Label|XPath],[element(Label,Attrs,Element)],Value1),
			xpath([point|XPath],SubElement,Value2), 
			append(Value1,Value2,Values).


xpath([Label,Att],[element(Label,Attrs,_)|Relement],Values2):-                                           
                                                is_attribute(Att,Name),
                                                attribute_values(Attrs,Name,Values), Values\=[],!,  
                                                xpath([Label,Att],Relement,Relement2),
                                                append(Values,Relement2,Values2).

xpath([Label,Att],[_|Relement],Relement2):-
                                                is_attribute(Att,_),!,
                                                xpath([Label,Att],Relement,Relement2).

xpath([Label],[element(Label,Attr,Sublabel)|Relement],
                                                [element(Label,Attr,Sublabel)|Relement2]):-!,
                                                xpath([Label],Relement,Relement2).

xpath([Label],[_|Relement],Relement2):-!,
                                                xpath([Label],Relement,Relement2).

xpath([Label1,Label2|RLabel],[element(Label1,_,Sublabel)|Relement],Elem):- 
                                                xpath([Label2|RLabel],Sublabel,Sublabel2),
                                                Sublabel2 \= [],
                                                xpath([Label1,Label2|RLabel],Relement,Relement2),
                                                Relement2 \= [],!,
                                                append(Sublabel2,Relement2,Elem).

xpath([Label1,Label2|RLabel],[element(Label1,_,Sublabel)|Relement],Relement2):- 
                                                xpath([Label2|RLabel],Sublabel,Sublabel2),
                                                Sublabel2 = [],
                                                xpath([Label1,Label2|RLabel],Relement,Relement2),
                                                Relement2 \= [],!.

xpath([Label1,Label2|RLabel],[element(Label1,_,Sublabel)|_],Sublabel2):- 
                                        xpath([Label2|RLabel],Sublabel,Sublabel2),
                                        Sublabel2 \= [],!.

xpath([Label1,Label2|RLabel],[_|Relement],Relement2):-!,
                                        xpath([Label1,Label2|RLabel],Relement,Relement2).

xpath(_,[],[]):-!.

xpath([],L,L):-!.

 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTE-XPATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_xpath(In,Out,XPath):- load_xml(In,InputDoc),
                        load_xpath(XPath,ListXPath),
                        xpath(ListXPath,InputDoc,OutputDoc), 
                        Result=[element(result,[],OutputDoc)],   
                        write_xml(Out,Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXECUTE-TERM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_term(Y,D,XPath):-(Y=[];Y=[_|_]),!,
                                          load_xpath(XPath,ListXPath),  
                                	   xpath([point|ListXPath],Y,D).

execute_term(In,D,XPath):-  
                        load_xml(In,L),
                        load_xpath(XPath,ListXPath),
                        xpath(ListXPath,L,D).

 