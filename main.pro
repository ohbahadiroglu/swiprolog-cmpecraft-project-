% huzeyfe bahadiroglu
% 2018400114
% compiling: yes
% complete: no
:- ['cmpecraft.pro'].

:- init_from_map.


% 10 points
manhattan_distance(A, B, Distance) :- 
    A = [X1,Y1], 
    B = [X2,Y2],
    Distance is ( abs(X1-X2) + abs(Y1-Y2) ).


% 10 points
min_of_two(A,B,A):- A=<B.
min_of_two(A,B,B):- A > B.
minimum_of_list([Min],Min).
minimum_of_list([A,B|Rest], Min) :- 
    minimum_of_list([B|Rest],MinRest),!,
    min_of_two(A,MinRest,Min),!.


% 10 points

obj_type_checker(Obj,ExpectedType) :- get_dict(type,Obj,Otype), Otype = ExpectedType.
get_obj_coor(Obj,ObjCoord) :- get_dict(x,Obj,Xobj),get_dict(y,Obj,Yobj), ObjCoord = [Xobj,Yobj].
get_agent_coor(Agent,AgentCoord) :- get_dict(x,Agent,Xagent),get_dict(y,Agent,Yagent), AgentCoord = [Xagent,Yagent].
enhanced_mandistance(Agent,Obj,Distance) :- get_agent_coor(Agent,AgentCoord), get_obj_coor(Obj,ObjCoord), manhattan_distance(AgentCoord,ObjCoord,Distance).
find_nearest_type(State, ObjectType ,ObjKey,Object,Distance) :- 
  State = [Adict,Odict,_],
  findall( Dist , 
                ( get_dict(Key,Odict,Obj),
                obj_type_checker(Obj,ObjectType),
                enhanced_mandistance(Adict,Obj,Dist)),Distances ),

  minimum_of_list(Distances,MinDist),  

  findall(_{key:Key,distance:Dist,object:Obj}, ( get_dict(Key,Odict,Obj), 
                                              obj_type_checker(Obj,ObjectType) ,
                                              enhanced_mandistance(Adict,Obj,Dist),
                                              Dist = MinDist) , ResultList),
  
  ResultList=[Result|_],
  Distance is Result.get(distance),
  get_dict(ObjKey,Odict,Object),
  ObjKey is Result.get(key).
build(X, N, List)  :- 
  length(List, N), 
  maplist(=(X), List).
% 10 points
make_xactions(Xdiff,Xabs,XActionList):-
  Xdiff < 0 -> build(go_left,Xabs,XActionList);
              build(go_right,Xabs,XActionList).
make_yactions(Ydiff,Yabs,YActionList):-
  Ydiff < 0 -> build(go_up,Yabs,YActionList);
              build(go_down,Yabs,YActionList).
navigate_to(State, X, Y, ActionList, _) :- 
  State = [Adict,_,_],
  get_agent_coor(Adict,AgentCoord),
  AgentCoord = [Xagent,Yagent],
  Xdiff is (X-Xagent),
  Ydiff is (Y-Yagent),
  Xabs is abs(Xdiff),
  Yabs is abs(Ydiff),
  make_xactions(Xdiff,Xabs,Xactions),
  make_yactions(Ydiff,Yabs,Yactions),
  append(Xactions,Yactions,ActionList).
   
% 10 points
chop_nearest_tree(State, ActionList) :- 
  find_nearest_type(State, tree ,_,Object,_),
  get_obj_coor(Object,ObjCoord),
  ObjCoord=[Xobj,Yobj],
  navigate_to(State,Xobj,Yobj,NavigateList,_),
  build(left_click_c,4,LeftClickList),
  append(NavigateList,LeftClickList,ActionList).
% 10 points
mine_nearest_stone(State, ActionList) :-
  find_nearest_type(State, stone ,_,Object,_),
  get_obj_coor(Object,ObjCoord),
  ObjCoord=[Xobj,Yobj],
  navigate_to(State,Xobj,Yobj,NavigateList,_),
  build(left_click_c,4,LeftClickList),
  append(NavigateList,LeftClickList,ActionList).
mine_nearest_cobble(State, ActionList) :-
  find_nearest_type(State, cobblestone ,_,Object,_),
  get_obj_coor(Object,ObjCoord),
  ObjCoord=[Xobj,Yobj],
  navigate_to(State,Xobj,Yobj,NavigateList,_),
  build(left_click_c,4,LeftClickList),
  append(NavigateList,LeftClickList,ActionList).
% 10 points
gather_nearest_food(State, ActionList) :- 
  find_nearest_type(State, food ,_,Object,_),
  get_obj_coor(Object,ObjCoord),
  ObjCoord=[Xobj,Yobj],
  navigate_to(State,Xobj,Yobj,NavigateList,_),
  build(left_click_c,1,LeftClickList),
  append(NavigateList,LeftClickList,ActionList).
% 10 points
coll_req_pickaxe(State,ActionList):-
  chop_nearest_tree(State,ActList1),
  execute_actions(State, ActList1, NextState1),
  chop_nearest_tree(NextState1,ActList2),
  execute_actions(NextState1, ActList2, NextState2),
  mine_nearest_stone(NextState2,ActList3),
  append(ActList1,ActList2,TmpList1),
  append(ActList3,[craft_stick],TmpList2),
  append(TmpList1,TmpList2,ActionList).

coll_req_axe(State,ActionList):-
  chop_nearest_tree(State,ActList1),
  execute_actions(State, ActList1, NextState1),
  chop_nearest_tree(NextState1,ActList2),
  execute_actions(NextState1, ActList2, NextState2),
  mine_nearest_stone(NextState2,ActList3),
  append(ActList1,ActList2,TmpList1),
  append(ActList3,[craft_stick],TmpList2),
  append(TmpList1,TmpList2,ActionList).

coll_req_stick(State,ActionList):-
  chop_nearest_tree(State,ActionList).

collect_requirements(State, ItemType, ActionList) :- craftable(State,ItemType) -> build("",0,ActionList);
                                                            collect_requirements_continued(State, ItemType, ActionList).
collect_requirements_continued(State, ItemType, ActionList) :-
  ItemType = stone_pickaxe -> coll_req_pickaxe(State,ActionList);
  ItemType = stone_axe -> coll_req_axe(State,ActionList);
  coll_req_stick(State,ActionList).                            
% 5 points
find_castle_location(State, XMin, YMin, XMax, YMax) :- % not completed, I tried many things but couldn't make it.
  State = [ _,_,_],
  member([XMin,YMin,'.'],Objects);member([XMin,YMin,'@'],Objects),
  member([XMin+1,YMin,'.'],Objects),
  member([XMin+2,YMin,'.'],Objects),
  member([XMin,YMin+1,'.'],Objects),
  member([XMin+1,YMin+1,'.'],Objects),
  member([XMin+2,YMin+1,'.'],Objects),
  member([XMin,YMin+2,'.'],Objects),
  member([XMin+1,YMin+2,'.'],Objects),
  member([XMin+2,YMin+2,'.'],Objects),
  XMax is XMin + 2,
  YMax is YMin + 2.

% 15 points
% make_castle(+State, -ActionList) :- .

