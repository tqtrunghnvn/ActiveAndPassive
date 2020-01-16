/*************************************************************************
	
	File: convert.pl
	Written by Trung Q. Tran
	
	This file defines three-steps conversion and its second step.
	
*************************************************************************/

:- module(convert, [convert/4
						]).

:- use_module(convertible, [s/6,
							lex/2,
							lex/3,
							lex/4,
							s1/6,
							subObj/2]).


/*========================================================================   
	Conversion rules
========================================================================*/
	
/*================ Three-steps conversion =================*/
convert(ActiveS,ActiveRe,PassiveS,PassiveRe) :- 
	/*=== From Active sentence to Passive sentence ===*/
	(\+ var(ActiveS);\+ var(ActiveRe)),
	s(ActiveRe,Tense,Qs,Qo,ActiveS,[]),			% First step
	write('Tense: '),write(Tense),write('\n'),
	convert(ActiveRe,Tense,Qs,Qo,PassiveRe),	% Second step	
	!,
	s1(PassiveRe,Tense,Qo,Qs,PassiveS,[])		% Third step	
;
	/*=== From Passive sentence to Active sentence ===*/
	(\+ var(PassiveS);\+ var(PassiveRe)),
	s1(PassiveRe,Tense,Qo,Qs,PassiveS,[]),		% First step
	write('Tense: '),write(Tense),write('\n'),
	convert(ActiveRe,Tense,Qs,Qo,PassiveRe),	% Second step
	!,
	s(ActiveRe,Tense,Qs,Qo,ActiveS,[])			% Third step	
;
	/*=========== Generate all cases =================*/
	var(ActiveS),var(ActiveRe),var(PassiveS),var(PassiveRe),
	s(ActiveRe,Tense,Qs,Qo,ActiveS,[]),			% First step
	write('Tense: '),write(Tense),write('\n'),
	convert(ActiveRe,Tense,Qs,Qo,PassiveRe),	% Second step	
	s1(PassiveRe,Tense,Qo,Qs,PassiveS,[]).		% Third step

/*========== Group 1: For simple_present and simple_past ===========*/
/*=== Positive ===*/
convert(s(NP1,v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),v(Y1),agent(by),NP11)) :-
	subAndObj(NP1,NP11),
	subAndObj(NP22,NP2),	
	(		
		\+ NP22=np(pro(i)),		
		lex(Aux,aux,Tense,Qo)	
	;			
		NP22=np(pro(i)),		
		lex(Aux,aux,Tense,special)
	),
	past_participle(Y,Y1,Qs,Tense).
	
/*=== Negative ===*/
convert(s(NP1,aux(AUX_POL),pol(not),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),pol(not),v(Y1),agent(by),NP11)) :-
	subAndObj(NP1,NP11),
	lex(AUX_POL,pol,Tense,Qs),
	subAndObj(NP22,NP2),	
	(		
		\+ NP22=np(pro(i)),		
		lex(Aux,aux,Tense,Qo)	
	;			
		NP22=np(pro(i)),		
		lex(Aux,aux,Tense,special)
	),
	past_participle(Y,Y1,plural,simple_present). % For negative active sentence, verb is always plural.
	
/*====== Group 2: For simple_future, continuous_past, continuous_present, ======*/
/*=============== perfect_past, and perfect_present ===================*/
/*=== Positive ===*/
convert(s(NP1,aux(Aux1),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),auxTense(AuxTense),v(Y1),agent(by),NP11)) :-
	subAndObj(NP1,NP11),
	(		
		\+ NP1=np(pro(i)),		
		lex(Aux1,aux,Tense,Qs)	
	;			
		NP1=np(pro(i)),		
		lex(Aux1,aux,Tense,special)
	),	
	subAndObj(NP22,NP2),	
	(		
		\+ NP22=np(pro(i)),		
		lex(Aux,aux,Tense,Qo)	
	;			
		NP22=np(pro(i)),		
		lex(Aux,aux,Tense,special)
	),	
	lex(AuxTense,aux,Tense),	
	past_participle(Y,Y1,Qs,Tense).
	
/*=== Negative ===*/
convert(s(NP1,aux(Aux1),pol(not),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),pol(not),auxTense(AuxTense),v(Y1),agent(by),NP11)) :-
	convert(s(NP1,aux(Aux1),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),auxTense(AuxTense),v(Y1),agent(by),NP11)).

/*======== Group 3: For continuous_future, perfect_future, ========*/
/*==== perfect_continuous_past, and perfect_continuous_present ====*/
/*=== Positive ===*/
convert(s(NP1,aux(Aux2),aux(Aux3),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),aux(Aux1),auxTense(AuxTense),v(Y1),agent(by),NP11)) :-
	subAndObj(NP1,NP11),
	(		
		\+ NP1=np(pro(i)),		
		lex(Aux2,aux,Tense,Qs)	
	;			
		NP1=np(pro(i)),		
		lex(Aux2,aux,Tense,special)
	),	
	lex(Aux3,aux1,Tense),
	subAndObj(NP22,NP2),	
	(		
		\+ NP22=np(pro(i)),		
		lex(Aux,aux,Tense,Qo)	
	;			
		NP22=np(pro(i)),		
		lex(Aux,aux,Tense,special)
	),	
	lex(Aux1,aux1,Tense),	
	lex(AuxTense,aux,Tense),	
	past_participle(Y,Y1,Qs,Tense).

/*=== Negative ===*/
convert(s(NP1,aux(Aux2),pol(not),aux(Aux3),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),pol(not),aux(Aux1),auxTense(AuxTense),v(Y1),agent(by),NP11)) :-
	convert(s(NP1,aux(Aux2),aux(Aux3),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),aux(Aux1),auxTense(AuxTense),v(Y1),agent(by),NP11)).
	
/*============ Group 4: For perfect_continuous_future =============*/
/*=== Positive ===*/
convert(s(NP1,aux(Aux3),aux(Aux4),aux(Aux5),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),aux(Aux1),aux(Aux2),auxTense(AuxTense),v(Y1),agent(by),NP11)) :-
	subAndObj(NP1,NP11),
	(		
		\+ NP1=np(pro(i)),		
		lex(Aux3,aux,Tense,Qs)	
	;			
		NP1=np(pro(i)),		
		lex(Aux3,aux,Tense,special)
	),	
	lex(Aux4,aux1,Tense),	
	lex(Aux5,aux2,Tense),
	subAndObj(NP22,NP2),	
	(		
		\+ NP22=np(pro(i)),		
		lex(Aux,aux,Tense,Qo)	
	;			
		NP22=np(pro(i)),		
		lex(Aux,aux,Tense,special)
	),	
	lex(Aux1,aux1,Tense),	
	lex(Aux2,aux2,Tense),	
	lex(AuxTense,aux,Tense),	
	past_participle(Y,Y1,Qs,Tense).	
	
/*=== Negative ===*/
convert(s(NP1,aux(Aux3),pol(not),aux(Aux4),aux(Aux5),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),pol(not),aux(Aux1),aux(Aux2),auxTense(AuxTense),v(Y1),agent(by),NP11)) :-
	convert(s(NP1,aux(Aux3),aux(Aux4),aux(Aux5),v(Y),NP2),Tense,Qs,Qo,s(NP22,aux(Aux),aux(Aux1),aux(Aux2),auxTense(AuxTense),v(Y1),agent(by),NP11)).
	
/*============ Modal verb =============*/
/*=== Positive ===*/
convert(s(NP1,modal(ModalVerb),v(Y),NP2),Tense,_,_,s(NP22,modal(ModalVerb),aux(Aux),v(Y1),agent(by),NP11)) :-
	subAndObj(NP1,NP11),
	subAndObj(NP22,NP2),	
	lex(ModalVerb,modal),
	lex(Aux,aux1,continuous_future),
	past_participle(Y,Y1,plural,Tense).

/*=== Negative ===*/
convert(s(NP1,modal(ModalVerb),pol(not),v(Y),NP2),Tense,Qs,Qo,s(NP22,modal(ModalVerb),pol(not),aux(Aux),v(Y1),agent(by),NP11)) :-
	convert(s(NP1,modal(ModalVerb),v(Y),NP2),Tense,Qs,Qo,s(NP22,modal(ModalVerb),aux(Aux),v(Y1),agent(by),NP11)).
	
	
/*========================================================================   
	Functional rules
========================================================================*/
	
/*====== Subject in active sentence becomes object in passive sentence ======*/
subAndObj(Sub,Obj) :-
(
	/*=== The case of pronoun ===*/
	(
		\+ var(Sub),Sub=np(pro(_));
		\+ var(Obj),Obj=np(pro(_))
	),
	Sub=np(pro(PRO1)),
	Obj=np(pro(PRO2)),
	subObj(PRO1,PRO2)
;
	/*=== The case of not pronoun ===*/
	(
		\+ var(Sub),\+ Sub=np(pro(_));
		\+ var(Obj),\+ Obj=np(pro(_))
	),
	Sub=Obj
).
	
/*================ Get the past participle form of verb ================*/
past_participle(X,Y,Qs,Tense) :- 
	Tense=simple_present,
	(
		Qs=singular,lex(Y,v_simple,X,_)
	;
		Qs=plural,lex(Y,v_simple,_,X)
	)
;
	Tense=simple_future,
	lex(Y,v_simple,_,X)
;
	(
		Tense=simple_past;
		Tense=perfect_past;Tense=perfect_present;Tense=perfect_future
	),
	lex(Y,v_past,X,X)
;
	(
		Tense=continuous_past;Tense=continuous_present;Tense=continuous_future;
		Tense=perfect_continuous_past;Tense=perfect_continuous_present;Tense=perfect_continuous_future		
	),
	lex(Y,v_continuous,X,X)
.
