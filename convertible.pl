/*************************************************************************
	
	File: convertible.pl
	Written by Trung Q. Tran
	
	This file defines DCG rules for the first step and third step in 
	three-steps conversion. 
	Lexicon and other rules are also defined here.
			
*************************************************************************/

:- module(convertible, [s/6,
						lex/2,
						lex/3,
						lex/4,
						s1/6,
						subObj/2]).

						
/*========================================================================
   Lexicon
========================================================================*/

/*======== Modal verb ===========*/
lex(should,modal).
lex(can,modal).
lex(could,modal).
lex(must,modal).
lex(may,modal).
lex(might,modal).

/*======== Adjective ===========*/
lex(small,adj).
lex(frightened,adj).
lex(beautiful,adj).
lex(big,adj).
lex(fat,adj).

/*======== Preposition ===========*/
lex(in,pre).
lex(on,pre).
lex(at,pre).
lex(above,pre).
lex(below,pre).
lex(over,pre).
lex(under,pre).

/*======== Determiner ===========*/
lex(a,det,singular).
lex(an,det,singular).
lex(the,det,_).

/*======== Noun =========*/
lex(man,n,singular).
lex(woman,n,singular).
lex(men,n,plural).
lex(women,n,plural).
lex(table,n,singular).
lex(tables,n,plural).
lex(cat,n,singular).
lex(cats,n,plural).
lex(shower,n,singular).
lex(showers,n,plural).
lex(cow,n,singular).
lex(cows,n,plural).
lex(apple,n,singular).
lex(apples,n,plural).

/*========= Auxiliary corresponding to Tense in Passive sentence ===========*/
lex(being,aux,continuous_past).
lex(being,aux,continuous_present).
lex(been,aux,perfect_past).
lex(been,aux,perfect_present).
lex(be,aux,simple_future).

lex(being,aux,continuous_future).
lex(been,aux,perfect_future).
lex(being,aux,perfect_continuous_past).
lex(being,aux,perfect_continuous_present).

lex(being,aux,perfect_continuous_future).

/*========= The second auxiliary corresponding to Tense =========*/
lex(be,aux1,continuous_future).
lex(have,aux1,perfect_future).
lex(been,aux1,perfect_continuous_past).
lex(been,aux1,perfect_continuous_present).

lex(have,aux1,perfect_continuous_future).

/*========= The third auxiliary corresponding to Tense =========*/
lex(been,aux2,perfect_continuous_future).

/*========= Verb =========*/
lex(shoots,v,simple_present,singular,group1).
lex(shoot,v,simple_present,plural,group1).
lex(shooted,v,simple_past,_,group1).	     
lex(shooting,v,continuous_present,_,group2).
lex(shooting,v,continuous_past,_,group2).
lex(shooted,v,perfect_past,_,group2).
lex(shooted,v,perfect_present,_,group2).
lex(shoot,v,simple_future,_,group2).
lex(shooting,v,continuous_future,_,group3).
lex(shooted,v,perfect_future,_,group3).
lex(shooting,v,perfect_continuous_past,_,group3).
lex(shooting,v,perfect_continuous_present,_,group3).
lex(shooting,v,perfect_continuous_future,_,group4).
lex(shooted,v,past_participle,_,past_participle).

lex(loves,v,simple_present,singular,group1).
lex(love,v,simple_present,plural,group1).
lex(loved,v,simple_past,_,group1).
lex(loving,v,continuous_present,_,group2).
lex(loving,v,continuous_past,_,group2).
lex(loved,v,perfect_past,_,group2).
lex(loved,v,perfect_present,_,group2).
lex(love,v,simple_future,_,group2).
lex(loving,v,continuous_future,_,group3).
lex(loved,v,perfect_future,_,group3).
lex(loving,v,perfect_continuous_past,_,group3).
lex(loving,v,perfect_continuous_present,_,group3).
lex(loving,v,perfect_continuous_future,_,group4).
lex(loved,v,past_participle,_,past_participle).

lex(buys,v,simple_present,singular,group1).
lex(buy,v,simple_present,plural,group1).
lex(bought,v,simple_past,_,group1).
lex(buying,v,continuous_present,_,group2).
lex(buying,v,continuous_past,_,group2).
lex(bought,v,perfect_past,_,group2).
lex(bought,v,perfect_present,_,group2).
lex(buy,v,simple_future,_,group2).
lex(buying,v,continuous_future,_,group3).
lex(bought,v,perfect_future,_,group3).
lex(buying,v,perfect_continuous_past,_,group3).
lex(buying,v,perfect_continuous_present,_,group3).
lex(buying,v,perfect_continuous_future,_,group4).
lex(bought,v,past_participle,_,past_participle).

/*======= Past participle form of verb ========*/
/*=== Three types: simple, past, continuous ===*/
lex(shooted,v_simple,shoots,shoot).
lex(shooted,v_past,shooted,shooted).
lex(shooted,v_continuous,shooting,shooting).
lex(loved,v_simple,loves,love).
lex(loved,v_past,loved,loved).
lex(loved,v_continuous,loving,loving).
lex(bought,v_simple,buys,buy).
lex(bought,v_past,bought,bought).
lex(bought,v_continuous,buying,buying).

/*========== First person pronoun ============*/
lex(i,pro,subject,plural).
lex(we,pro,subject,plural).
lex(me,pro,object,plural).
lex(us,pro,object,plural).

/*========== Second person pronoun ===========*/
lex(you,pro,subject,plural).
lex(you,pro,object,plural).

/*========== Third person pronoun ============*/
lex(he,pro,subject,singular).
lex(she,pro,subject,singular).
lex(it,pro,subject,singular).
lex(they,pro,subject,plural).
lex(him,pro,object,singular).
lex(her,pro,object,singular).
lex(it,pro,object,singular).
lex(them,pro,object,plural).

/*========= Auxiliary verb ===========*/
lex(am,aux,simple_present,special).
lex(is,aux,simple_present,singular).
lex(are,aux,simple_present,plural).
lex(am,aux,continuous_present,special).
lex(is,aux,continuous_present,singular).
lex(are,aux,continuous_present,plural).

lex(was,aux,simple_past,special).
lex(was,aux,simple_past,singular).
lex(were,aux,simple_past,plural).
lex(was,aux,continuous_past,special).
lex(was,aux,continuous_past,singular).
lex(were,aux,continuous_past,plural).

lex(had,aux,perfect_past,_).
lex(have,aux,perfect_present,special).
lex(has,aux,perfect_present,singular).
lex(have,aux,perfect_present,plural).
lex(will,aux,simple_future,_).

lex(will,aux,continuous_future,_).
lex(will,aux,perfect_future,_).
lex(had,aux,perfect_continuous_past,_).
lex(have,aux,perfect_continuous_present,special).
lex(has,aux,perfect_continuous_present,singular).
lex(have,aux,perfect_continuous_present,plural).

lex(will,aux,perfect_continuous_future,_).

lex(did,pol,simple_past,_).
lex(does,pol,simple_present,singular).
lex(do,pol,simple_present,plural).

/*======== Subject and its corresponding object ========*/
subObj(i,me).
subObj(we,us).
subObj(you,you).
subObj(he,him).
subObj(she,her).
subObj(it,it).
subObj(they,them).


/*========================================================================
   DCG rules
========================================================================*/

/*============================= Active sentence ============================*/
/*= Qs is for subject ; Qo is for object ; Qo is used for passive sentence =*/
/*=============== Group 1: For simple_present and simple_past ==============*/
/*=== Positive ===*/
s(s(NP1,V,NP2),Tense,Qs,Qo) --> 
			np(NP1,subject,Qs), 
			v(V,Tense,Qs,group1), 
			np(NP2,object,Qo).
			
/*=== Negative ===*/
s(s(NP1,AUX_POL,pol(not),V,NP2),Tense,Qs,Qo) --> 
			np(NP1,subject,Qs), 
			pol(AUX_POL,Tense,Qs),
			pol,
			v(V,simple_present,plural,group1),
			np(NP2,object,Qo).			

/*====== Group 2: For simple_future, continuous_past, continuous_present, ======*/
/*=================== perfect_past, and perfect_present ========================*/
/*=== Positive ===*/
s(s(NP1,AUX,V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			(
				{\+ NP1=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP1=np(pro(i))},
				aux(AUX,Tense,special)
			),
			v(V,Tense,Qs,group2),
			np(NP2,object,Qo).
			
/*=== Negative ===*/
s(s(NP1,AUX,pol(not),V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			(
				{\+ NP1=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP1=np(pro(i))},
				aux(AUX,Tense,special)
			),
			pol,
			v(V,Tense,Qs,group2),
			np(NP2,object,Qo).
			
/*======== Group 3: For continuous_future, perfect_future, ========*/
/*==== perfect_continuous_past, and perfect_continuous_present ====*/
/*=== Positive ===*/
s(s(NP1,AUX,AUX1,V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			(
				{\+ NP1=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP1=np(pro(i))},
				aux(AUX,Tense,special)
			),
			aux1(AUX1,Tense),
			v(V,Tense,Qs,group3),
			np(NP2,object,Qo).
			
/*=== Negative ===*/
s(s(NP1,AUX,pol(not),AUX1,V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			(
				{\+ NP1=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP1=np(pro(i))},
				aux(AUX,Tense,special)
			),
			pol,
			aux1(AUX1,Tense),
			v(V,Tense,Qs,group3),
			np(NP2,object,Qo).
			
/*============ Group 4: For perfect_continuous_future =============*/
/*=== Positive ===*/
s(s(NP1,AUX,AUX1,AUX2,V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			(
				{\+ NP1=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP1=np(pro(i))},
				aux(AUX,Tense,special)
			),
			aux1(AUX1,Tense),
			aux2(AUX2,Tense),
			v(V,Tense,Qs,group4),
			np(NP2,object,Qo).
			
/*=== Negative ===*/
s(s(NP1,AUX,pol(not),AUX1,AUX2,V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			(
				{\+ NP1=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP1=np(pro(i))},
				aux(AUX,Tense,special)
			),
			pol,
			aux1(AUX1,Tense),
			aux2(AUX2,Tense),
			v(V,Tense,Qs,group4),
			np(NP2,object,Qo).
			
/*============ Modal verb =============*/
/*=== Positive ===*/
s(s(NP1,MODAL,V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			modal(MODAL),
			{Tense=simple_present},
			v(V,Tense,plural,group1),
			np(NP2,object,Qo).

/*=== Negative ===*/
s(s(NP1,MODAL,pol(not),V,NP2),Tense,Qs,Qo) -->
			np(NP1,subject,Qs),
			modal(MODAL),
			pol,
			{Tense=simple_present},
			v(V,Tense,plural,group1),
			np(NP2,object,Qo).
			
	
/*========================== Passive sentence ========================*/
/*========== Group1: For simple_present and simple_past ===========*/
/*=== Positive ===*/
s1(s(NP2,AUX,V,agent(by),NP1),Tense,Qs,Qo) --> 
			np(NP2,subject,Qs), 			
			(
				{\+ NP2=np(pro(i))},				
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},				
				aux(AUX,Tense,special)				
			),			
			v(V,past_participle,Qs,past_participle),			
			agent,
			np(NP1,object,Qo).
				
/*=== Negative ===*/
s1(s(NP2,AUX,pol(not),V,agent(by),NP1),Tense,Qs,Qo) --> 
			np(NP2,subject,Qs), 			
			(
				{\+ NP2=np(pro(i))},				
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},				
				aux(AUX,Tense,special)				
			),			
			pol,
			v(V,past_participle,Qs,past_participle),			
			agent,
			np(NP1,object,Qo).			
			
/*====== Group 2: For simple_future, continuous_past, continuous_present, ======*/
/*=============== perfect_past, and perfect_present ===================*/
/*=== Positive ===*/
s1(s(NP2,AUX,AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			np(NP2,subject,Qs),						
			(
				{\+ NP2=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},
				aux(AUX,Tense,special)
			),						
			aux(AUX_TENSE,Tense),			
			v(V,past_participle,Qs,past_participle),			
			agent,
			np(NP1,object,Qo).			
			
/*=== Negative ===*/
s1(s(NP2,AUX,pol(not),AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			np(NP2,subject,Qs),						
			(
				{\+ NP2=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},
				aux(AUX,Tense,special)
			),			
			pol,
			aux(AUX_TENSE,Tense),			
			v(V,past_participle,Qs,past_participle),			
			agent,
			np(NP1,object,Qo).			
			
/*======== Group 3: For continuous_future, perfect_future, ========*/
/*==== perfect_continuous_past, and perfect_continuous_present ====*/
/*=== Positive ===*/
s1(s(NP2,AUX,AUX1,AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			np(NP2,subject,Qs),			
			(
				{\+ NP2=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},
				aux(AUX,Tense,special)
			),
			aux1(AUX1,Tense),
			aux(AUX_TENSE,Tense),
			v(V,past_participle,Qs,past_participle),
			agent,
			np(NP1,object,Qo).			
			
/*=== Negative ===*/
s1(s(NP2,AUX,pol(not),AUX1,AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			np(NP2,subject,Qs),			
			(
				{\+ NP2=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},
				aux(AUX,Tense,special)
			),
			pol,
			aux1(AUX1,Tense),
			aux(AUX_TENSE,Tense),
			v(V,past_participle,Qs,past_participle),
			agent,
			np(NP1,object,Qo).			
			
/*============ Group 4: For perfect_continuous_future =============*/
/*=== Positive ===*/
s1(s(NP2,AUX,AUX1,AUX2,AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			np(NP2,subject,Qs),			
			(
				{\+ NP2=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},
				aux(AUX,Tense,special)
			),
			aux1(AUX1,Tense),
			aux2(AUX2,Tense),
			aux(AUX_TENSE,Tense),
			v(V,past_participle,Qs,past_participle),
			agent,
			np(NP1,object,Qo).			
			
/*=== Negative ===*/
s1(s(NP2,AUX,pol(not),AUX1,AUX2,AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			np(NP2,subject,Qs),			
			(
				{\+ NP2=np(pro(i))},
				aux(AUX,Tense,Qs)
			;
				{NP2=np(pro(i))},
				aux(AUX,Tense,special)
			),
			pol,
			aux1(AUX1,Tense),
			aux2(AUX2,Tense),
			aux(AUX_TENSE,Tense),
			v(V,past_participle,Qs,past_participle),
			agent,
			np(NP1,object,Qo).			
			
/*============ Modal verb =============*/
/*=== Positive ===*/
s1(s(NP2,MODAL,AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->
			%{write("Case 1."),write("\n")},
			np(NP2,subject,Qs),
			modal(MODAL),
			{Tense=simple_present},
			aux1(AUX_TENSE,continuous_future),
			v(V,past_participle,Qs,past_participle),
			agent,
			np(NP1,object,Qo).			

/*=== Negative ===*/
s1(s(NP2,MODAL,pol(not),AUX_TENSE,V,agent(by),NP1),Tense,Qs,Qo) -->			
			np(NP2,subject,Qs),			
			modal(MODAL),			
			pol,
			{Tense=simple_present},
			aux1(AUX_TENSE,continuous_future),
			v(V,past_participle,Qs,past_participle),			
			agent,
			np(NP1,object,Qo).
			

/*============ Noun phrase ===============*/
np(np(DET,N),_,Q) --> det(DET,Q),n(N,Q).
np(np(PRO),P,Q) --> pro(PRO,P,Q).
np(np(DET,ADJ,N),_,Q) --> det(DET,Q),adj(ADJ),n(N,Q).

np(np(DET,N,PP),_,Q) --> det(DET,Q),n(N,Q),pp(PP).
np(np(PRO,PP),P,Q) --> pro(PRO,P,Q),pp(PP).
np(np(DET,ADJ,N,PP),_,Q) --> det(DET,Q),adj(ADJ),n(N,Q),pp(PP).

/*============ Prepositional phrase ===============*/
pp(pp(PRE,NP)) --> pre(PRE),np(NP,object,_).

/*============ Fundamental elements ==============*/
det(det(Word),Q) --> [Word],{lex(Word,det,Q)}.
n(n(Word),Q) --> [Word],{lex(Word,n,Q)}.
pro(pro(Word),P,Q) --> [Word],{lex(Word,pro,P,Q)}.
v(v(Word),Tense,Q,Group) --> [Word],{lex(Word,v,Tense,Q,Group)}.
pre(pre(Word)) --> [Word],{lex(Word,pre)}.

adj(adj([Word])) --> [Word],{lex(Word,adj)}.
adj(adj([Word|L])) --> [Word],{lex(Word,adj)},adj(adj(L)).

agent --> [by].
pol --> [not].
pol(aux(Word),Tense,Q) --> [Word],{lex(Word,pol,Tense,Q)}.
aux(aux(Word),Tense,Q) --> [Word],{lex(Word,aux,Tense,Q)}.
aux(auxTense(Word),Tense) --> [Word],{lex(Word,aux,Tense)}.
aux1(aux(Word),Tense) --> [Word],{lex(Word,aux1,Tense)}.
aux2(aux(Word),Tense) --> [Word],{lex(Word,aux2,Tense)}.
modal(modal(Word)) --> [Word],{lex(Word,modal)}.
