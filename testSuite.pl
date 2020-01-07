/*************************************************************************
	
	File: testSuite.pl
	Written by Tran Quang Trung	
	
*************************************************************************/

:- use_module(readLine,[readLine/1]).

:- use_module(convert, [convert/4]).


/*========================================================================
   Examples for active sentences
========================================================================*/

active([the,man,buys,an,apple]).
active([she,love,him]).
active([she,loves,him]).

active([i,bought,an,apple]).
active([they,loved,me]).
active([i,loved,you]).

active([i,is,loving,her]).
active([she,is,loving,me]).
active([i,am,loving,them]).
active([i,am,loving,you]).

active([he,was,loving,i]).
active([she,was,loving,me]).
active([they,were,buying,the,apples]).
active([i,was,loving,you]).

active([i,had,bought,the,apples]).
active([he,has,bought,the,apples]).
active([you,have,loved,her]).
active([she,will,love,me]).

active([you,will,be,loving,them]).
active([the,beautiful,woman,will,have,loved,me]).
active([they,had,been,loving,the,women]).
active([you,have,been,loving,her]).

active([you,will,have,been,loving,them]).
active([they,will,have,been,loving,the,women]).

active([he,does,not,buy,the,big,apples]).
active([they,did,not,love,her]).
active([she,will,not,have,been,buying,an,apple]).

active([he,should,buys,the,big,apples]).
active([he,should,love,them]).
active([he,may,not,love,them]).

active([a,small,beautiful,woman,will,be,buying,a,small,apple,on,the,big,beautiful,table]).


/*========================================================================
   Examples for passive sentences
========================================================================*/

passive([she,was,loved,by,them]).
passive([the,apples,are,bought,by,her]).
passive([you,will,be,loved,by,me]).
passive([the,apples,were,being,bought,by,me]).
passive([he,is,being,loved,by,them]).
passive([he,will,be,being,loved,by,her]).
passive([the,apple,had,been,bought,by,him]).
passive([i,have,been,loved,by,her]).
passive([she,will,have,been,loved,by,me]).
passive([an,apple,had,been,being,bought,by,us]).
passive([she,has,been,being,loved,by,us]).
passive([an,apple,will,have,been,being,bought,by,me]).

passive([the,apples,were,not,bought,by,them]).
passive([she,has,not,been,being,loved,by,him]).

passive([he,should,be,love,by,her]).
passive([he,should,be,loved,by,her]).
passive([the,apples,should,not,be,bought,by,him]).

passive([a,small,apple,on,the,big,beautiful,table,will,be,being,bought,by,a,small,beautiful,woman]).
passive([the,apple,should,not,be,bought,by,the,beautiful,woman,on,the,big,beautiful,table]).


/*========================================================================
   Manual predicates
========================================================================*/

active :-
	readLine(ActiveS),
	!,
	write('ActiveS: '),write(ActiveS),write('\n'),
	convert(ActiveS,ActiveRe,PassiveS,PassiveRe),		
	write('ActiveRe: '),write(ActiveRe),write('\n'),
	write('PassiveS: '),write(PassiveS),write('\n'),
	write('PassiveRe: '),write(PassiveRe),write('\n').
	
passive :-
	readLine(PassiveS),
	!,
	write('PassiveS: '),write(PassiveS),write('\n'),
	convert(ActiveS,ActiveRe,PassiveS,PassiveRe),
	write('ActiveS: '),write(ActiveS),write('\n'),
	write('ActiveRe: '),write(ActiveRe),write('\n'),	
	write('PassiveRe: '),write(PassiveRe),write('\n').
	

/*========================================================================
   Testsuite predicates
========================================================================*/

activeTestSuite :-
	active(ActiveS),
	write('=========================================================='),write('\n'),
	write('ActiveS: '),write(ActiveS),write('\n'),
	convert(ActiveS,ActiveRe,PassiveS,PassiveRe),		
	write('ActiveRe: '),write(ActiveRe),write('\n'),
	write('PassiveS: '),write(PassiveS),write('\n'),
	write('PassiveRe: '),write(PassiveRe),write('\n'),
	write('\n'),
	fail.	
	
activeTestSuite.


passiveTestSuite :-
	passive(PassiveS),
	write('=========================================================='),write('\n'),
	write('PassiveS: '),write(PassiveS),write('\n'),
	convert(ActiveS,ActiveRe,PassiveS,PassiveRe),
	write('ActiveS: '),write(ActiveS),write('\n'),
	write('ActiveRe: '),write(ActiveRe),write('\n'),	
	write('PassiveRe: '),write(PassiveRe),write('\n'),
	write('\n'),
	fail.
	
passiveTestSuite.


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ----------------------------------------------------------------------------- <',[]),
   format('~n> testSuite.pl, by Tran Quang Trung                                             <',[]),
   format('~n>                                                                               <',[]),
   format('~n> ?- active.             - Type an active sentence and get the passive sentence <',[]),
   format('~n> ?- passive.            - Type a passive sentence and get the active sentence  <',[]),
   format('~n> ?- activeTestSuite.    - Test suite for from active to passive                <',[]),
   format('~n> ?- passiveTestSuite.   - Test suite for from passive to active                <',[]),   
   format('~n> ?- info.               - show this information                                <',[]),
   format('~n> ----------------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.