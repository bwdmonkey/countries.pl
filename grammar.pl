% IMPORTANT: country data imported must in the format of:
% country(name,region,happiness_rank,happiness_score,gdp_per_capita,family,
%         life_expectancy,freedom,government_corruption,generosity,dystopia_residual)
:- consult("data/happiness_data.pl").

% Ind is individual noun phrase is referring to
% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
    det(T0,T1,Ind),
    adjectives(T1,T2,Ind),
    noun(T2,T3,Ind),
    mp(T3,T4,Ind).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T,_).
det([a | T],T,_).
det([an | T],T,_).
det(T,T,_).

% adjectives(T0,T1,Ind) is true if
% T0-T1 is an adjective is true of Ind
adjectives(T0,T2,Ind) :-
    adj(T0,T1,Ind),
    adjectives(T1,T2,Ind).
adjectives(T,T,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing
mp(T0,T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).
mp([that|T0],T2,Subject) :-
    reln(T0,T1,Subject,Object),
    noun_phrase(T1,T2,Object).

mp(T,T,_).

% DICTIONARY
adj([happy | T],T,Obj) :- happy(Obj).
adj([unhappy | T],T,Obj) :- unhappy(Obj).
adj([poor | T],T,Obj) :- poor(Obj).
adj([low,gdp | T],T,Obj) :- poor(Obj).
adj([wealthy | T],T,Obj) :- wealthy(Obj).
adj([high,gdp | T],T,Obj) :- wealthy(Obj).

region([western,europe | T],T,Obj) :- country(Obj,western_europe,_,_,_,_,_,_,_,_,_).
region([australia,and,new,zealand | T],T,Obj) :- country(Obj,australia_and_new_zealand,_,_,_,_,_,_,_,_,_).
region([middle,east,and,northern_africa | T],T,Obj) :- country(Obj,middle_east_and_northern_africa,_,_,_,_,_,_,_,_,_).
region([north,america | T],T,Obj) :- country(Obj,north_america,_,_,_,_,_,_,_,_,_).
region([latin,america,and,caribbean | T],T,Obj) :- country(Obj,latin_america_and_caribbean,_,_,_,_,_,_,_,_,_).
region([southeastern,asia | T],T,Obj) :- country(Obj,southeastern_asia,_,_,_,_,_,_,_,_,_).
region([central,and,eastern,europe | T],T,Obj) :- country(Obj,central_and_eastern_europe,_,_,_,_,_,_,_,_,_).
region([middle,east,and,northern,africa | T],T,Obj) :- country(Obj,middle_east_and_northern_africa,_,_,_,_,_,_,_,_,_).
region([eastern,asia | T],T,Obj) :- country(Obj,eastern_asia,_,_,_,_,_,_,_,_,_).
region([sub,saharan,africa | T],T,Obj) :- country(Obj,sub_saharan_africa,_,_,_,_,_,_,_,_,_).

noun([country | T],T,Obj) :- country(Obj,_,_,_,_,_,_,_,_,_,_).
noun([X | T],T,X) :- country(X,_,_,_,_,_,_,_,_,_,_).
noun([region | T],T,Obj) :- country(_,Obj,_,_,_,_,_,_,_,_,_).
noun([X | T],T,X):- country(_,X,_,_,_,_,_,_,_,_,_).

reln([the,region,of | T],T,O1,O2) :- country(O1,O2,_,_,_,_,_,_,_,_,_).
reln([the,continent,of | T],T,O1,O2) :- country(O2,O1,_,_,_,_,_,_,_,_,_).
reln([the,countries,in | T], T, O1, O2) :- country(O1,O2,_,_,_,_,_,_,_,_,_).
reln([country,is,in | T],T,01,O2) :- country(01,O2,_,_,_,_,_,_,_,_,_).
reln([the,happiness,rank,of | T],T,_01,O2) :- country(O2,_,_01,_,_,_,_,_,_,_,_).
reln([the,happiness,score,of | T],T,_01,O2) :- country(O2,_,_,_01,_,_,_,_,_,_,_).
reln([the,gdp,score,of | T],T,_01,O2) :- country(O2,_,_,_,_01,_,_,_,_,_,_).
reln([the,family,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_01,_,_,_,_,_).
reln([the,life,expectancy,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_01,_,_,_,_).
reln([the,freedom,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_,_01,_,_,_).
reln([the,government,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_,_,_01,_,_).
reln([the,corruption,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_,_,_01,_,_).
reln([the,government,corruption,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_,_,_01,_,_).
reln([the,generosity,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_,_,_,_01,_).
reln([the,dystopia,residual,score,of | T],T,_01,O2) :- country(O2,_,_,_,_,_,_,_,_,_,_01).

% question(Question,QR,Object) is true if Query provides an answer about Object to Question
question([is | T0],T2,Obj) :-
    noun_phrase(T0,T1,Obj),
    mp(T1,T2,Obj).
question([what,is | T0], T1, Obj) :-
    mp(T0,T1,Obj).
question([which, are | T0], T1, Obj) :-
	mp(T0, T1, Obj).
question([what,is | T0],T1,Obj) :-
    noun_phrase(T0,T1,Obj).
question([what,country,is,in | T0],T1,Ind) :-
    region(T0,T1,Ind).
question([what,country,is| T0],T1,Ind) :-
    adjectives(T0,T1,Ind).
question([are | T0],T2,Obj) :-
    noun_phrase(T0,T1,Obj),
    mp(T1,T2,Obj).
question([what,are | T0], T1, Obj) :-
    mp(T0,T1,Obj).
question([what,are | T0],T1,Obj) :-
    noun_phrase(T0,T1,Obj).
question([what,countries,are,in | T0],T1,Ind) :-
    region(T0,T1,Ind).
question([what,countries,are| T0],T1,Ind) :-
    adjectives(T0,T1,Ind).
question([what | T0],T2,Obj) :-
    noun_phrase(T0,T1,Obj),
    mp(T1,T2,Obj).

% Attributes

happy(X):-
    country(X,_,B,_,_,_,_,_,_,_,_),
    B =< 20.

unhappy(X):-
    country(X,_,B,_,_,_,_,_,_,_,_),
    B >= 130.

wealthy(X):-
    country(X,_,_,_,C,_,_,_,_,_,_),
    C >= 1.5.

poor(X):-
    country(X,_,_,_,C,_,_,_,_,_,_),
    C < 0.5.


% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    question(Q,[],A).

/* Try the following queries:
?- ask([what,is,a,country],A).
?- ask([what,country,is,happy],A).
?- ask([what,is,a,happy,country],A).
?- ask([what,is,a,wealthy,country],A).
?- ask([what,is,a,high,gdp,country],A).
?- ask([what,is,the,region,of,chile],A).
?- ask([what,is,the,continent,of,canada],A).
?- ask([what,is,the,gdp,score,of,argentina],A).
?- ask([what,is,the,family,score,of,argentina],A).
?- ask([what,country,is,in,north_america],A).

% Does not work yet [WIP]
?- ask([what,is,a,country,that,is,the,region,of,western_europe],A).
?- ask([what,is,a,country,that,is,in,the,same,region,as,the,region,of,denmark],A).
?- ask([what,is,the,gdp,of,a,country,that, in,the,region,western_europe],A).
?- ask([what,country,is,in,the,region,north_america],A).
*/

% Tests - To run tests `run_tests.`
:- begin_tests(grammar).

% Basic attribute score tests - deterministic
test(denmark_continent, [nondet]) :-
    ask([what,is,the,continent,of,denmark],A),
    assertion(A == western_europe).
test(denmark_happiness_rank, [nondet]) :-
    ask([what,is,the,happiness,rank,of,denmark],A),
    assertion(A == 1).
test(denmark_happiness_score, [nondet]) :-
    ask([what,is,the,happiness,score,of,denmark],A),
    assertion(A == 7.526).
test(denmark_gdp_score, [nondet]) :-
    ask([what,is,the,gdp,score,of,denmark],A),
    assertion(A == 1.44178).
test(denmark_family_score, [nondet]) :-
    ask([what,is,the,family,score,of,denmark],A),
    assertion(A == 1.16374).
test(denmark_life_expectancy_score, [nondet]) :-
    ask([what,is,the,life,expectancy,score,of,denmark],A),
    assertion(A == 0.79504).
test(denmark_freedom_score, [nondet]) :-
    ask([what,is,the,freedom,score,of,denmark],A),
    assertion(A == 0.57941).
test(denmark_corruption_score, [nondet]) :-
    ask([what,is,the,corruption,score,of,denmark],A),
    assertion(A == 0.44453).
test(denmark_government_score, [nondet]) :-
    ask([what,is,the,government,score,of,denmark],A),
    assertion(A == 0.44453).
test(denmark_government_corruption_score, [nondet]) :-
    ask([what,is,the,government,corruption,score,of,denmark],A),
    assertion(A == 0.44453).
test(denmark_generosity_score, [nondet]) :-
    ask([what,is,the,generosity,score,of,denmark],A),
    assertion(A == 0.36171).
test(denmark_dystopia_residual_score, [nondet]) :-
    ask([what,is,the,dystopia,residual,score,of,denmark],A),
    assertion(A == 2.73939).

% More complex continent country relationship test
test(north_america_countries, all(A == [canada, united_states])) :-
    region([north,america],[],A).
test(north_america_countries_query_is, all(A == [canada, united_states])) :-
    ask([what,country,is,in,north,america],A).
test(north_america_countries_query_are, all(A == [canada, united_states])) :-
    ask([what,countries,are,in,north,america],A).

test(happy_countries, all(A == [denmark, switzerland, iceland, norway, finland, canada, netherlands, new_zealand, australia, sweden, israel, austria, united_states, costa_rica, puerto_rico, germany, brazil, belgium, ireland, luxembourg])) :-
    ask([what, country, is, happy],A).

:- end_tests(grammar).
