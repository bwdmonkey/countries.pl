% Start Program
q(Ans) :-
	print("List of attributes that you can inquire about:"),
	nl,
	print("Country, Region, Happiness Rank, Happiness Score, GDP per Capita, Family, Life Expectancy, Freedom, Government Corruption, Generosity, Dystopia Residual"),
	
	nl, nl,
	print("Sample Queries: What is the happiness rating of Sweden?"),
	nl,
	print("                What is the GDP of France."),
	nl,
	print("                What are the countries in Europe"),
	
	nl, nl,
	print("Please enter query in lowercase."),
	nl,
	write("Ask me: "), flush_output(current_output),
	readln(Ln),
	question(Ln,End, Ans),
	member(End,[[],['?'],['.']]).
	
% The type of the question
question([is | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
question([what | T0],T2,Ind) :-
    noun_phrase(T0,T1,Ind),
    mp(T1,T2,Ind).
% The following has "is" betten the noun_phrase and the mp:
question([what | T0],T2,Ind) :-
    noun_phrase(T0,[is|T1],Ind),
    mp(T1,T2,Ind).