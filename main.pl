:- consult("data/happiness_data.pl").
:- consult("grammar.pl").


q(Ans) :-
	print("List of score attributes that you can inquire about:"), nl,
	print("Country, Region, Happiness Rank, Happiness Score, GDP per Capita, Family, Life Expectancy, Freedom, Government Corruption, Generosity, Dystopia Residual"), nl, nl,
	print("Sample Queries: what is the happiness rank of sweden?"), nl,
	print("                what is the gdp score of france."), nl,
	print("                what is a western europe country"), nl, nl,
	print("Please enter query in lowercase."), nl,
	write("Ask me: "), flush_output(current_output),
	readln(Ln),
	question(Ln,End, Ans),
	member(End,[[],['?'],['.']]).
