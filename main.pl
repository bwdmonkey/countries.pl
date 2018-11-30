:- consult("grammar.pl").

q(Ans) :-
	print("List of score attributes that you can inquire about:"), nl,
	print("Country, Region, Happiness Rank, Happiness Score, GDP score, Family score,"),nl,
	print("Life Expectancy score, Freedom score, Government Corruption score, "),nl,
	print("Generosity score, Dystopia Residual score"), nl, nl,
	print("Sample Queries: what is the happiness rank of sweden?"), nl,
	print("                what is the gdp score of france."), nl,
	print("                what countries are in western europe"), nl, nl,
	print("Please enter query in lowercase."), nl,
	write("Ask me: "), flush_output(current_output),
	readln(Ln),
	question(Ln,End, Ans),
	member(End,[[],['?'],['.']]).
