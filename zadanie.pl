%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mame predikat zakaznik so styrmi argumentami a potrebujeme
% povedat prologu, ze sa pocet jeho klauzul moze menit.
% zakaznik(Meno,Priezvisko,Adresa,Objednavka)

:- dynamic system/3.
:- dynamic podsystem/2.
:- dynamic suciastka/2.

%%%
% zadefinujeme operatory - len priklad

%:- op(800,xfy,::).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hlavny cyklus programu
% main()

main:-	        %uz pri spusteni moze nacitat databazu
	repeat,
	menu,
	get(C),
	vykonaj(C),
	C == 57,
	writeln('Koniec prace.').

%%%
% Menu rozsirite podla zadania
% menu
menu:-
	nl,
	writeln('1 - citanie zo suboru'),
	writeln('2 - zapis do suboru'),
	writeln('3 - vypis vsetkych zakaznikov'),
	writeln('9 - koniec prace systemu'),
	writeln('------------------------'),
	nl.

%%%
% vykonanie vybranej moznosti
% vykonaj(+Code)

vykonaj(49):-citaj('db.txt'),!.
%vykonaj(50):-zapis('db.txt'),!. %ocakava sa vlozenie mena suboru!
%vykonaj(51):-vypis,!.
vykonaj(57):-!.
vykonaj(_):-writeln('Pouzivaj len urcene znaky!').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na citanie zo suboru, najprv vyprazdni existujucu DB.
% citaj(+Subor)

citaj(S):-
	abolish(system/3),
	abolish(podsystem/2),
	abolish(suciastka/2),
	see(S),
	repeat,
	read(Term),
	(
	    Term = end_of_file,
	    !,
	    seen
	    ;
	    assertz(Term),
	    fail
	).

%%%
% Sluzi na zapis textoveho suboru.
% zapis(+Subor)

%zapis(S):-
%	tell(S),
%	zakaznik(Meno,Priezvisko,Adresa,Objednavka),
%	writeq(zakaznik(Meno,Priezvisko,Adresa,Objednavka)),
%	write('.'),
%	nl,
%	fail.
%zapis(_):-told.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na vypis
% vypis()

%vypis:-
%	zakaznik(Meno,Priezvisko,Adresa,Objednavka),
%	write(Meno),
%	write(", "),
%	write(Priezvisko),
%	write(", "),
%	write(Adresa),
%	write(", "),
%	writeln(Objednavka),
%	fail.
%vypis.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pomocne predikaty - priklady
% Predikat, ktory nacita string aj ked tam je velke zaciatocne pismeno
% read_string(?String) argument je ale vhodnejsie pouzivat ako vystupny

%read_string(String):-
%	current_input(Input),
%	read_line_to_codes(Input,Codes),
%	string_codes(String,Codes).

%%%
% Predikat sa vykonava opakovane kym pouzivatel nezada korektne cislo
% read_num(?Number) argument je velmi vhodne pouzivat len ako vystupny

%read_num(Num) :-
%	read_string(Str),
%	number_string(Num, Str), !.
%
%read_num(Num) :-
%	write('\tMusite zadat cislo: '),
%	read_num(Num).
%

%%%
% Konverzia retazca na atom
% read_atom(?Atom)

%read_atom(A):-
%	read_string(Str),
%	atom_string(A,Str).
%
%%%
% Najde vsetky riesenia pre dany ciel
% findall(+Template, :Goal, -Bag)
% vrati zoznam Mien a Priezvisk pre vsetkych zakaznikov v databaze
% findall(M^P , zakaznik(M,P,A,O), List).
% findall(M-P-A>O,zakaznik(M,P,A,O),List).
