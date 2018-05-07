%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Mame predikat zakaznik so styrmi argumentami a potrebujeme
% povedat prologu, ze sa pocet jeho klauzul moze menit.
% zakaznik(Meno,Priezvisko,Adresa,Objednavka)

:- dynamic system/1.
:- dynamic suciastka/2.
:- dynamic vztah/3.

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
	uloz('db.txt'),
	writeln('Koniec prace.').

%%%
% Menu rozsirite podla zadania
% menu
menu:-
	nl,
	writeln("HLAVNE MENU"),
	writeln('1 - citanie zo suboru'),
	writeln('2 - ulozenie dat do suboru'),
	writeln('3 - moznosti vypisu'),
	writeln('9 - koniec prace systemu'),
	writeln('------------------------'),
	nl.

%%%
% vykonanie vybranej moznosti
% vykonaj(+Code)

vykonaj(49):-
	read_string(_),
	citaj,
	!.
vykonaj(50):-
	writeln("Subor: "),
	read_atom(Subor),
	uloz(Subor),
	!.
vykonaj(51):-vypis_main,!.
vykonaj(57):-!.
vykonaj(_):-writeln('Pouzivaj len urcene znaky!').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na citanie zo suboru, najprv vyprazdni existujucu DB.
% citaj(+Subor)

citaj:-
	writeln("Subor: "),
	read_atom(Subor),
	abolish(system/1),
	abolish(suciastka/2),
	abolish(vztah/3),
	see(Subor),
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

uloz(Subor):-
	tell(Subor),
	uloz_systemy,
	uloz_suciastky,
	uloz_vztahy,
	told.

uloz_systemy:-
	system(Meno),
	writeq(system(Meno)), %uloz na konci, tri typy
	write('.'),
	nl,
	fail.
uloz_systemy.

uloz_suciastky:-
	suciastka(Meno,Cena),
	writeq(suciastka(Meno,Cena)), %uloz na konci, tri typy
	write('.'),
	nl,
	fail.
uloz_suciastky.

uloz_vztahy:-
	vztah(Meno1,Meno2,Mnozstvo),
	writeq(vztah(Meno1,Meno2,Mnozstvo)),
	write('.'),
	nl,
	fail.
uloz_vztahy.

	

%po case zapis
%write(daco),
%read_string(Meno),
%POcet
%assertz(system(Meno)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na vypis
% vypis()

vypis_main:-
	repeat,
	vypis_menu,
	get(C),
	vykonaj_vypis(C),
	C == 57.

vypis_menu:-
	nl,
	writeln("VYPIS MENU"),
	writeln('1 - detailny vypis vsetkych systemov'),
	writeln('2 - najst system'),
	writeln('3 - najst suciastku'),
	writeln('9 - navrat do hlavneho menu'),
	writeln('------------------------'),
	nl.

vykonaj_vypis(49):- vypis,!.
vykonaj_vypis(50):- najdi_system,!.
vykonaj_vypis(51):- najdi_suciastku,!.
vykonaj_vypis(57):-!.
vykonaj_vypis(_):-writeln('Pouzivaj len urcene znaky!').
	
%%%%%%%%%
%vypis celeho stromu

vypis:-
	system(Meno),
	vypis_system(Meno),
	fail.
vypis.

vypis_system(Meno):-
	writeln("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),
	write(Meno),
	cena_systemu(Meno, Cena),
	write(", celkova cena: "),
	writeln(Cena),
	vztah(Meno,Cast,Mnozstvo),
	vypis_cast(Cast, Mnozstvo),
	fail.

cena_systemu(Meno,Cena):-
	findall(Cast,(vztah(Meno,Cast,_), suciastka(Cast,_)),Zoz),
	cena_suciastok(Meno,Zoz,Cena1),
	findall(Cast1,(vztah(Meno,Cast1,_), system(Cast1)),Zoz1),
	cena_podsystemov(Meno,Zoz1,Cena2),
	Cena is Cena1 + Cena2.

cena_podsystemov(Meno,[H|T],Cena):-
	vztah(Meno,H,Mnozstvo),	
	cena_systemu(H,Cena1),
	cena_podsystemov(Meno,T,Cena2),
	Cena is Cena2 + Cena1 * Mnozstvo.
cena_podsystemov(_,[],0).

cena_suciastok(Meno,[H|T],Cena):-
	suciastka(H,Hodnota),
	vztah(Meno,H,Mnozstvo),
	cena_suciastok(Meno,T,Cena1),
	Cena is Cena1 + Hodnota * Mnozstvo.
cena_suciastok(_,[],0).

vypis_cast(Meno, Mnozstvo):-
	(
	system(Meno),
	write(Meno),
	write(" "),
	write(Mnozstvo),
	writeln(" krat"),
	vztah(Meno,X,Mnozstvo1),
	write("	"),
	vypis_cast(X, Mnozstvo1)
	;
	suciastka(Meno,Cena),
	write(Meno),
	write(", "),
	write(Mnozstvo),
	write(" krat, "),
	write("cena za kus: "),
	write(Cena),
	writeln("")
	).	


%%%%
%najdi a vypis detaily o systeme

najdi_system:-
	read_atom(_),
	writeln("Zadaj meno systemu: "),
	read_string(Meno),
	system(Meno),
	vypis_system(Meno).

najdi_system.

%%%
%najdi a vypis detaily o suciastke
najdi_suciastku:-
	read_atom(_),
	writeln("Zadaj meno suciastky: "),
	read_string(Meno),
	suciastka(Meno,Cena),
	write(Meno),
	write(", cena za kus: "),
	writeln(Cena).
najdi_suciastku.

	
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pomocne predikaty - priklady
% Predikat, ktory nacita string aj ked tam je velke zaciatocne pismeno
% read_string(?String) argument je ale vhodnejsie pouzivat ako vystupny

read_string(String):-
	current_input(Input),
	read_line_to_codes(Input,Codes),
	string_codes(String,Codes).

%%%
% Predikat sa vykonava opakovane kym pouzivatel nezada korektne cislo
% read_num(?Number) argument je velmi vhodne pouzivat len ako vystupny

read_num(Num) :-
	read_string(Str),
	number_string(Num, Str), !.

read_num(Num) :-
	write('\tMusite zadat cislo: '),
	read_num(Num).
%

%%%
% Konverzia retazca na atom
% read_atom(?Atom)

read_atom(A):-
	read_string(Str),
	atom_string(A,Str).
%
%%%
% Najde vsetky riesenia pre dany ciel
% findall(+Template, :Goal, -Bag)
% vrati zoznam Mien a Priezvisk pre vsetkych zakaznikov v databaze
% findall(M^P , zakaznik(M,P,A,O), List).
% findall(M-P-A>O,zakaznik(M,P,A,O),List).
