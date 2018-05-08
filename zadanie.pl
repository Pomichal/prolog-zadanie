%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Autor: Vajk Pomichal
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Program pracuje s predikatmi system(Meno), suciastka(Meno,CenaZaKus)
% a vztah(System,System,Mnozstvo) alebo vztah(System,Suciastka,Mnozstvo)
% systemy mozu byt usporiadane hierarchicky

:- dynamic system/1.
:- dynamic suciastka/2.
:- dynamic vztah/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hlavny cyklus programu
% main()

main:-	        %uz pri spusteni nacita databazu zo suboru db.txt
	citaj('db.txt'),
	repeat,		%hlavny cyklus
	menu,
	get(C),
	vykonaj(C),
	C == 57,
	uloz('db.txt'),
	writeln('Koniec prace.').

%%%
% hlavne menu
% menu()
menu:-
	nl,
	writeln("HLAVNE MENU"),
	writeln('1 - citanie zo suboru'),
	writeln('2 - ulozenie dat do suboru'),
	writeln('3 - moznosti vypisu'),
	writeln('4 - moznosti tvorby zaznamov'),
	writeln('9 - koniec prace systemu'),
	writeln('------------------------'),
	nl.

%%%
% vykonanie vybranej moznosti z hlavnej menu
% vykonaj(+Code)

vykonaj(49):-			% nacita subor, z ktoreho sa ma nacitat databaza
	read_string(_),
	writeln("Subor: "),
	read_atom(Subor),
	citaj(Subor),
	!.
vykonaj(50):-			% nacita subor, kam sa maju udaje zapisat
	read_string(_),
	writeln("Subor: "),
	read_atom(Subor),
	uloz(Subor),
	!.
vykonaj(51):-vypis_main,!.	% vstupi do menu pre vypis
vykonaj(52):-vytvor_main,!.	% vstupi do menu pre tvorbu zaznamov
vykonaj(57):-!.
vykonaj(_):-writeln('Pouzivaj len urcene znaky!').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na citanie zo suboru, najprv vyprazdni existujucu DB.
% citaj(+Subor)

citaj(Subor):-
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sluzi na zapis do textoveho suboru.
% uloz(+Subor)

uloz(Subor):-
	tell(Subor),
	uloz_systemy,
	uloz_suciastky,
	uloz_vztahy,
	told.

% sluzi na zapis systemov do suboru
% uloz_systemy()
uloz_systemy:-
	system(Meno),
	writeq(system(Meno)), %uloz na konci, tri typy
	write('.'),
	nl,
	fail.
uloz_systemy.

% sluzi na zapis suciastok do suboru
% uloz_suciastky()
uloz_suciastky:-
	suciastka(Meno,Cena),
	writeq(suciastka(Meno,Cena)), %uloz na konci, tri typy
	write('.'),
	nl,
	fail.
uloz_suciastky.

% sluzi na zapis vztahov do suboru
%uloz_vztahy()
uloz_vztahy:-
	vztah(Meno1,Meno2,Mnozstvo),
	writeq(vztah(Meno1,Meno2,Mnozstvo)),
	write('.'),
	nl,
	fail.
uloz_vztahy.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tvorba noveho zaznamu
% hlavny cyklus pre tvorbu zaznamov
% vytvor_main()
vytvor_main:-
	repeat,
	vytvor_menu,
	get(C),
	vykonaj_vytvor(C),
	C == 57.

% menu pre tvorbu zaznamov
%vytvor_menu()
vytvor_menu:-
	nl,
	writeln("VYTVOR MENU"),
	writeln('1 - vytvor novy system'),
	writeln('2 - pridaj podsystem k systemu'),
	writeln('3 - pridaj suciastku k systemu/podsystemu'),
	writeln('9 - navrat do hlavneho menu'),
	writeln('------------------------'),
	nl.

%vykonanie vybranej moznosti z menu zapisov
% vykonaj_vytvor(+Code)
vykonaj_vytvor(49):- 
	read_string(_),
	vytvor_system,!.
vykonaj_vytvor(50):- 
	read_string(_),
	pridaj_podsystem,!.
vykonaj_vytvor(51):- 
	read_string(_),
	pridaj_suciastku,!.
vykonaj_vytvor(57):-!.
vykonaj_vytvor(_):-writeln('Pouzivaj len urcene znaky!').
	
%vytvori novy system
%vytvor_system()
vytvor_system:-
	writeln("Meno systemu: "),
	read_string(Meno),
	assertz(system(Meno)).

% prida alebo vytvori novy podsystem k danemu systemu
% pridaj_podsystem()
pridaj_podsystem():-
	writeln("Meno systemu: "),
	read_string(Meno),
	(
	system(Meno),
	writeln("Meno podsystemu: "),
	read_string(Meno1),
	(
	system(Meno1)
	;
	assertz(system(Meno1))
	),
	writeln("mnozstvo:"),
	read_num(Mnozstvo),
	assertz(vztah(Meno,Meno1,Mnozstvo)),
	writeln("udaje boli zaevidovane")
	;
	writeln("system so zadanym menom neexistuje")
	).
	
% prida alebo vytvori suciastku k danemu systemu
% pridaj_suciastku()
pridaj_suciastku:-
	writeln("Meno systemu: "),
	read_string(Meno),
	(
	system(Meno),
	writeln("Meno suciastky: "),
	read_string(Meno1),
	(
	suciastka(Meno1,_)
	;
	writeln("Cena za kus: "),
	read_num(Cena),
	assertz(suciastka(Meno1,Cena))
	),
	writeln("mnozstvo:"),
	read_num(Mnozstvo),
	assertz(vztah(Meno,Meno1,Mnozstvo)),
	writeln("udaje boli zaevidovane")
	;
	writeln("system so zadanym menom neexistuje")
	).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% moznosti vypisu
% hlavny cyklus pre vypis
% vypis_main()
vypis_main:-
	repeat,
	vypis_menu,
	get(C),
	vykonaj_vypis(C),
	C == 57.

% menu pre vypis
% vypis_menu()
vypis_menu:-
	nl,
	writeln("VYPIS MENU"),
	writeln('1 - detailny vypis vsetkych systemov'),
	writeln('2 - najst system'),
	writeln('3 - najst suciastku'),
	writeln('9 - navrat do hlavneho menu'),
	writeln('------------------------'),
	nl.

% vykonanie vybranej moznosti z menu vypisov
% vykonaj_vypis(+Code)
vykonaj_vypis(49):- vypis,!.
vykonaj_vypis(50):- najdi_system,!.
vykonaj_vypis(51):- najdi_suciastku,!.
vykonaj_vypis(57):-!.
vykonaj_vypis(_):-writeln('Pouzivaj len urcene znaky!').
	
% vypis kazdeho systemu a jeho podsystemov a suciastok
% vypis()
vypis:-
	system(Meno),
	vypis_system(Meno),
	fail.
vypis.

% vypis jedneho systemu a jeho podsystemov a suciastok
% vypis_system(+MenoSystemu)
vypis_system(Meno):-
	writeln("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"),
	write(Meno),
	cena_systemu(Meno, Cena),
	write(", celkova cena: "),
	writeln(Cena),
	vztah(Meno,Cast,Mnozstvo),
	vypis_cast(Cast, Mnozstvo),
	fail.

% vypocita cenu systemu
% cena_systemu(+MenoSystemu,?Cena)
cena_systemu(Meno,Cena):-
	findall(Cast,(vztah(Meno,Cast,_), suciastka(Cast,_)),Zoz),
	cena_suciastok(Meno,Zoz,Cena1),
	findall(Cast1,(vztah(Meno,Cast1,_), system(Cast1)),Zoz1),
	cena_podsystemov(Meno,Zoz1,Cena2),
	Cena is Cena1 + Cena2.

% vypocita cenu podsystemov systemu
% cena_podsystemov(+MenoSystemu, +ZoznamPodsystemov, ?Cena)
cena_podsystemov(Meno,[H|T],Cena):-
	vztah(Meno,H,Mnozstvo),	
	cena_systemu(H,Cena1),
	cena_podsystemov(Meno,T,Cena2),
	Cena is Cena2 + Cena1 * Mnozstvo.
cena_podsystemov(_,[],0).

% vypocita cenu suciastok priamo podradene systemu
% cena_suciastok(+MenoSystemu, +ZoznamSuciastok, ?Cena)
cena_suciastok(Meno,[H|T],Cena):-
	suciastka(H,Hodnota),
	vztah(Meno,H,Mnozstvo),
	cena_suciastok(Meno,T,Cena1),
	Cena is Cena1 + Hodnota * Mnozstvo.
cena_suciastok(_,[],0).

% vypise cast systemu (podsystem alebo suciastku)
% vypis_cast(+Meno,+Mnozstvo)
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

% najde a vypise detaily o vybranom systeme
% najdi_system()
najdi_system:-
	read_atom(_),
	writeln("Zadaj meno systemu: "),
	read_string(Meno),
	system(Meno),
	vypis_system(Meno).

najdi_system.

% najde a vypise detaily o vybranej suciastke
% najdi_suciastku()
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
% Pomocne predikaty
% Predikat, ktory nacita string aj ked tam je velke zaciatocne pismeno
% read_string(?String) argument je ale vhodnejsie pouzivat ako vystupny

read_string(String):-
	current_input(Input),
	read_line_to_codes(Input,Codes),
	string_codes(String,Codes).

% Predikat sa vykonava opakovane kym pouzivatel nezada korektne cislo
% read_num(?Number) argument je velmi vhodne pouzivat len ako vystupny

read_num(Num) :-
	read_string(Str),
	number_string(Num, Str), !.

read_num(Num) :-
	write('\tMusite zadat cislo: '),
	read_num(Num).

% Konverzia retazca na atom
% read_atom(?Atom)

read_atom(A):-
	read_string(Str),
	atom_string(A,Str).
