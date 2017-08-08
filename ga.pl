% Geneticke algoritmy v Prologu %


% Prechadza prveho a zaroven druheho rodica, kym nenarazi na poziciu krizenia(generovana nahodne pred volanim predikatu),
% vklada hodnoty prveho rodica do prveho potomka, a hodnoty druheho rodica do druheho potomka, ked prejde cez poziciu
% krizenia zacne hodnoty z prveho rodica ukladat do druheho potomka,a hodnoty z druheho rodica do prveho potomka

%krizenie(+PrvyRodic, +DruhyRodic, -PrvyPotomok, -DruhyPotomok, ?Index, +PoziciaKrizenia).
krizenie([],[], [], [], _, _):-!.
krizenie([X1|R1], [X2|R2], [X1|P1], [X2|P2], N, P) :- N=<P, M is N + 1, krizenie(R1,R2,P1,P2, M, P).
krizenie([X1|R1], [X2|R2], [X2|P1], [X1|P2], N, P) :- N>P, M is N + 1, krizenie(R1,R2,P1,P2, M, P).



% S pravdepodobnostou 1% mutuje kazdy "bit" jedinca.
% Generuje cislo od 1 do 100, ak je cislo mensie ako 1, prepise 1 na 0, alebo 0 na 1.

%mutacia(+Jedinec, +PravdepodobnostMutacie, -VyslednyJedinec)
mutacia([], _, []).
mutacia([X|Xs], P, [X|Y]):- P > 1, random_between(0, 100, P1), mutacia(Xs, P1, Y).
mutacia([X|Xs], P, [0|Y]):- P =< 1, X == 1, random_between(0, 100, P1), mutacia(Xs, P1, Y).
mutacia([X|Xs], P, [1|Y]):- P =< 1, X == 0, random_between(0, 100, P1), mutacia(Xs, P1, Y).



% Generuje 0 alebo 1 a pridava do zoznamu, kym nie je velkost zoznamu N.

%vytvor_jedinca(+VelkostJedinca, -Jedinec)
vytvor_jedinca(0, []).
vytvor_jedinca(N, [X|Z]):- M is N-1, random_between(0, 1, X), vytvor_jedinca(M, Z).



% Kym nie velkost populacie M, vola vytvor_jedinca.

%vytvor_populaciu(+VelkostPopulacie, +VelkostJedinca, -Populacia)
vytvor_populaciu(0, _, []).
vytvor_populaciu(VelkostPopulacie, VelkostJedinca, [Jedinec|Z]):- VelkostPopulacie2 is VelkostPopulacie - 1, vytvor_jedinca(VelkostJedinca, Jedinec),
																  vytvor_populaciu(VelkostPopulacie2, VelkostJedinca, Z).


% Vrati nahodne vygenerovany zoznam vah predmetov. Vahy predmetov su v rozsahu od 0 do 50.

%vahy_predmetov(+VelkostJedinca, -VahyPredmetov)
vahy_predmetov(0, []).
vahy_predmetov(N, [X|Z]):- M is N-1, random_between(0, 50, X), vahy_predmetov(M, Z).



% Pocita fitness u problemu lupeznikov.

%fitnessLupeznici(+Jedinec, +Vaha, +N, ?AkumulatorL1, ?AkumulatorL2, ?HodnotaL1, ?HodnotaL2, -Hodnota)
fitnessLupeznici([], _, N, HodnotaL1, HodnotaL2, HodnotaL1, HodnotaL2, Hodnota):-Hodnota is N - abs(HodnotaL1 - HodnotaL2).
fitnessLupeznici([X|Jedinec], [Y|Vaha], N, AkumulatorL1, AkumulatorL2, HodnotaL1, HodnotaL2, Hodnota):-
								X==1, AkumulatorL12 is AkumulatorL1 + Y, fitnessLupeznici(Jedinec, Vaha, N, AkumulatorL12, AkumulatorL2, HodnotaL1, HodnotaL2, Hodnota).
fitnessLupeznici([X|Jedinec], [Y|Vaha], N, AkumulatorL1, AkumulatorL2, HodnotaL1, HodnotaL2, Hodnota):- 
								X==0, AkumulatorL22 is AkumulatorL2 + Y, fitnessLupeznici(Jedinec, Vaha, N, AkumulatorL1, AkumulatorL22, HodnotaL1, HodnotaL2, Hodnota).


%fitnessPopulacieLupeznici(+Populacia, +VahyPredmetov, -Hodnoty)
fitnessPopulacieLupeznici([], _, []).
fitnessPopulacieLupeznici([Predmet|Populacia], VahyPredmetov, [Z|Hodnoty]):- sumFitness(VahyPredmetov, 0, Sum), fitnessLupeznici(Predmet, VahyPredmetov, Sum, 0, 0, X, Y, Z), fitnessPopulacieLupeznici(Populacia, VahyPredmetov, Hodnoty).




% Pomocou predikatu fitness, pocita a uklada do zoznamu Hodnoty fitness hodnoty jedincov.

%fitnessPopulacie(+Populacia, -ZoznamFitnessov)
fitnessPopulacie([], []).
fitnessPopulacie([Jedinec|ZbytokPopulacie], [FitnessJedinca|Zoznam]):-fitness(Jedinec, 0, FitnessJedinca), fitnessPopulacie(ZbytokPopulacie, Zoznam).


% Scitava pocet 1 v jedincovi.

%fitness(+Jedinec, ?Index, -Hodnota)
fitness([], Hodnota, Hodnota).
fitness([X|Jedinec], N, Hodnota):- X==1, M is N + 1, fitness(Jedinec, M, Hodnota).
fitness([X|Jedinec], N, Hodnota):- X==0, fitness(Jedinec, N, Hodnota).


% Scita vsetky fitness hodnoty jedincov.

%sumFitness(+Hodnoty, ?Akumulator, -Suma)
sumFitness([], Akumulator, Akumulator).
sumFitness([X|Hodnoty], Akumulator, Sum):- Akumulator2 is Akumulator + X, sumFitness(Hodnoty, Akumulator2, Sum).


% Spocita sumu vah predmetov u prveho a u druheho lupeznika. 

%sumyLupeznikov(+Jedinec, +Vaha, ?AkumulatorL1, ?AkumulatorL2, ?HodnotaL1, ?HodnotaL2)
sumyLupeznikov([], _, HodnotaL1, HodnotaL2, HodnotaL1, HodnotaL2).
sumyLupeznikov([X|Jedinec], [Y|Vaha], AkumulatorL1, AkumulatorL2, HodnotaL1, HodnotaL2):-
								X==1, AkumulatorL12 is AkumulatorL1 + Y, sumyLupeznikov(Jedinec, Vaha, AkumulatorL12, AkumulatorL2, HodnotaL1, HodnotaL2).
sumyLupeznikov([X|Jedinec], [Y|Vaha], AkumulatorL1, AkumulatorL2, HodnotaL1, HodnotaL2):- 
								X==0, AkumulatorL22 is AkumulatorL2 + Y, sumyLupeznikov(Jedinec, Vaha, AkumulatorL1, AkumulatorL22, HodnotaL1, HodnotaL2).



% Pred volanim tohto predikatu, sa nahodne vygeneruje Hranica - cislo od 0 po sumu vsetkych fitness hodnot.
% Hodnoty jedincov sa scitavaju, kym sa nescitaju na hodnotu vacsiu ako je hranica. Prvy jedinec, ktory dosiahne Hranicu,
% je vybrany do krizenia.

%ruleta(+Hodnoty, +Hranica, ?Suma, ?Akumulator, -Index)
ruleta([X|Hodnoty], Hranica, Suma, Akumulator, Index):- Suma2 is Suma + X, Suma2 >= Hranica, Index is Akumulator, !.
ruleta([X|Hodnoty], Hranica, Suma, Akumulator, Index):- Suma2 is Suma + X, Suma2 < Hranica, Akumulator2 is Akumulator + 1,
														ruleta(Hodnoty, Hranica, Suma2, Akumulator2, Index).


% Vybere prvych E jedincov zo zoznamu.

%vyberElitnych(+Populacia, +PocetVybranychJedincov, +ElitniJedinci)
vyberElitnych(_, 0, []).
vyberElitnych([Jedinec|Populacia], E, [Jedinec|ElitniJedinci]):- E1 is E - 1, vyberElitnych(Populacia, E1, ElitniJedinci).


% Pomocou upravaneho quick sortu zoradi zoznam %s fitness hodnotami jedincov Hodnoty a Populacia a pomocou predikatu vyberElitnych,
% vybere E najsilnejsich jedincov.

%elitizmus(+Populacia, +Hodnoty, +E, -ElitniJedinci)
elitizmus(Populacia, Hodnoty, E, ElitniJedinci):- quick_sort(Hodnoty, Populacia, ZoradeneHodnoty, ZoradenaPopulacia),
													   vyberElitnych(ZoradenaPopulacia, E, ElitniJedinci).


% Porovna dve hodnoty a vrati vacsiu aj %s jej indexom v zozname).

%porovnaj(+Fitness1, +Fitness2, +IndexJedinec1, +IndexJedinec2, -IndexSilnejsiehoJedinca, -FitnessSilnejsiehoJedinca)
porovnaj(Fitness1, Fitness2, Jedinec1, Jedinec2, SilnejsiJedinec, FitnessSilnejsiehoJedinca):-
												Fitness1=<Fitness2, SilnejsiJedinec is Jedinec2, FitnessSilnejsiehoJedinca is Fitness2,!.
porovnaj(Fitness1, Fitness2, Jedinec1, Jedinec2, SilnejsiJedinec, FitnessSilnejsiehoJedinca):-
												Fitness1>Fitness2, SilnejsiJedinec is Jedinec1, FitnessSilnejsiehoJedinca is Fitness1, !.
														 

% Vygeneruje 4 nahodne indexy jedincov, najde ich fitness hodnoty.
% Pomocou predikatu porovnaj porovna prvu dvojicu fitness, vrati prveho vitaza. Potom druhu dvojicu fitness, vrati druheho vitaza.
% A nakoniec porovna prveho a druheho vitaza a vrati konecneho vitaza, ktory je vybrany na krizenie.

%turnaj(+Populacia, +Hodnoty, +PocetJedincov, -PrvyRodic, -DruhyRodic)
turnaj(Populacia, Hodnoty, N, Rodic):- 
						random_between(1, N, Jedinec1), random_between(1, N, Jedinec2), 
						random_between(1, N, Jedinec3), random_between(1, N, Jedinec4),
						nth1(Jedinec1, Hodnoty, Fitness1), nth1(Jedinec2, Hodnoty, Fitness2),
						nth1(Jedinec3, Hodnoty, Fitness3), nth1(Jedinec4, Hodnoty, Fitness4),
						porovnaj(Fitness1, Fitness2, Jedinec1, Jedinec2, Index1, FitnessSilnejsiehoJedinca1),
						porovnaj(Fitness3, Fitness4, Jedinec3, Jedinec4, Index2, FitnessSilnejsiehoJedinca2),
						porovnaj(FitnessSilnejsiehoJedinca1, FitnessSilnejsiehoJedinca2, Index1, Index2, Index, FitnessSilnejsiehoJedinca),
						nth1(Index, Populacia, Rodic).




% Pomocou predikatov turnaj, krizenie a mutacia vytvara novu generaciu velkosti N zo starej populacie.

%vytvorenieDalsejGeneracie(+Populacia, +VelkostJedinca, +Hodnoty, +PocetJedincov, -Novapopulacia)
vytvorenieDalsejGeneracie(_, _, _,0,[]).
vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, N, [VyslednyPotomok1, VyslednyPotomok2 | NovaPopulacia]):- 
						turnaj(Populacia, Hodnoty, N, PrvyRodic),
						turnaj(Populacia, Hodnoty, N, DruhyRodic),
						random_between(1, VelkostJedinca, PoziciaKrizenia),
						krizenie(PrvyRodic, DruhyRodic, PrvyPotomok, DruhyPotomok, 1, PoziciaKrizenia),
						random_between(1, 100, P1),
						mutacia(PrvyPotomok, P1, VyslednyPotomok1),
						random_between(1, 100, P2),
						mutacia(DruhyPotomok, P2, VyslednyPotomok2),
						N2 is N - 1,
						vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, N2, NovaPopulacia).


% Pomocou predikatov ruleta, krizenie a mutacia vytvara novu generaciu velkosti N zo starej populacie.

%vytvorenieDalsejGeneracieRuleta(+Populacia, +VelkostJedinca, +Hodnoty, +PocetJedincov, -Novapopulacia)
vytvorenieDalsejGeneracieRuleta(_, _, _,0,[]).
vytvorenieDalsejGeneracieRuleta(Populacia, VelkostJedinca, Hodnoty, N, [VyslednyPotomok1, VyslednyPotomok2 | NovaPopulacia]):- 
						sumFitness(Hodnoty, 0, Sum),						
						random_between(0, Sum, Hranica1),						
						ruleta(Hodnoty, Hranica1, 0, 1, Index1),
						nth1(Index1, Populacia, PrvyRodic),
						random_between(VelkostJedinca, Sum, Hranica2),						
						ruleta(Hodnoty, Hranica2, 0, 1, Index2),
						nth1(Index2, Populacia, DruhyRodic),						 
						random_between(1, VelkostJedinca, PoziciaKrizenia),
						krizenie(PrvyRodic, DruhyRodic, PrvyPotomok, DruhyPotomok, 1, PoziciaKrizenia),
						random_between(1, 100, P1),
						mutacia(PrvyPotomok, P1, VyslednyPotomok1),
						random_between(1, 100, P2),
						mutacia(DruhyPotomok, P2, VyslednyPotomok2),
						N2 is N - 1,
						vytvorenieDalsejGeneracieRuleta(Populacia, VelkostJedinca, Hodnoty, N2, NovaPopulacia).



% Hlada index prvku v zozname.

%indexOf(+Zoznam, +Prvok, -Index)
indexOf([X|_], X, 1):- !.
indexOf([_|Xs], X, Index):- indexOf(Xs, X, Index1), !, Index is Index1+1.


% Pomocny predikat k najdiMaxIndex.

%najdiMax2(+Zoznam, ?AkumulatorMax, -Max, ?AkumulatorIndex, -Index)
najdiMax2([], _, Max, Max, Index, Index).
najdiMax2([X|Xs], PomocnyIndex, AkuMax, Max, AkuIndex, Index):- X > AkuMax,
																AkuMax2 is X,
																AkuIndex2 is PomocnyIndex,
																PomocnyIndex2 is PomocnyIndex + 1,
																najdiMax2(Xs, PomocnyIndex2, AkuMax2, Max, AkuIndex2, Index).
najdiMax2([X|Xs], PomocnyIndex, AkuMax, Max, AkuIndex, Index):- X =< AkuMax,
																PomocnyIndex2 is PomocnyIndex + 1,
																najdiMax2(Xs, PomocnyIndex2, AkuMax, Max, AkuIndex, Index).


% Hlada maximum v zozname a zaroven vrati aj jeho index.

%najdiMaxIndex(+Zoznam, -Max, -Index)
najdiMaxIndex(Zoznam, Max, Index):-najdiMax2(Zoznam, 1, 0, Max, 0, Index), !.



% Upraveny quick sort. Pomocou jedneho zoznamu zoraduje aj druhy.

%pivoting
pivoting(H, [], [], [], [], [], []).
pivoting(H, [X|T], [Jedinec|ZbytokPopulacie], [X|L], G, [Jedinec|SlabsiaPopulacia], SilnejsiaPopulacia):- X =< H, pivoting(H, T, ZbytokPopulacie, L, G, SlabsiaPopulacia, SilnejsiaPopulacia).
pivoting(H, [X|T], [Jedinec|ZbytokPopulacie], L, [X|G], SlabsiaPopulacia, [Jedinec|SilnejsiaPopulacia]):- X > H, pivoting(H, T, ZbytokPopulacie, L, G, SlabsiaPopulacia, SilnejsiaPopulacia).

%quick_sort(+Hodnoty, +Populacia, -ZoradeneHodnoty, -ZoradenaPopulacia)
quick_sort(List, Populacia, Sorted, ZoradenaPopulacia):- quick_sort2(List, Populacia, [], [], Sorted, ZoradenaPopulacia).

quick_sort2([], [], AccH, AccP, AccH, AccP).
quick_sort2([H|T], [J|Populacia], AccH, AccP, Sorted, ZoradenaPopulacia):- pivoting(H, T, Populacia, L1, L2, P1, P2),
								  quick_sort2(L1, P1, AccH, AccP, Sorted1, ZoradenaPopulacia1), quick_sort2(L2, P2, [H|Sorted1], [J|ZoradenaPopulacia1], Sorted, ZoradenaPopulacia).



%spojZoznamy(+Prvy, +Druhy, -Vysledny)
spojZoznamy([], Y, Y).
spojZoznamy([X|Xs], Y, [X|Vysledny]):-spojZoznamy(Xs, Y, Vysledny).



% Spocita fitness populacie, vytvori dalsiu generaciu, do akumulatoru si ulozi najsilnejsieho jedinca v novej populacii.
% Ked prebehne pocet generacii, kolko zadal uzivatel, do Vysledku zapise najsilnejsieho jedinca z poslednej generacie.

%cyklusTurnaj(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok)
cyklusTurnaj(_, 0, _, _, A, A).
cyklusTurnaj(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok):- 
														fitnessPopulacie(Populacia, Hodnoty),
														vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, N, NovaPopulacia),
														fitnessPopulacie(NovaPopulacia, Hodnoty2), 
														najdiMaxIndex(Hodnoty2, Max, Index),
                                                        nth1(Index, NovaPopulacia, Akumulator2),
														PocetGeneracii2 is PocetGeneracii - 1,
														cyklusTurnaj(NovaPopulacia, PocetGeneracii2, N, VelkostJedinca, Akumulator2, Vysledok).

% Lupeznici

%cyklusTurnajLupeznici(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok)
cyklusTurnajLupeznici(_, 0, _, _, _, A, A).
cyklusTurnajLupeznici(Populacia, PocetGeneracii, N, VelkostJedinca, VahyPredmetov, Akumulator, Vysledok):- 
														fitnessPopulacieLupeznici(Populacia, VahyPredmetov, Hodnoty),
														vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, N, NovaPopulacia),
														fitnessPopulacieLupeznici(NovaPopulacia, VahyPredmetov, Hodnoty2), 
														najdiMaxIndex(Hodnoty2, Max, Index),
                                                        nth1(Index, NovaPopulacia, Akumulator2),
														PocetGeneracii2 is PocetGeneracii - 1,
														cyklusTurnajLupeznici(NovaPopulacia, PocetGeneracii2, N, VelkostJedinca, VahyPredmetov, Akumulator2, Vysledok).



% plus elitizmus

%cyklusTurnajElitizmus(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok)
cyklusTurnajElitizmus(_, 0, _, _, A, A).
cyklusTurnajElitizmus(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok):- 
														fitnessPopulacie(Populacia, Hodnoty),
														E is round(N*5/100), M is N - E,
														vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, M, NovaPopulacia),
														elitizmus(Populacia, Hodnoty, E, ElitniJedinci),
														spojZoznamy(ElitniJedinci, NovaPopulacia, NovaPopulacia2),
														fitnessPopulacie(NovaPopulacia,Hodnoty2), 
														najdiMaxIndex(Hodnoty2, Max, Index),
                                                        nth1(Index, NovaPopulacia, Akumulator2),
														PocetGeneracii2 is PocetGeneracii - 1,
														cyklusTurnajElitizmus(NovaPopulacia, PocetGeneracii2, N, VelkostJedinca, Akumulator2, Vysledok).



% Ako cyklusTurnaj, ale zastavi sa, ked sa v populacii objavi maximalny jedinec. Nie vzdy pouzitelne, ak nevieme, ako vyzera maximum problemu,
% ktoreho riesenie hladame.
% Vyuziva to, ze vieme, ze maximum je Velkostjedinca.

%cyklusTurnajMax(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok, ?JeMax, -PocetKol)
cyklusTurnajMax(_, PocetGeneracii, _, _, A, A, 1, PocetGeneracii).
cyklusTurnajMax(_, 0, _, _, A, A, _, 0).
cyklusTurnajMax(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok, JeMax, PocetKol):-
														fitnessPopulacie(Populacia, Hodnoty), 
														not(member(VelkostJedinca, Hodnoty)),
														vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, N, NovaPopulacia),
														fitnessPopulacie(NovaPopulacia,Hodnoty2), 
														najdiMaxIndex(Hodnoty2, Max, Index),
                                                        nth1(Index, NovaPopulacia, Akumulator2),
														PocetGeneracii2 is PocetGeneracii - 1,
														JeMax2 is 0,
														cyklusTurnajMax(NovaPopulacia, PocetGeneracii2, N, VelkostJedinca, Akumulator2, Vysledok, JeMax2, PocetKol).
cyklusTurnajMax(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok, JeMax, PocetKol):-
														fitnessPopulacie(Populacia, Hodnoty), member(VelkostJedinca, Hodnoty),
														indexOf(Hodnoty, VelkostJedinca, Index), 
                                                        nth1(Index, Populacia, NajsilnejsiJedinec),
														PocetGeneracii2 is PocetGeneracii - 1,
														JeMax2 is 1,
														cyklusTurnajMax(Populacia, PocetGeneracii2, N, VelkostJedinca, NajsilnejsiJedinec, Vysledok, JeMax2, PocetKol).


% Navyse plus elitizmus.

%cyklusTurnajMaxElitizmus(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok, ?JeMax, -PocetKol)
cyklusTurnajMaxElitizmus(_, PocetGeneracii, _, _, A, A, 1, PocetGeneracii).
cyklusTurnajMaxElitizmus(_, 0, _, _, A, A, _, 0).
cyklusTurnajMaxElitizmus(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok, JeMax, PocetKol):-
												fitnessPopulacie(Populacia, Hodnoty), 
												not(member(VelkostJedinca, Hodnoty)),
												E is round(N*5/100), M is N - E,
												vytvorenieDalsejGeneracie(Populacia, VelkostJedinca, Hodnoty, M, NovaPopulacia),
												elitizmus(Populacia, Hodnoty, E, ElitniJedinci),
												spojZoznamy(ElitniJedinci, NovaPopulacia, NovaPopulacia2),
												fitnessPopulacie(NovaPopulacia2,Hodnoty2), 
												najdiMaxIndex(Hodnoty2, Max, Index),
												nth1(Index, NovaPopulacia2, Akumulator2),
												PocetGeneracii2 is PocetGeneracii - 1,
												JeMax2 is 0,
												cyklusTurnajMaxElitizmus(NovaPopulacia2, PocetGeneracii2, N, VelkostJedinca, Akumulator2, Vysledok, JeMax2, PocetKol).
cyklusTurnajMaxElitizmus(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok, JeMax, PocetKol):-
												fitnessPopulacie(Populacia, Hodnoty), member(VelkostJedinca, Hodnoty),
												indexOf(Hodnoty, VelkostJedinca, Index), 
												nth1(Index, Populacia, NajsilnejsiJedinec),
												PocetGeneracii2 is PocetGeneracii - 1,
												JeMax2 is 1,
												cyklusTurnajMaxElitizmus(Populacia, PocetGeneracii2, N, VelkostJedinca, NajsilnejsiJedinec, Vysledok, JeMax2, PocetKol).



% Zbytok ako cyklusTurnaj, len sa ako selekcia pouziva ruleta.

%cyklusRuleta(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok)
cyklusRuleta(_, 0, _, _, A, A).
cyklusRuleta(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok):- 
														fitnessPopulacie(Populacia, Hodnoty),
														vytvorenieDalsejGeneracieRuleta(Populacia, VelkostJedinca, Hodnoty, N, NovaPopulacia),
														fitnessPopulacie(NovaPopulacia,Hodnoty2), 
														najdiMaxIndex(Hodnoty2, Max, Index),
                                                        nth1(Index, NovaPopulacia, Akumulator2),
														PocetGeneracii2 is PocetGeneracii - 1,
														cyklusRuleta(NovaPopulacia, PocetGeneracii2, N, VelkostJedinca, Akumulator2, Vysledok).



%cyklusRuletaMax(+Populacia, +PocetGeneracii, +VelkostPopulacie, +VelkostJedinca, ?Akumulator, -Vysledok, ?JeMax, -PocetKol)
cyklusRuletaMax(_, PocetGeneracii, _, _, A, A, 1, PocetGeneracii).
cyklusRuletaMax(_, 0, _, _, A, A, _, 0).
cyklusRuletaMax(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok, JeMax, PocetKol):-
														fitnessPopulacie(Populacia, Hodnoty), 
														not(member(VelkostJedinca, Hodnoty)),
														vytvorenieDalsejGeneracieRuleta(Populacia, VelkostJedinca, Hodnoty, N, NovaPopulacia),
														fitnessPopulacie(NovaPopulacia,Hodnoty2), 
														najdiMaxIndex(Hodnoty2, Max, Index),
                                                        nth1(Index, NovaPopulacia, Akumulator2),
														PocetGeneracii2 is PocetGeneracii - 1,
														JeMax2 is 0,
														cyklusRuletaMax(NovaPopulacia, PocetGeneracii2, N, VelkostJedinca, Akumulator2, Vysledok, JeMax2, PocetKol).
cyklusRuletaMax(Populacia, PocetGeneracii, N, VelkostJedinca, Akumulator, Vysledok, JeMax, PocetKol):- 
														fitnessPopulacie(Populacia, Hodnoty), 
														member(VelkostJedinca, Hodnoty),
														indexOf(Hodnoty, VelkostJedinca, Index), 
                                                        nth1(Index, Populacia, NajsilnejsiJedinec),
														PocetGeneracii2 is PocetGeneracii - 1,
														JeMax2 is 1,
														cyklusRuletaMax(Populacia, PocetGeneracii2, N, VelkostJedinca, NajsilnejsiJedinec, Vysledok, JeMax2, PocetKol).





% Predikaty, ktore spusta uzivatel. Vytvoria novu populaciu a volaju cyklus, ktory vrati vysledok a v niektorych pripadoch aj, 
% v ktorej generacii sa nasiel(ak je maximalny).

%evoluciaTurnaj(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTurnaj(N, M, PocetGeneracii, Vysledok):-
		vytvor_populaciu(N, M, Populacia), cyklusTurnaj(Populacia, PocetGeneracii, N, M, Akumulator, Vysledok).

%evoluciaTurnajLupeznici(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec, -Populacia,-VahyPredmetov,-SumaVahPredmetov,-1.Lupeznik, -2.Lupeznik)
evoluciaTurnajLupeznici(N, M, PocetGeneracii, Vysledok, Populacia, VahyPredmetov, Sum, PrvyLupeznik, DruhyLupeznik):-
																vytvor_populaciu(N, M, Populacia),
																vahy_predmetov(M, VahyPredmetov),
																sumFitness(VahyPredmetov, 0, Sum),
																cyklusTurnajLupeznici(Populacia, PocetGeneracii, N, M, VahyPredmetov, Akumulator, Vysledok),
																sumyLupeznikov(Vysledok, VahyPredmetov, 0,0, PrvyLupeznik, DruhyLupeznik).


%evoluciaTurnajElitizmus(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTurnajElitizmus(N, M, PocetGeneracii, Vysledok):-vytvor_populaciu(N, M, Populacia), cyklusTurnajElitizmus(Populacia, PocetGeneracii, N, M, Akumulator, Vysledok).


%evoluciaTurnajMax(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec, -PocetPotrebnychKol)
evoluciaTurnajMax(N, M, PocetGeneracii, Vysledok, PocetKol):- vytvor_populaciu(N, M, Populacia),
															  cyklusTurnajMax(Populacia, PocetGeneracii, N, M, Akumulator, Vysledok, 0, PocetKol2),
															  PocetKol is PocetGeneracii - PocetKol2.

%evoluciaTurnajMaxElitizmus(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec, -PocetPotrebnychKol)
evoluciaTurnajMaxElitizmus(N, M, PocetGeneracii, Vysledok, PocetKol):- vytvor_populaciu(N, M, Populacia),
															  cyklusTurnajMaxElitizmus(Populacia, PocetGeneracii, N, M, Akumulator, Vysledok, 0, PocetKol2),
															  PocetKol is PocetGeneracii - PocetKol2.

%evoluciaRuleta(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec)
evoluciaRuleta(N, M, PocetGeneracii, Vysledok):-vytvor_populaciu(N, M, Populacia), cyklusRuleta(Populacia, PocetGeneracii, N, M, Akumulator, Vysledok).

%evoluciaRuletaMax(+VelkostPopulacie, +VelkostJedinca, +PocetGeneracii, -NajsilnejsiJedinec, -PocetPotrebnychKol)
evoluciaRuletaMax(N, M, PocetGeneracii, Vysledok, PocetKol):- vytvor_populaciu(N, M, Populacia),
														   cyklusRuletaMax(Populacia, PocetGeneracii, N, M, Akumulator, Vysledok, 0, PocetKol2),
														   PocetKol is PocetGeneracii - PocetKol2.


%evoluciaTest1(+PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTest1(PocetGeneracii, Vysledok):-cyklusTurnaj(
[[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0]],
PocetGeneracii, 10, 8, Akumulator, Vysledok).

%evoluciaTest2(+PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTest2(PocetGeneracii, Vysledok):-cyklusTurnaj(
[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],
PocetGeneracii, 10, 16, Akumulator, Vysledok).


%evoluciaTest3(+PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTest3(PocetGeneracii, Vysledok):-cyklusTurnaj(
[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]],
PocetGeneracii, 10, 32, Akumulator, Vysledok).

%% Lupeznici

%evoluciaTest4(+PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTest4(PocetGeneracii, Vysledok):-cyklusTurnajLupeznici(
[[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0]],
PocetGeneracii, 10, 8, [1,1,1,1,1,1,1,1], Akumulator, Vysledok).

%evoluciaTest5(+PocetGeneracii, -NajsilnejsiJedinec)
evoluciaTest5(PocetGeneracii, Vysledok):-cyklusTurnajLupeznici(
[[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[0,0,0,0,0,0,0,0],
[1,1,1,1,1,1,1,1],
[1,1,1,1,1,1,1,1],
[1,1,1,1,1,1,1,1],
[1,1,1,1,1,1,1,1],
[1,1,1,1,1,1,1,1]],
PocetGeneracii, 10, 8, [1,1,1,1,1,1,1,1], Akumulator, Vysledok).
