:- discontiguous sibling/2.
:- discontiguous cousin/2.
:- discontiguous parent/2.
:- discontiguous grandparent/2.
:- discontiguous aunt_uncle/2.
:- discontiguous cousin/2.

% Relaciones familiares: padres, hermanos, abuelos, tíos y primos.
parent(juan, maria).
parent(juan, pedro).
parent(ana, maria).
parent(ana, pedro).
parent(marta, luis).
parent(carlos, ana).
parent(lucia, ana).
parent(carlos, marta).

% Relaciones de cónyuge (spouse)
spouse(juan, ana).
spouse(ana, juan).
spouse(carlos, lucia).
spouse(lucia, carlos).
spouse(marta, pedro).
spouse(pedro, marta).

cousin(juan, sofia).
sibling(juan, diego).

parent(antonio, juana).
grandparent(paolo, antonio).
grandparent(josefa, antonio).
aunt_uncle(mario, antonio).
aunt_uncle(carmen, antonio).
aunt_uncle(federico, antonio).

parent(camila, paula).
sibling(camila, tomas).
sibling(federico, camila).
cousin(camila, alberto).
cousin(camila, david).

% Definir abuelos de forma general (grandparents)
grandparent(X, Y) :- parent(X, Z), parent(Z, Y).

% Definir hermanos (siblings) de forma general
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.

% Definir tíos/tías (aunt/uncle) de forma general
aunt_uncle(X, Y) :- sibling(X, Parent), parent(Parent, Y).
aunt_uncle(X, Y) :- spouse(X, Sibling), sibling(Sibling, Parent), parent(Parent, Y).

% Definir primos/primas (cousins) de forma general
cousin(X, Y) :- parent(P1, X), parent(P2, Y), sibling(P1, P2).

% Cálculo del nivel de consanguinidad
levelConsanguinity(X, Y, 1) :- parent(X, Y).
levelConsanguinity(X, Y, 1) :- parent(Y, X).
levelConsanguinity(X, Y, 2) :- sibling(X, Y).
levelConsanguinity(X, Y, 2) :- sibling(Y, X).
levelConsanguinity(X, Y, 2) :- grandparent(X, Y).
levelConsanguinity(X, Y, 2) :- grandparent(Y, X).
levelConsanguinity(X, Y, 3) :- aunt_uncle(X, Y).
levelConsanguinity(X, Y, 3) :- aunt_uncle(Y, X).
levelConsanguinity(X, Y, 3) :- cousin(X, Y).
levelConsanguinity(X, Y, 3) :- cousin(Y, X).

% Agrupar familiares por nivel de consanguinidad
agrupar_familiares(Fallecido, Nivel1, Nivel2, Nivel3) :-
    findall(Y, (levelConsanguinity(Fallecido, Y, 1), Y \= Fallecido), Nivel1),
    findall(Y, (levelConsanguinity(Fallecido, Y, 2), Y \= Fallecido), Nivel2),
    findall(Y, (levelConsanguinity(Fallecido, Y, 3), Y \= Fallecido), Nivel3).

% Distribuir la herencia cuando los porcentajes superan el 100%
distribuir_herencia_ajustada(HerenciaTotal, PorcentajeTotal, Nivel1, Nivel2, Nivel3, Distribucion) :-
    Ajuste is 100 / PorcentajeTotal,
    maplist({Ajuste, HerenciaTotal}/[Persona, (Persona, Monto)]>>(
        Monto is (30 * Ajuste * HerenciaTotal / 100)
    ), Nivel1, DistribucionNivel1),
    maplist({Ajuste, HerenciaTotal}/[Persona, (Persona, Monto)]>>(
        Monto is (20 * Ajuste * HerenciaTotal / 100)
    ), Nivel2, DistribucionNivel2),
    maplist({Ajuste, HerenciaTotal}/[Persona, (Persona, Monto)]>>(
        Monto is (10 * Ajuste * HerenciaTotal / 100)
    ), Nivel3, DistribucionNivel3),
    append(DistribucionNivel1, DistribucionNivel2, ParcialDistribucion),
    append(ParcialDistribucion, DistribucionNivel3, Distribucion).

% Distribuir la herencia cuando el total es menor o igual a 100%
distribuir_herencia_simple(HerenciaTotal, Nivel1, Nivel2, Nivel3, Distribucion) :-
    maplist({HerenciaTotal}/[Persona, (Persona, Monto)]>>(
        Monto is (30 * HerenciaTotal / 100)
    ), Nivel1, DistribucionNivel1),
    maplist({HerenciaTotal}/[Persona, (Persona, Monto)]>>(
        Monto is (20 * HerenciaTotal / 100)
    ), Nivel2, DistribucionNivel2),
    maplist({HerenciaTotal}/[Persona, (Persona, Monto)]>>(
        Monto is (10 * HerenciaTotal / 100)
    ), Nivel3, DistribucionNivel3),
    append(DistribucionNivel1, DistribucionNivel2, ParcialDistribucion),
    append(ParcialDistribucion, DistribucionNivel3, Distribucion).

% Función principal para distribuir la herencia
distribuir_herencia(Fallecido, HerenciaTotal, Distribucion) :-
    agrupar_familiares(Fallecido, Nivel1, Nivel2, Nivel3),
    length(Nivel1, CantidadNivel1),
    length(Nivel2, CantidadNivel2),
    length(Nivel3, CantidadNivel3),
    PorcentajeTotal is CantidadNivel1 * 30 + CantidadNivel2 * 20 + CantidadNivel3 * 10,
    (PorcentajeTotal > 100 ->
        distribuir_herencia_ajustada(HerenciaTotal, PorcentajeTotal, Nivel1, Nivel2, Nivel3, Distribucion)
    ;
        distribuir_herencia_simple(HerenciaTotal, Nivel1, Nivel2, Nivel3, Distribucion)
    ).
