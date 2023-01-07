% tipologia descritiva


posicao_inicial('PW', 'A', 2).
posicao_inicial('PW', 'B', 2).
posicao_inicial('PW', 'C', 2).
posicao_inicial('PW', 'D', 2).
posicao_inicial('PW', 'E', 2).
posicao_inicial('PW', 'F', 2).
posicao_inicial('PW', 'G', 2).
posicao_inicial('PW', 'H', 2).
posicao_inicial('TW', 'A', 1).
posicao_inicial('HW', 'B', 1).
posicao_inicial('BW', 'C', 1).
posicao_inicial('QW', 'D', 1).
posicao_inicial('KW', 'E', 1).
posicao_inicial('BW', 'F', 1).
posicao_inicial('HW', 'G', 1).
posicao_inicial('TW', 'H', 1).
posicao_inicial('PB', 'A', 7).
posicao_inicial('PB', 'B', 7).
posicao_inicial('PB', 'C', 7).
posicao_inicial('PB', 'D', 7).
posicao_inicial('PB', 'E', 7).
posicao_inicial('PB', 'F', 7).
posicao_inicial('PB', 'G', 7).
posicao_inicial('PB', 'H', 7).
posicao_inicial('TB', 'A', 8).
posicao_inicial('HB', 'B', 8).
posicao_inicial('BB', 'C', 8).
posicao_inicial('QB', 'D', 8).
posicao_inicial('KB', 'E', 8).
posicao_inicial('BB', 'F', 8).
posicao_inicial('HB', 'G', 8).
posicao_inicial('TB', 'H', 8).

linhas(19).
colunas(37).

valor(80, 100).
valor(66, 325).
valor(72, 325).
valor(84, 500).
valor(81, 1000).
valor(75, 20000).

%////////////////////////////////////////////////////////
%       LER STANDART INPUT
%////////////////////////////////////////////////////////
/*
:- initialization(jogar).    
:- initialization(comando).

comando :- argument_list(Arg), comando(Arg).
%comando(Arg) :- process_file.
comando([]) :- nl,readChar.
comando([A|AT]) :- write(A),write('|'),comando(AT).
readChar :- get0(Char),  process(Char,[]).
readChar(A) :- get0(Char),  process(Char,A).
process(-1,A) :- /*write("Adeus| |\n|\t"),*//*nl.
process(32,A) :-jogada(A), readChar([]).
%process(32,[]) :- readChar([]).
process(9,A) :- jogada(A), readChar([]).
%process(9,[]) :- readChar([]).
process(10,A) :- jogada(A), readChar([]).
%process(10,[]) :- readChar([]).
process(C,[]) :- readChar([C]).
process(C,T) :- append(T,[C],X), readChar(X).   */

%//////////////////////////////////////////////////////////
%       INICIALIZAR TABULEIRO
%//////////////////////////////////////////////////////////

:- dynamic(pos_act/3).

iniciar :- (posicao_inicial(P, X, Y), \+pos_act(P,X,Y),
        asserta(pos_act(P, X, Y)) ,iniciar) ; !.

jogar :- iniciar, desenhar.

%//////////////////////////////////////////////////////////
%       FUNÇÕES AUXILIARES
%//////////////////////////////////////////////////////////
% -- PREDICADO: frases/1

frases(F) :- engole(S), frases(F, S, _R), !.    % _R=resto (depois da frase)
frases(_) :- true.		                % haveria de dar erro... :-)

% -- PREDICADO: engole/1: engole stdin para um string

engole(S) :- get0(C), engole(C, S).

  engole(-1, []) :- !, seen.
  engole(C, [C|S]) :- engole(S).


% -- PREDICADO: frases/3 (nao-terminal frases/1)

frases([F|FS]) --> frase(F), frases(FS), !.
frases([F]) --> frase(F).

frase(PS) --> palavras(PS), ponto.

espaco --> " ".
espaco --> "\t".
espaco --> "\n".

virgula --> ",".

ponto --> ".", espacos.

espacos --> espaco, espacos.
espacos --> [].

letra(L) --> minuscula(L).
letra(L) --> maiuscula(L).

maiuscula(M) --> [L], { 0'A =< L, L =< 0'Z, !, M is L+0'a-0'A }.
minuscula(L) --> [L], { 0'a =< L, L =< 0'z }.

letras([L|LS]) --> letra(L), letras(LS).
letras([L]) --> letra(L), !.


palavra(P) --> letras(LS), { name(P, LS) }.

palavras([P|PS]) --> espacos, palavra(P), virgula, !, palavras(PS).
palavras([P|PS]) --> espacos, palavra(P), !, palavras(PS).
palavras([]) --> [].


comando :- argument_list(As), comando(As).

comando([]).
comando([A|As]) :- argumento(A), frases(F), mostra(F),
		   comando(As).

argumento('-') :- !, format("\n[standard input]\n\n", []).
argumento(F) :- file_exists(F), see(F), !,
		format("\n[file ~w]\n\n", [F]).
argumento(U) :- format("[nao existe: ~w]\n", [U]), !, halt.

mostra([]) :- format("\n[acabou]\n", []).
mostra([F|Fs]) :- mostra_frase(F), mostra(Fs).

mostra_frase(F) :- format("frase: <<", []), 
		   mostra_palavras(F), 
		   format(">>\n", []).

mostra_palavras([]).
mostra_palavras([P]) :- !, format("~w", [P]).
mostra_palavras([P|Ps]) :- format("~w ", [P]), mostra_palavras(Ps).

% -- VERIFICA A NECESSIDADE DE "|"

ver_traco(Pos, Max) :-  
        Max=<10, ((Y is Pos-4*Max, Y=1, !) ; (MaxN is Max+1, ver_traco(Pos, MaxN))).

% -- VERIFICA A NECESSIDADE DE " "

espaco_Peca(Pos, Max) :-
        Max=<10, (((Y is Pos-4*Max, Y=4, !)) ;
        (MaxN is Max+1, espaco_Peca(Pos, MaxN))).

% -- VERIFICA A PEÇA E ESCREVE-A

ver_Peca(Y, X) :- 
        Z is round(Y/2)-1, ASCII is round(63+(X+2)/4),
        char_code(Letra, ASCII), 
        pos_act(Peca, Letra,Z),
        write(Peca).

% -- VERIFICA A NECESSIDADE DE " " NA 1ª LINHA

ver_espaco(Pos, Max) :-
        Max=<10, (((Y is Pos-4*Max, Y=2, !) ;
        (Y is Pos-4*Max, Y=4, !)) ; (MaxN is Max+1, ver_espaco(Pos, MaxN))).

% -- VERERIFICA A LETRA E ESCREVE-A

ver_letra(Pos, Max) :-
    Max=<10, ((Y is Pos-4*Max, Y=3, !) ; (MaxN is Max+1, ver_letra(Pos, MaxN))).

digito(X) :- 
        (X=1;
        X=2;
        X=3;
        X=4;
        X=5;
        X=6;
        X=7).

%///////////////////////////////////////////////////////////
%       DESENHAR TABULEIRO
%///////////////////////////////////////////////////////////

desenhar :- desenhar_Tab(1).


desenhar_Tab(Linha):-(linhas(Z), Linha=<Z,
        ((Y is Linha mod 2, Y=1, colunas(X), print_Linha(X), nl) ;
        (Y is Linha mod 2, Y=0, print_Col(Linha, 2), nl)),
        LinhaN is Linha+1, desenhar_Tab(LinhaN)) ; !.

% -- DESENHAR "="

print_Linha(X) :- (X>0, write('='), print_Linha(X-1)) ; !.

% -- DESENHAR "|", " " E A LETRA DA 1ª lINHA 

print_Col(2, X) :-
        (colunas(C), X=<C,
        ((ver_traco(X, 0), write('|')) ; 
        (ver_espaco(X,0), write(' ')) ;
        ((X=3,write(' ')) ; (ver_letra(X, 0),
        ASCII is round(63+(X+1)/4), char_code(Letra, ASCII), write(Letra)))),
        Y is X+1, print_Col(2, Y)) ; !.

% -- DESENHAR "|", " " E A LETRA DO RESTO DO TABULEIRO

print_Col(Y, X) :- 
        (colunas(C), X=<C, 
        ((ver_traco(X, 0), write('|'), Z is X+1);
        ((X=2 ; espaco_Peca(X, 0)), write(' '), Z is X+1);
        (X=3, W is round(Y/2)-1, write(W), Z is X+1);
        (ver_Peca(Y,X), Z is X+2) ; (write(' '), Z is X+1)),
        print_Col(Y,Z)) ; !.

%///////////////////////////////////////////////////////////////////
%       REGRAS
%///////////////////////////////////////////////////////////////////
% -- PEÃO PRETO SEM COMER

regra([80,66], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        Xantes = Xdepois, 
        ((Z is Yantes+1, Ydepois=Z,
        ASCII is 64+Xdepois, char_code(Letra, ASCII),
        \+(pos_act(_, Letra, Ydepois))) ;
        (Ydepois=4, Yantes=2, ASCII is 64+Xdepois,
        char_code(Letra, ASCII), \+(pos_act(_, Letra, Ydepois)),
        W is Ydepois-1, !, \+(pos_act(_, Letra, W)))),
        Acao is 0.

% -- PEÃO PRETO A COMER

regra([80, 66], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        ((X is Xantes + 1, X = Xdepois);
        (X is Xantes - 1, X = Xdepois)),
        Y is Yantes + 1, Y = Ydepois,
        ASCII is 64 + Xdepois, char_code(Letra, ASCII),
        pos_act(Peca, Letra, Ydepois),
        name(P, [_,87]),
        Acao is 1.

% -- PEÃO BRANCO SEM COMER

regra([80,87], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        Xantes = Xdepois, 
        ((Z is Yantes-1, Ydepois=Z,
        ASCII is 64+Xdepois, char_code(Letra, ASCII),
        \+(pos_act(_, Letra, Ydepois))) ;
        (Ydepois=5, Yantes=7, ASCII is 64+Xdepois,
        char_code(Letra, ASCII), \+(pos_act(_, Letra, Ydepois)),
        W is Ydepois+1, !, \+(pos_act(_, Letra, W)))),
        Acao is 0.

% -- PEÃO BRANCO A COMER

regra([80,87], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        ((X is Xantes+1, X=Xdepois) ; (X is Xantes-1, X=Xdepois)),
        Y is Yantes+1, Y=Ydepois, 
        ASCII is 64+Xdepois, char_code(Letra, ASCII), 
        pos_act(Peca, Letra, Ydepois),
        name(Peca, [_, 66]), 
        Acao is 1.

% -- TORRE

regra([84,Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :- 
        ((var(Xdepois), (!, digito(Z), 
        ((Xdepois is Xantes+Z, Ydepois is Yantes) ; 
        (((Z=<Xantes, Xdepois is Xantes-Z) ;
        (Z>Xantes, Xdepois is Z-Xantes)), Ydepois is Yantes) ;
        (Ydepois is Yantes+Z, Xdepois is Xantes) ;
        (((Z=<Yantes, Ydepois is Yantes-Z) ;
        (Z>Yantes, Ydepois is Z-Yantes)), Xdepois is Xantes))),
        Xdepois>0, Xdepois=<8, Ydepois>0, Ydepois=<8) ; 
        (\+(var(Xdepois)), ((Xantes=Xdepois) ; 
        (Yantes=Ydepois)))), ASCII is 64+Xdepois, 
        char_code(Letra, ASCII), ((\+(pos_act(_, Letra, Ydepois)), 
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 0) ; 
        (pos_act(Peca, Letra, Ydepois), \+(name(Peca, [_,Cor])),
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 1)).

% -- BISPO

regra([66, Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        ((var(Xdepois), (!, digito(Z),
        ((Xdepois is Xantes+Z, (Ydepois is Yantes+Z ; Ydepois is Yantes-Z)) ;
        (Xdepois is Xantes-Z, (Ydepois is Yantes+Z ; Ydepois is Yantes-Z)))),
        Xdepois>0, Xdepois=<8, Ydepois>0, Ydepois=<8) ; 
        (\+(var(Xdepois)), (Dif1 is abs(Xantes-Xdepois),
        Dif2 is abs(Yantes-Ydepois),
        Dif1=Dif2))), ASCII is 64+Xdepois, char_code(Letra, ASCII),
        ((\+(pos_act(_, Letra, Ydepois)),
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 0) ; 
        (pos_act(Peca, Letra, Ydepois), \+(name(Peca, [_, Cor])),
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 1)).

% -- REI

regra([75, Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        ((var(Xdepois),
        ((Xdepois is Xantes+1, (Ydepois is Yantes+1 ; Ydepois is Yantes-1)) ;
        (Xdepois is Xantes-1, (Ydepois is Yantes+1 ; Ydepois is Yantes-1)) ;
        (Xdepois is Xantes+1, Ydepois is Yantes) ;
        (Ydepois is Yantes+1, Xdepois is Xantes) ;
        (Ydepois is Yantes-1, Xdepois is Xantes)),
        (Xdepois is Xantes-1, Ydepois is Yantes) ;
        Xdepois>0, Xdepois=<8, Ydepois>0, Ydepois=<8) ;
        (\+(var(Xdepois)), ((Dif1 is abs(Xantes-Xdepois),
        Dif2 is abs(Yantes-Ydepois), Dif1=Dif2, Dif1=1) ;
        (Xantes=Xdepois,
                ((Z is Yantes+1, Ydepois=Z) ; (Z is Yantes-1, Ydepois=Z))) ;
        (Yantes=Ydepois, 
                ((Z is Xantes+1, Xdepois=Z) ; (Z is Xantes-1, Xdepois=Z)))))),
        ASCII is 64+Xdepois, char_code(Letra, ASCII),
        ((\+(pos_act(_, Letra, Ydepois)), Acao is 0) ; 
        (pos_act(Peca, Letra, Ydepois), \+(name(Peca, [_, Cor])), Acao is 1)).

% -- RAINHA

regra([81, Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        ((var(Xdepois), (!, digito(Z), 
        ((Xdepois is Xantes+Z, (Ydepois is Yantes+Z ; Ydepois is Yantes-Z)) ;
        (Xdepois is Xantes-Z, (Ydepois is Yantes+Z ; Ydepois is Yantes-Z)) ;
        (Xdepois is Xantes+Z, Ydepois is Yantes) ; 
        (((Z=<Xantes, Xdepois is Xantes-Z) ;
        (Z>Xantes, Xdepois is Z-Xantes)), Ydepois is Yantes) ;
        (Ydepois is Yantes+Z, Xdepois is Xantes) ;
        (((Z=<Yantes, Ydepois is Yantes-Z) ; 
        (Z>Yantes, Ydepois is Z-Yantes)), Xdepois is Xantes))),
        Xdepois>0, Xdepois=<8, Yantes>0, Ydepois=<8) ; 
        (\+(var(Xdepois)), ((Dif1 is abs(Xantes-Xdepois),
        Dif2 is abs(Yantes-Ydepois), Dif1=Dif2) ;
        (Xantes=Xdepois) ; (Yantes=Ydepois)))), ASCII is 64+Xdepois,
        char_code(Letra, ASCII), ((\+(pos_act(_, Letra, Ydepois)),
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 0) ; 
        (pos_act(Peca, Letra, Ydepois),
        \+(name(Peca, [_, Cor])),
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 1)).

% -- CAVALO

regra([72, Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
        ((var(Xdepois), 
        ((Z is Xantes+1, Xdepois=Z, (W is Yantes+2 ; W is Yantes-2), Ydepois=W) ;
        (Z is Xantes+2, Xdepois=Z, (W is Yantes+1 ; W is Yantes-1), Ydepois=W) ;
        (Z is Xantes-2, Xdepois=Z, (W is Yantes+1 ; W is Yantes-1), Ydepois=W) ;
        (Z is Xantes-1, Xdepois=Z, (W is Yantes+1 ; W is Yantes-2)),
        Xdepois>0, Xdepois=<8, Ydepois>0, Ydepois=<8) ;
        (\+(var(Xdepois)), Dif1 is abs(Xantes-Xdepois),
        Dif2 is abs(Yantes-Ydepois),
        ((Dif1=1, Dif2=2) ; (Dif1=2, Dif2=1)))),
        ASCII is 64+Xdepois, char_code(Letra, ASCII),
        ((\+(pos_act(_, Letra, Ydepois)), Acao is 0) ;
        (pos_act(Peca, Letra, Ydepois), \+name(Peca, [_, Cor])),
        Acao is 1)).

%//////////////////////////////////////////////////////////////////////////////////////
%       REALIZAR JOGADAS
%//////////////////////////////////////////////////////////////////////////////////////

get([H|_],0,H).
get([_,T], Position, Elemento) :- NP is Position-1, get(T,NP,Elemento).


jogada(A):- length(A, X), jogada(A,X), write('Length: '),write(A),nl.
jogada([H|T],2) :- joga(H,T).
jogada([P1,P2,P3],3) :-  joga(P1,P2,P3).
jogada([P1,P2,P3,P4],4) :-  joga(P1,P2,P3,P4).
jogada([P1,P2,P3,P4,P5],5) :-  joga(P1,P2,P3,P4,P5).
joga(A,B). % A desenvolver
joga(Peca,X,Y). %A desenvolver

%//////////////////////////////////////////////////////////////////////////////////////
%       VERIFICAR A EXISTENCIA DE PEÇAS PELO CAMINHO
%//////////////////////////////////////////////////////////////////////////////////////
% -- VERIFICAR NA COLUNA

existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]) :-
        \+(Yantes=Ydepois), ASCII is 64+Xantes, char_code(Letra, ASCII),
        ((Ydepois>Yantes, Z is Yantes+1) ; (Ydepois<Yantes, Z is Yantes-1)),
        (Z=Ydepois ; \+(pos_act(_, Letra, Z))),
        existe_peca_no_caminho([Xantes, Z], [Xantes, Ydepois]).

% -- VERIFICAR NA DIAGONAL

existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]) :-
        Dif1 is abs(Xantes-Xdepois), 
        Dif2 is abs(Yantes-Ydepois), Dif1=Dif2,
        ((Xdepois>Xantes, Z is Xantes+1) ; (Xdepois<Xantes, Z is Xantes-1)),
        ((Ydepois>Yantes, W is Yantes+1) ; (Ydepois<Yantes, W is Yantes-1)),
        ASCII is 64+Z, char_code(Letra, ASCII), 
        ((W=Ydepois, Z=Xdepois) ; \+(pos_act(_, Letra, W))),
        existe_peca_no_caminho([Z, W], [Xdepois, Ydepois]).

% -- VERIFICAR NA LINHA

existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]) :-
        \+(Xantes=Xdepois), 
        ((Xdepois>Xantes, Z is Xantes+1) ; (Xdepois<Xantes, Z is Xantes-1)),
        ASCII is 64+Z, char_code(Letra, ASCII), 
        (Z=Xdepois ; \+(pos_act(_, Letra, Yantes))),
        existe_peca_no_caminho([Z, Yantes], [Xdepois, Yantes]).

% -- VERIFICAR FIM

existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]).