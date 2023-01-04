% tipologia descritiva

/*coluna('A').
coluna('B').
coluna('C').
coluna('D').
coluna('E').
coluna('F').
coluna('G').
coluna('H').
linha(1).
linha(2).
linha(3).
linha(4).
linha(5).
linha(6).
linha(7).
linha(8).
celula(X,Y) :- coluna(X), linha(Y).*/


posicao_inicial('PB', 'A', 2).
posicao_inicial('PB', 'B', 2).
posicao_inicial('PB', 'C', 2).
posicao_inicial('PB', 'D', 2).
posicao_inicial('PB', 'E', 2).
posicao_inicial('PB', 'F', 2).
posicao_inicial('PB', 'G', 2).
posicao_inicial('PB', 'H', 2).
posicao_inicial('TB', 'A', 1).
posicao_inicial('HB', 'B', 1).
posicao_inicial('BB', 'C', 1).
posicao_inicial('KB', 'D', 1).
posicao_inicial('QB', 'E', 1).
posicao_inicial('BB', 'F', 1).
posicao_inicial('HB', 'G', 1).
posicao_inicial('TB', 'H', 1).
posicao_inicial('PW', 'A', 7).
posicao_inicial('PW', 'B', 7).
posicao_inicial('PW', 'C', 7).
posicao_inicial('PW', 'D', 7).
posicao_inicial('PW', 'E', 7).
posicao_inicial('PW', 'F', 7).
posicao_inicial('PW', 'G', 7).
posicao_inicial('PW', 'H', 7).
posicao_inicial('TW', 'A', 8).
posicao_inicial('HW', 'B', 8).
posicao_inicial('BW', 'C', 8).
posicao_inicial('KW', 'D', 8).
posicao_inicial('QW', 'E', 8).
posicao_inicial('BW', 'F', 8).
posicao_inicial('HW', 'G', 8).
posicao_inicial('TW', 'H', 8).
linhas(19).
colunas(37).
valor(80, 100).
valor(66, 325).
valor(72, 325).
valor(84, 500).
valor(81, 1000).
valor(75, 20000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        INICIAR TABULEIRO        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(posicao_act/3).

iniciar :- (posicao_inicial(P, X, Y),\+posicao_act(P,X,Y),
        asserta(posicao_act(P, X, Y)),iniciar);!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       DESENHAR TABULEIRO        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jogar :- iniciar, desenhar.

desenhar :- desenhar_Tab(1).

desenhar_Tab(Linha):-(linhas(Z), Linha=<Z,
        ((Y is Linha mod 2, Y=1, colunas(X), print_Linha(X), nl);
        (Y is Linha mod 2, Y=0, print_Col(Linha,2), nl)),
        LinhaN is Linha+1, desenhar_Tab(LinhaN)); !.

print_Linha(X) :- (X>1,write('='), print_Linha(X-1)); !.

print_Col(2, X) :-
        (colunas(C), X=<C,
        ((ver_traco(X,0), write('|')); 
        (espacoBoard(X,0), write(' '));
        ((X=3,write(' ')); (ver_letra(X,0),
        ASCII is truncate(63+(X+1)/4), 
        char_code(Letra,ASCII), write(Letra)))),
        Y is X+1, print_Col(2, Y)); !.

print_Col(Y, X) :- 
        (colunas(C), X=<C, 
        ((ver_traco(X,0),write('|'), Z is X+1);
        ((X=2;espaco_Peca(X,0)), write(' '), Z is X+1);
        (X=3, W is round(Y/2)-1, write(W), Z is X+1);
        (ver_Peca(Y,X), Z is X+2);(write(' '), Z is X+1)),
        print_Col(Y,Z)); !.

ver_traco(Pos, Max) :-
        Max=<10, ((Y is Pos-4*Max, Y=1, !); (MaxN is Max+1, ver_traco(Pos,MaxN))).

espaco_Peca(Pos, Max) :-
        Max=<10, (((Y is Pos-4*Max, Y=4, !));
        (MaxN is Max+1, espaco_Peca(Pos,MaxN))).

ver_Peca(Y, X) :- 
        Z is round(Y/2)-1, ASCII is round(63+(X+2)/4),
        char_code(Letra,ASCII), 
        posicao_act(Peca,Letra,Z),
        write(Peca).
        
espacoBoard(Pos, Max) :-
        Max=<10, (((Y is Pos-4*Max,Y=2,!);
        (Y is Pos-4*Max, Y=4, !)); (MaxN is Max+1, espacoBoard(Pos,MaxN))).

ver_letra(Pos, Max) :-
    Max=<10, ((Y is Pos-4*Max, Y=3, !); (MaxN is Max+1, ver_letra(Pos,MaxN))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          INICIAR JOGO           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        LER STANDART INPUT       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization(jogar).
:- initialization(comando).

comando :- argument_list(Arg), comando(Arg).
%comando(Arg) :- process_file.
comando([]) :- nl,readChar.
comando([A|AT]) :- write(A),write('|'),comando(AT).
readChar :- get0(Char),  process(Char,[]).
readChar(A) :- get0(Char),  process(Char,A).
process(-1,A) :- write("Adeus| |\n|\t"),nl,write(A),nl.
process(32,A) :- nl,name(X,A),write(X), nl, readChar([]).
process(32,[]) :- readChar([]).
process(C,[]) :- char_code(L,C), write(L), readChar([C]).
process(C,T) :-  char_code(L,C), write(L),append(T,[C],X), readChar(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            REGRAS               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Peao Branco
regra([80,87],[XA,YA],[XD,YD],Accao):-
        XA=XD, 
        ((Z is YA-1, YD is Z,
           char_code(Letra, XD),
                \+ posicao_act(_,Letra,YD));
        (YD = 5, YA = 7,
         char_code(Letra, XD),
         \+posicao_act(_,Letra,YD),
        W is YD+1,!,
        \+ posicao_act(_,Letra,W))),Accao is 0.