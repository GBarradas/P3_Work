% tipologia descritiva
/*
coluna('A').
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
imprimir('RB',' ♜').
imprimir('NB',' ♞').
imprimir('BB',' ♝').
imprimir('QB',' ♛').
imprimir('KB',' ♚').
imprimir('PB',' ♟').
imprimir('PW',' ♙').
imprimir('RW',' ♖').
imprimir('NW',' ♘').
imprimir('BW',' ♗').
imprimir('QW',' ♕').
imprimir('KW',' ♔').



posicao_inicial('PB', 'A', 7).
posicao_inicial('PB', 'B', 7).
posicao_inicial('PB', 'C', 7).
posicao_inicial('PB', 'D', 7).
posicao_inicial('PB', 'E', 7).
posicao_inicial('PB', 'F', 7).
posicao_inicial('PB', 'G', 7).
posicao_inicial('PB', 'H', 7).
posicao_inicial('RB', 'A', 8).
posicao_inicial('NB', 'B', 8).
posicao_inicial('BB', 'C', 8).
posicao_inicial('QB', 'D', 8).
posicao_inicial('KB', 'E', 8).
posicao_inicial('BB', 'F', 8).
posicao_inicial('NB', 'G', 8).
posicao_inicial('RB', 'H', 8).
posicao_inicial('PW', 'A', 2).
posicao_inicial('PW', 'B', 2).
posicao_inicial('PW', 'C', 2).
posicao_inicial('PW', 'D', 2).
posicao_inicial('PW', 'E', 2).
posicao_inicial('PW', 'F', 2).
posicao_inicial('PW', 'G', 2).
posicao_inicial('PW', 'H', 2).
posicao_inicial('RW', 'A', 1).
posicao_inicial('NW', 'B', 1).
posicao_inicial('BW', 'C', 1).
posicao_inicial('QW', 'D', 1).
posicao_inicial('KW', 'E', 1).
posicao_inicial('BW', 'F', 1).
posicao_inicial('NW', 'G', 1).
posicao_inicial('RW', 'H', 1).
linhas(19).
colunas(37).
valor(80, 100).
valor(66, 325).
valor(72, 325).
valor(84, 500).
valor(81, 1000).
valor(75, 20000).

peca('Q').
peca('K').
peca('B').
peca('N').
peca('R').
peca('P').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        INICIAR TABULEIRO        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(pos_act/3).
:- dynamic(cor/1).
:- dynamic(cheque/1).

iniciar :- (posicao_inicial(P, X, Y),\+pos_act(P,X,Y),
        asserta(pos_act(P, X, Y)),iniciar);!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%       DESENHAR TABULEIRO        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jogar :- iniciar,asserta(cor(87)).

desenhar :- desenhar_Tab(1),nl.

desenhar_Tab(Linha):-(linhas(Z), Linha=<Z,
        ((Y is Linha mod 2, Y=1, colunas(X), print_Linha(X), nl);
        (Y is Linha mod 2, Y=0, print_Col(Linha,2), nl)),
        LinhaN is Linha+1, desenhar_Tab(LinhaN)); !.

print_Linha(X) :- (X>1,write('—'), print_Linha(X-1)); !.

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
        pos_act(Peca,Letra,Z),
        imprimir(Peca,Simbolo),
        write(Simbolo).
        
espacoBoard(Pos, Max) :-
        Max=<10, (((Y is Pos-4*Max,Y=2,!);
        (Y is Pos-4*Max, Y=4, !)); (MaxN is Max+1, espacoBoard(Pos,MaxN))).

ver_letra(Pos, Max) :-
    Max=<10, ((Y is Pos-4*Max, Y=3, !); (MaxN is Max+1, ver_letra(Pos,MaxN))).
digito(X) :- (X=1;X=2;X=3;X=4;X=5;X=6;X=7).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          INICIAR JOGO           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(jogar).
:- initialization(comando).


comando :- argument_list(Arg), comando(Arg).
%comando(Arg) :- process_file.
%comando([]) :- nl,readChar.
comando(['algebrica','algebrica']) :- desenhar, readChar.
comando(['algebrica','mostrar']) :- desenhar, readCharHide, desenhar.
comando(['algebrica','estado']) :- readCharHide, desenhar, show_estado.
comando(_):- write('Argumentos Invalidos!!'),nl.
show_estado:- ((cheque(X), write('O jogador '),((X=66,write('Preto'));(X=87, write('Branco'))),
write(' esta em Cheque!!'));(write('Nenhum Jogador em Cheque'))),nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%        LER STANDART INPUT       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

readChar :- get0(Char),  process(Char,[]).
readChar(A) :- get0(Char),  process(Char,A).
process(-1,_) :- /*write("Adeus| |\n|\t"),*/nl.
process(32,A) :-name(STR,A),write(STR),nl, (jogada(A);\+jogada(A)),desenhar, readChar([]).
process(32,[]) :- readChar([]).
process(46,A) :- name(STR,A),write(STR),nl, (jogada(A);\+jogada(A)), desenhar.
process(46,_) :- desenhar.
process(9,A) :- name(STR,A),write(STR),nl, (jogada(A);\+jogada(A)),desenhar, readChar([]).
process(9,[]) :- readChar([]).
process(10,A) :- name(STR,A),write(STR),nl, (jogada(A);\+jogada(A)),desenhar, readChar([]).
process(10,[]) :- readChar([]).
process(C,[]) :- readChar([C]).
process(C,T) :- append(T,[C],X), readChar(X).

readCharHide :- get0(Char),  processHide(Char,[]).
readCharHide(A) :- get0(Char),  processHide(Char,A).
processHide(-1,_) :- nl.
processHide(32,A) :-(jogada(A);\+jogada(A)), readCharHide([]).
processHide(32,[]) :- readCharHide([]).
processHide(46,A) :- (jogada(A);\+jogada(A)).
processHide(46,_) :- nl.
processHide(9,A) :- (jogada(A);\+jogada(A)), readCharHide([]).
processHide(9,[]) :- readCharHide([]).
processHide(10,A) :- (jogada(A);\+jogada(A)), readCharHide([]).
processHide(10,[]) :- readCharHide([]).
processHide(C,[]) :- readCharHide([C]).
processHide(C,T) :- append(T,[C],X), readCharHide(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%             JOGADAS             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get([H|_],0,H).
%get([_,T], Position, Elemento) :- NP is Position-1, get(T,NP,Elemento).
change_color(87):-retract(cor(87)),asserta(cor(66)).
change_color(66):-retract(cor(66)),asserta(cor(87)).

jogada(A):- length(A, X), name(STR,A),cor(Cor),
%write('>'),((Cor=66,write('Black->'));(Cor=87,write('White->'))),write(STR),nl,
jogada(A,X).
jogada([H,T],2) :- joga(H,T).
jogada([P1,P2,P3],3) :-  joga(P1,P2,P3).
jogada([P1,P2,P3,P4],4) :- joga(P1,P2,P3,P4).
jogada([P1,P2,P3,P4,P5],5) :-  joga(P1,P2,P3,P4,P5).

joga(A,B):- AC is A-96,BC is B-48,cor(COR),name(P,[80,COR]),
        (pos_act(P,X1,Y1), char_code(X1,X2), X3 is X2-64,      
        regra([80,COR],[X3,Y1],[AC,BC],0),!),
        AA is AC+64, char_code(CD,AA),
        retract(pos_act(P,X1,Y)), asserta(pos_act(P,CD,BC)),
        change_color(COR), retractall(cheque(_)).

joga(X,Y,43) :- joga(X,Y),cor(COR),((COR=66, asserta(cheque(87)));(Cor=87, asserta(cheque(66)))).
joga(X,Y,35) :- joga(X,Y),cor(COR),((COR=66, asserta(cheque(87)));(Cor=87, asserta(cheque(66)))).
joga(Peca,X,Y):- AC is X -96, BC is Y-48,cor(COR),name(P,[Peca,COR]),
        (pos_act(P,X1,Y1), char_code(X1,X2),X3 is X2-64,
        regra([Peca,COR],[X3,Y1],[AC,BC],0),!),
        AA is AC+64, char_code(CD,AA),
        retract(pos_act(P,X1,Y1)),asserta(pos_act(P,CD,BC)),
        change_color(COR), retractall(cheque(_)).

joga(79,45,79) :- cor(Cor), roque(Cor),
        name(KP,[75,Cor]), name(RP,[82,Cor]),
        pos_act(KP,'E',YK), pos_act(RP,'H',YR),
        retract(pos_act(KP,'E',YK)), asserta(pos_act(KP,'G',YK)),
        retract(pos_act(RP,'H',YR)), asserta(pos_act(RP,'F',YR)),
        change_color(Cor), retractall(cheque(_)).
joga(P,120,X,Y):- ((char_code(PL,P), peca(PL),
        AC is X -96, BC is Y-48,cor(COR),name(Peca,[P,COR]),
        (pos_act(Peca,X1,Y1), char_code(X1,X2),X3 is X2-64,
        regra([P,COR],[X3,Y1],[AC,BC],1),!),
        AA is AC+64, char_code(CD,AA),
        retract(pos_act(_,CD,BC)),
        retract(pos_act(Peca,X1,Y1)),asserta(pos_act(Peca,CD,BC)));
        (AC is X -96, BC is Y-48,XP is P-32, AN is X-32,
        char_code(L,XP),cor(COR),name(Peca,[80,COR]),
        (pos_act(Peca,L,YA),char_code(L,NCA),NC is NCA-64,
        %write(NC),write(YA),write(AC),write(BC),
        regra([80,COR],[NC,YA],[AC,BC],1),!),char_code(FD,AN),
        (retract(pos_act(_,FD,BC));retract(pos_act(_,FD,YA))),
        retract(pos_act(Peca,L,YA)),asserta(pos_act(Peca,FD,BC))
        ))
        ,change_color(COR), retractall(cheque(_)).

joga(P,X,Y,43):- joga(P,X,Y),cor(COR),((COR=66, asserta(cheque(87)));(Cor=87, asserta(cheque(66)))).
joga(P,X,Y,35):- joga(P,X,Y),cor(COR),((COR=66, asserta(cheque(87)));(Cor=87, asserta(cheque(66)))).

joga(Peca,XP,X,Y):-  AC is X -96, BC is Y-48,cor(COR),name(P,[Peca,COR]),
        XA is XP-32,char_code(LP,XA),
        (pos_act(P,LP,Y1), char_code(LP,X2),X3 is X2-64,
        regra([Peca,COR],[X3,Y1],[AC,BC],0),!),
        AA is AC+64, char_code(CD,AA),
        retract(pos_act(P,LP,Y1)),asserta(pos_act(P,CD,BC)),
        change_color(COR), retractall(cheque(_)).

joga(Peca,X,Y,A):- write('Ocorreu um erro').
joga(Peca,X,Y,A,43):- joga(Peca,X,Y,A),cor(COR),((COR=66, asserta(cheque(87)));(Cor=87, asserta(cheque(66)))).
joga(Peca,XP,120,X,Y):- AC is X -96, BC is Y-48,cor(COR),name(P,[Peca,COR]),
        XA is XP-32,char_code(LP,XA),
        (pos_act(P,LP,Y1), char_code(LP,X2),X3 is X2-64,
        regra([Peca,COR],[X3,Y1],[AC,BC],1),!),
        AA is AC+64, char_code(CD,AA),
        retract(pos_act(_,CD,BC)),
        retract(pos_act(P,X1,Y1)),
        asserta(pos_act(P,CD,BC)),
        change_color(COR), retractall(cheque(_)).
%joga(Peca,X,Y,A,B):- cor(COR),change_color(COR), write('Por Implementar').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%            REGRAS               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- PEÃO BRANCO SEM COMER

regra([80,87], [Xantes, Yantes], [Xdepois, Ydepois], 0) :-
        Xantes = Xdepois, 
        ((Z is Yantes+1, Ydepois=Z,
        ASCII is 64+Xdepois, char_code(Letra, ASCII),
        \+(pos_act(_, Letra, Ydepois))) ;
        (Ydepois=4, Yantes=2, ASCII is 64+Xdepois,
        char_code(Letra, ASCII), \+(pos_act(_, Letra, Ydepois)),
        W is Ydepois-1, !, \+(pos_act(_, Letra, W)))).

% -- PEÃO BRANCO A COMER

regra([80, 87], [Xantes, Yantes], [Xdepois, Ydepois], 1) :-
        
        ((X is Xantes + 1, X = Xdepois);
        (X is Xantes - 1, X = Xdepois)),
        Y is Yantes + 1, Y = Ydepois,
        ASCII is 64 + Xdepois, char_code(Letra, ASCII),
        (pos_act(Peca, Letra, Ydepois);pos_act(Peca,Letra,Yantes)),
        name(Peca, [_,66]).

% -- PEÃO PRETO SEM COMER

regra([80,66], [Xantes, Yantes], [Xdepois, Ydepois], 0) :-
        Xantes = Xdepois, 
        ((Z is Yantes-1, Ydepois=Z,
        ASCII is 64+Xdepois, char_code(Letra, ASCII),
        \+(pos_act(_, Letra, Ydepois))) ;
        (Ydepois=5, Yantes=7, ASCII is 64+Xdepois,
        char_code(Letra, ASCII), \+(pos_act(_, Letra, Ydepois)),
        W is Ydepois+1, !, \+(pos_act(_, Letra, W)))).

% -- PEÃO PRETO A COMER

regra([80,66], [Xantes, Yantes], [Xdepois, Ydepois], 1) :-
        
        ((X is Xantes+1, X=Xdepois) ; (X is Xantes-1, X=Xdepois)),
        Y is Yantes-1, Y=Ydepois,
        ASCII is 64+Xdepois, char_code(Letra, ASCII), 
        (pos_act(Peca, Letra, Ydepois);pos_act(Peca,Letra,Yantes)),
        name(Peca, [_, 87]).

% -- TORRE

regra([82,Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :- 
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
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
        (pos_act(Peca, Letra, Ydepois),
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
        (pos_act(Peca, Letra, Ydepois),
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
        (pos_act(Peca, Letra, Ydepois), Acao is 1)).

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
        existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]),
        Acao is 1)).

% -- CAVALO

regra([78, Cor], [Xantes, Yantes], [Xdepois, Ydepois], Acao) :-
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
        (pos_act(Peca, Letra, Ydepois)),Acao is 1)).

% -- ROQUE PRETO

roque(66):- pos_act('KB','E',8),\+pos_act(_,'F',8),
        \+pos_act(_,'G',8),pos_act('RB','H',8).
% -- ROQUE BRANCO
roque(87):- pos_act('KW','E',1),\+pos_act(_,'F',1),
\+pos_act(_,'G',1),pos_act('RW','H',1).

%//////////////////////////////////////////////////////////////////////////////////////
%       VERIFICAR A EXISTENCIA DE PEÇAS PELO CAMINHO
%//////////////////////////////////////////////////////////////////////////////////////
% -- VERIFICAR NA COLUNA

existe_peca_no_caminho([Xantes, Yantes], [Xantes, Ydepois]) :-
        \+(Yantes=Ydepois), ASCII is 64+Xantes, char_code(Letra, ASCII),
        ((Ydepois>Yantes, Z is Yantes+1) ; (Ydepois<Yantes, Z is Yantes-1)),
        (Z=Ydepois ; \+(pos_act(_, Letra, Z))),
        existe_peca_no_caminho([Xantes, Z], [Xantes, Ydepois]).


% -- VERIFICAR NA LINHA

existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Yantes]) :-
        \+(Xantes=Xdepois), 
        ((Xdepois>Xantes, Z is Xantes+1) ; (Xdepois<Xantes, Z is Xantes-1)),
        ASCII is 64+Z, char_code(Letra, ASCII),
        (Z=Xdepois ; \+(pos_act(_, Letra, Yantes))),
        existe_peca_no_caminho([Z, Yantes], [Xdepois, Yantes]).

% -- VERIFICAR NA DIAGONAL

existe_peca_no_caminho([Xantes, Yantes], [Xdepois, Ydepois]) :-
        Dif1 is abs(Xantes-Xdepois), 
        Dif2 is abs(Yantes-Ydepois), Dif1=Dif2,
        ((Xdepois>Xantes, Z is Xantes+1) ; (Xdepois<Xantes, Z is Xantes-1)),
        ((Ydepois>Yantes, W is Yantes+1) ; (Ydepois<Yantes, W is Yantes-1)),
        ASCII is 64+Z, char_code(Letra, ASCII), 
        ((W=Ydepois, Z=Xdepois) ; \+(pos_act(_, Letra, W))),
        existe_peca_no_caminho([Z, W], [Xdepois, Ydepois]).
% -- VERIFICAR FIM

existe_peca_no_caminho([X, Y], [X, Y]).