%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROGRAMA - XADREZ-PROLOG%
% Ricardo Fernandes e Alexandra Bernardo%
% Metodologias de Intelig^encia Artificial%
% MIACC 2002/2003%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BASE DE CONHECIMENTO%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
:-dynamic cemiterio/2.
39
posicao_inicial('TB','A',1).

posicao_inicial('HB','B',1).

posicao_inicial('BB','C',1).

posicao_inicial('KB','D',1).

posicao_inicial('QB','E',1).

posicao_inicial('BB','F',1).

posicao_inicial('HB','G',1).

posicao_inicial('TB','H',1).

posicao_inicial('TW','A',8).

posicao_inicial('HW','B',8).

posicao_inicial('BW','C',8).

posicao_inicial('KW','D',8).

posicao_inicial('QW','E',8).

posicao_inicial('BW','F',8).

posicao_inicial('HW','G',8).

posicao_inicial('TW','H',8).

posicao_inicial('PB','A',2).

posicao_inicial('PB','B',2).

posicao_inicial('PB','C',2).

posicao_inicial('PB','D',2).

posicao_inicial('PB','E',2).

posicao_inicial('PB','F',2).

posicao_inicial('PB','G',2).

posicao_inicial('PB','H',2).

posicao_inicial('PW','A',7).

posicao_inicial('PW','B',7).

posicao_inicial('PW','C',7).

posicao_inicial('PW','D',7).

posicao_inicial('PW','E',7).

posicao_inicial('PW','F',7).

posicao_inicial('PW','G',7).

posicao_inicial('PW','H',7).

linhas(19).
colunas(37).
valor(80,100).
valor(66,325).
valor(72,325).
valor(84,500).
valor(81,1000).
valor(75,20000).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inicializa ̧c~ao das pe ̧cas no tabuleiro%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic posicao_actual/4,jogadaN/1,nivel_prof/1.

iniciar:-(posicao_inicial(P,X,Y),not(posicao_actual(P,X,Y,0)),
    assert(posicao_actual(P,X,Y,0)),iniciar);!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Inicializa ̧c~ao do JOGO%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jogar(Tipo_jogo):-
    retractall(posicao_actual(_,_,_,_)),
    retractall(jogador(_,_)),
    retractall(cemiterio(_,_)),
    retractall(jogadaN(_)),
    retractall(nivel_prof(_)), 
    assert(jogadaN(0)),
    assert(nivel_prof(2)),
    iniciar,
    nome(Tipo_jogo),!,
    l(Tipo_jogo,1).

    l(Tipo_jogo,Jogador):-
        (not(movimento(Tipo_jogo,Jogador)),
        ((Jogador=1,JogadorN=2,jogadaN(Num),retractall(jogadaN(_)),NumN is Num+1,
        assert(jogadaN(NumN)));
        (JogadorN=1)),
        l(Tipo_jogo,JogadorN));jogar.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Movimentos dos Jogadores%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
movimento(1,Jogador):-  
    desenhar,
    jogador(Jogador,[Nome]),
    write(Nome),
    write(', qual a sua jogada?'),nl,
    readln(Jogada),nl,
    (((Jogada=['S'];Jogada=['s']),!,jogar);
    (((jogada(Jogada,Jogador),
    ((Jogador=1,((validar_fim_Jogo(66,Nome),jogar);
    movimento(1,2)));
    (Jogador=2,((validar_fim_Jogo(87,Nome),jogar);
    movimento(1,1)))));(nl,write(Nome),
    write(', efectuou um movimento invalido!!'),nl
    ,movimento(1,Jogador))))).

movimento(2,Jogador):-
    jogador(Jogador,[Nome]),
    ((Jogador=1,desenhar,!,write(Nome),
    write(', qual a sua jogada?'),nl,
    readln(Jogada),nl,
    (((Jogada=['S'];Jogada=['s']),
    retractall(cemiterio(_,_)),!,
    jogar);((jogada(Jogada,Jogador),
    ((validar_fim_Jogo(66,Nome),
    retractall(cemiterio(_,_)),
    jogar);(!,fail)));
    (nl,write(Nome),
    write(', efectuou um movimento inv ́alido!!'),nl,
    movimento(2,Jogador)))));’
    (Jogador=2,movimento_maximo_valor(66,1,Xantes,Yantes,Xdepois,Ydepois,Accao),
    mover_PecaNivel(Xantes,Yantes,Xdepois,Ydepois,0,Accao),!,
    (validar_fim_Jogo(87,Nome);fail))).

movimento(3,Jogador):-
    jogador(Jogador,[Nome]),
    ((Jogador=1,desenhar,!,
    write(Nome),
    write(', qual a sua jogada?'),nl,
    readln(Jogada),nl,
    (((Jogada=['S'];Jogada=['s']),
    retractall(cemiterio(_,_)),!,
    jogar);((jogada(Jogada,Jogador),
    ((validar_fim_Jogo(66,Nome),
    retractall(cemiterio(_,_)),
    jogar);(!,fail)));
    (nl,write(Nome),
    write(', efectuou um movimento inv ́alido!!'),nl,
    movimento(3,Jogador)))));
    (Jogador=2,
    alphabeta([], -100000, 100000, [Xantes,Yantes,Xdepois,Ydepois,Accao,_,_],_),
    apagar_tab_aux(1),
    mover_PecaNivel(Xantes,Yantes,Xdepois,Ydepois,0,Accao),!,
    (validar_fim_Jogo(87,Nome);fail))).

validar_fim_Jogo(Cor,Nome):-
    fim_Jogo(Cor,0,Valor),
    (((Valor=(-1),write('Fim de Jogo. Parabens '),
    write(Nome),write('.'));
    (Valor=0,write('Fim de Jogo.Empate.'))),nl,!)
    ;fail.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Jogada do jogador%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jogada([Jogada|R],Jogador):-
    R=[],name(Jogada,JogadaArray),
    converter(JogadaArray,[Xantes,Yantes,Xdepois,Ydepois]),
    ASCIIa is 64+Xantes,
    char_code(LetraA,ASCIIa),
    posicao_actual(Peca,LetraA,Yantes,_),
    name(Peca,[_,Cor]),
    ((Jogador=1,Cor=87);
    (Jogador=2,Cor=66)),
    mover_Peca(Xantes,Yantes,Xdepois,Ydepois).
converter([],[]).

converter([LetraA,NumA,LetraD,NumD],[LetraAn,NumAn,LetraDn,NumDn]):-
    ((LetraA>=65,LetraA=<72,LetraAn is LetraA-64);
    (LetraA>=97,LetraA=<104,LetraAn is LetraA-96)),
    ((LetraD>=65,LetraD=<72,LetraDn is LetraD-64);
    (LetraD>=97,LetraD=<104,LetraDn is LetraD-96)),
    (NumA>=49,NumA=<56,NumAn is NumA-48),
    (NumD>=49,NumD=<56,NumDn is NumD-48).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pedido dos Nomes dos JOGADORES%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nome(1):-write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),nl,nl,
write('Quais os nomes dos jogadores?'),nl,nl,
write('Jogador 1:'),
readln(Nome1),
assert(jogador(1,Nome1)),nl,
write('Jogador 2:'),
readln(Nome2),
assert(jogador(2,Nome2)),nl,nl,
jogador(1,[N1]),
jogador(2,[N2]),
write(N1),
write(', vai jogar com as pe ̧cas brancas.'),nl,nl,
write(N2),
write(', vai jogar com as pe ̧cas pretas.'),nl,nl,
write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'),
nl,nl,
write('Boa Sorte!!'),nl,nl.

nome(X):-
    (X=2;X=3),
    write('Qual o nome do jogador que se atreve a jogar contra n ́os?'),nl,nl,
    write('Jogador:'),
    readln(Nome1),
    assert(jogador(1,Nome1)),nl,
    Nome='Xana&tbs',
    assert(jogador(2,[Nome])).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Menu do JOGO%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jogar:-write('Jogo do Xadrez (Xana&Tbs Lda).'),nl,nl,
write('     [1]Humano vs Humano'),nl,
write('     [2]Humano vs Computador (nivel 1)'),nl,
write('     [3]Humano vs Computador (nivel 2)'),nl,
write('     [4]N~ao quero jogar nada!'),nl,
readln(X),
(validar(X);jogar).

validar([X]):-
    ((X=1;X=2;X=3),!,
    jogar(X));
    (X=4,nl,write('Adeus.Volte sempre.'));
    (write('O jogo '),
    write(X),
    write(' não esta disponivel!! Tente novamente!'),nl,
    nl,jogar).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Desenho do TABULEIRO%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
desenhar:-desenharTabuleiro(1).
desenharTabuleiro(Linha):-
    (linhas(Z),Linha=<Z,
    ((Y is Linha mod 2,Y=1,
    colunas(X),escrever_Linha(X),nl);
    (Y is Linha mod 2,Y=0,escrever_Coluna(Linha,1),nl)),
    LinhaN is Linha+1,desenharTabuleiro(LinhaN));!.
escrever_Linha(X):-
    (X>0,write('_'),escrever_Linha(X-1));!.
escrever_Coluna(2,X):-
    (colunas(C),X=<C,((ver_traco(X,0),write(’|’));
    (ver_espaco(X,0),write(’ ’));
    ((X=3,write(’ ’));
    (ver_letra(X,0),
    ASCII is 63+(X+1)/4,
    char_code(Letra,ASCII),
    write(Letra)))),
    Y is X+1,
    escrever_Coluna(2,Y));!.


    escrever_Coluna(Y,X):-
        (colunas(C),
        X=<C,
        ((ver_traco(X,0),write(’|’),Z is X+1);
        ((X=2;ver_espacoPeca(X,0)),
        write(’ ’),Z is X+1);
        (X=3,W is (Y/2)-1,write(linha(W)),Z is X+1);
        (ver_Peca(Y,X),Z is X+2);
        (write(’ ’),Z is X+1)),
        escrever_Coluna(Y,Z));!.
    
ver_traco(Pos,Max):-
    Max=<10,
    ((Y is Pos-4*Max,Y=1,!);
    (MaxN is Max+1,ver_traco(Pos,MaxN))).
ver_espaco(Pos,Max):-
    Max=<10,
    (((Y is Pos-4*Max,Y=2,!);
    (Y is Pos-4*Max,Y=4,!));
    (MaxN is Max+1,ver_espaco(Pos,MaxN))).
ver_letra(Pos,Max):-
    Max=<10,
    ((Y is Pos-4*Max,Y=3,!);
    (MaxN is Max+1,ver_letra(Pos,MaxN))).
ver_espacoPeca(Pos,Max):-
    Max=<10,
    (((Y is Pos-4*Max,Y=4,!));
    (MaxN is Max+1,ver_espacoPeca(Pos,MaxN))).
ver_Peca(Y,X):-
    Z is (Y/2)-1,
    ASCII is 63+(X+2)/4,
    posicao_actual(Peca,Letra,Z,_),
    char_code(Letra,ASCII_Letra),
    ASCII_Letra=ASCII,write(Peca).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Movimento das PEC ̧AS%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mover_PecaNivel(Xantes,Yantes,Xdepois,Ydepois,MaxNivel,Accao):-
    (not(Ydepois=Yantes);not(Xdepois=Xantes)),
    ASCIId is 64+Xdepois,
    char_code(LetraD,ASCIId),
    ASCIIa is 64+Xantes,
    char_code(LetraA,ASCIIa),
    posicao_actual(Peca,LetraA,Yantes,MaxNivel),
    ((Accao=0,
        retract(posicao_actual(Z,LetraA,Yantes,MaxNivel)),
        assert(posicao_actual(Z,LetraD,Ydepois,MaxNivel)));
        (Accao=1,retract(posicao_actual(PecaMorta,LetraD,Ydepois,MaxNivel)),
        upss,assert(cemiterio(PecaMorta,MaxNivel)),
        retract(posicao_actual(Peca,LetraA,Yantes,MaxNivel)),
        assert(posicao_actual(Peca,LetraD,Ydepois,MaxNivel)))).
mover_Peca(Xantes,Yantes,Xdepois,Ydepois):-
    (not(Ydepois=Yantes);not(Xdepois=Xantes)),
    ASCIId is 64+Xdepois,char_code(LetraD,ASCIId),
    ASCIIa is 64+Xantes,
    char_code(LetraA,ASCIIa),
    posicao_actual(Peca,LetraA,Yantes,0),
    name(Peca,PecaArray),
    regra(PecaArray,[Xantes,Yantes],[Xdepois,Ydepois],Accao,0),
    ((Accao=0,retract(posicao_actual(Z,LetraA,Yantes,0)),
    assert(posicao_actual(Z,LetraD,Ydepois,0)));
    (Accao=1,retract(posicao_actual(PecaMorta,LetraD,Ydepois,0)),
    assert(cemiterio(PecaMorta,0)),retract(posicao_actual(Peca,LetraA,Yantes,0)),
    assert(posicao_actual(Peca,LetraD,Ydepois,0)))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% REGRAS DAS PEC ̧AS%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Peao Branco
regra([80,87],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
    Xantes=Xdepois,((Z is Yantes-1,Ydepois=Z,ASCII is 64+Xdepois,
        char_code(Letra,ASCII),
        not(posicao_actual(_,Letra,Ydepois,MaxNivel)));
        (Ydepois=5,Yantes=7,
            ASCII is 64+Xdepois,
            char_code(Letra,ASCII),
            not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
    W is Ydepois+1,!, 
    not(posicao_actual(_,Letra,W,MaxNivel)))),
    Accao is 0.
regra([80,87],[Xantes,Yantes],
        [Xdepois,Ydepois],
        Accao,MaxNivel):-
            ((Z is Xantes+1,Z=Xdepois);
            (Z is Xantes-1,Z=Xdepois)),
            W is Yantes-1,W=Ydepois,
            ASCII is 64+Xdepois,char_code(Letra,ASCII),
            posicao_actual(Peca,Letra,Ydepois,MaxNivel),
            name(Peca,[_,66]),Accao is 1.
        %Peao Preto
regra([80,66],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
    Xantes=Xdepois,
    ((Z is Yantes+1,Ydepois=Z,
        ASCII is 64+Xdepois,
        char_code(Letra,ASCII),
        not(posicao_actual(_,Letra,Ydepois,MaxNivel)));
        (Ydepois=4,Yantes=2,
            ASCII is 64+Xdepois,
            char_code(Letra,ASCII),
            not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
            W is Ydepois-1,!,not(posicao_actual(_,Letra,W,MaxNivel)))),
            Accao is 0.
regra([80,66],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
    ((Z is Xantes+1,Z=Xdepois);
    (Z is Xantes-1,Z=Xdepois)),
    W is Yantes+1,
    W=Ydepois,ASCII is 64+Xdepois,
    char_code(Letra,ASCII),
    posicao_actual(Peca,Letra,Ydepois,MaxNivel),
    name(Peca,[_,87]),Accao is 1.
%Torre de cor "Cor"
regra([84,Cor],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
    ((var(Xdepois),(!,retornar_num(Z),((Xdepois is Xantes+Z,Ydepois is Yantes);
    (((Z=<Xantes,Xdepois is Xantes-Z);
    (Z>Xantes,Xdepois is Z-Xantes)),
    Ydepois is Yantes);
    (Ydepois is Yantes+Z,Xdepois is Xantes);
    (((Z=<Yantes,Ydepois is Yantes-Z);
    (Z>Yantes,Ydepois is Z-Yantes)),Xdepois is Xantes))),
    Xdepois>0,Xdepois=<8,Ydepois>0,Ydepois=<8);
    (not(var(Xdepois)),((Xantes=Xdepois);(Yantes=Ydepois)))),
    ASCII is 64+Xdepois,char_code(Letra,ASCII),
    ((not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
    ver_Peca_Intermedia([Xantes,Yantes],[Xdepois,Ydepois],MaxNivel),
    Accao is 0);(posicao_actual(Peca,Letra,Ydepois,MaxNivel),
    not(name(Peca,[_,Cor])),ver_Peca_Intermedia([Xantes,Yantes],
        [Xdepois,Ydepois],MaxNivel),Accao is 1)).
%Bispo de cor "Cor"
    regra([66,Cor],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
        ((var(Xdepois),
        (!,retornar_num(Z),
        ((Xdepois is Xantes+Z,(Ydepois is Yantes+Z;Ydepois is Yantes-Z));
        (Xdepois is Xantes-Z,(Ydepois is Yantes+Z;Ydepois is Yantes-Z)))),
        Xdepois>0,Xdepois=<8,Ydepois>0,Ydepois=<8);(not(var(Xdepois)),
        (Dif1 is abs(Xantes-Xdepois),Dif2 is abs(Yantes-Ydepois),Dif1 = Dif2))),
        ASCII is 64+Xdepois,char_code(Letra,ASCII),
        ((not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
        ver_Peca_Intermedia([Xantes,Yantes],[Xdepois,Ydepois],MaxNivel),
        Accao is 0);(posicao_actual(Peca,Letra,Ydepois,MaxNivel),
        not(name(Peca,[_,Cor])),ver_Peca_Intermedia([Xantes,Yantes],
            [Xdepois,Ydepois],MaxNivel),Accao is 1)).
%Rei de cor "cor"
regra([75,Cor],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
((var(Xdepois),((Xdepois is Xantes+1,(Ydepois is Yantes+1;Ydepois is Yantes-1));
(Xdepois is Xantes-1,(Ydepois is Yantes+1;Ydepois is Yantes-1));
(Xdepois is Xantes+1,Ydepois is Yantes);
(Xdepois is Xantes-1,Ydepois is Yantes);
(Ydepois is Yantes+1,Xdepois is Xantes);
(Ydepois is Yantes-1,Xdepois is Xantes)),
Xdepois>0,Xdepois=<8,Ydepois>0,Ydepois=<8);
(not(var(Xdepois)),
((Dif1 is abs(Xantes-Xdepois),Dif2 is abs(Yantes-Ydepois),Dif1 = Dif2,Dif1 = 1);
(Xantes=Xdepois,((Z is Yantes+1,Ydepois=Z);(Z is Yantes-1,Ydepois=Z)));
(Yantes=Ydepois,((Z is Xantes+1,Xdepois=Z);(Z is Xantes-1,Xdepois=Z)))))),
ASCII is 64+Xdepois,char_code(Letra,ASCII),
((not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
Accao is 0);(posicao_actual(Peca,Letra,Ydepois,MaxNivel),
not(name(Peca,[_,Cor])),Accao is 1)).
%Rainha de cor "cor"

regra([81,Cor],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
    ((var(Xdepois),
    (!,retornar_num(Z),
    ((Xdepois is Xantes+Z,(Ydepois is Yantes+Z;Ydepois is Yantes-Z));
    (Xdepois is Xantes-Z,(Ydepois is Yantes+Z;Ydepois is Yantes-Z));
    (Xdepois is Xantes+Z,Ydepois is Yantes);
    (((Z=<Xantes,Xdepois is Xantes-Z);
    (Z>Xantes,Xdepois is Z-Xantes)),Ydepois is Yantes);
    (Ydepois is Yantes+Z,Xdepois is Xantes);
    (((Z=<Yantes,Ydepois is Yantes-Z);
    (Z>Yantes,Ydepois is Z-Yantes)),
    Xdepois is Xantes))),
    Xdepois>0,Xdepois=<8,Ydepois>0,Ydepois=<8);
    (not(var(Xdepois)),
    ((Dif1 is abs(Xantes-Xdepois),
    Dif2 is abs(Yantes-Ydepois),
    Dif1 = Dif2);(Xantes=Xdepois);
    (Yantes=Ydepois)))),
    ASCII is 64+Xdepois,
    char_code(Letra,ASCII),
    ((not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
    ver_Peca_Intermedia([Xantes,Yantes],[Xdepois,Ydepois],MaxNivel),
    Accao is 0);
    (posicao_actual(Peca,Letra,Ydepois,MaxNivel),
    not(name(Peca,[_,Cor])),
    ver_Peca_Intermedia([Xantes,Yantes],[Xdepois,Ydepois],MaxNivel),Accao is 1)).

%Cavalo de cor "cor"

regra([72,Cor],[Xantes,Yantes],[Xdepois,Ydepois],Accao,MaxNivel):-
    ((var(Xdepois),((Z is Xantes+1,Xdepois=Z,(W is Yantes+2;W is Yantes-2),
    Ydepois=W);(Z is Xantes-1,Xdepois=Z,
        (W is Yantes+2;W is Yantes-2),Ydepois=W);
        (Z is Xantes+2,Xdepois=Z,
            (W is Yantes+1;W is Yantes-1),Ydepois=W);
            (Z is Xantes-2,Xdepois=Z,
            (W is Yantes+1;W is Yantes-1),Ydepois=W)),
            Xdepois>0,Xdepois=<8,Ydepois>0,Ydepois=<8);
            (not(var(Xdepois)),Dif1 is abs(Xantes-Xdepois),
            Dif2 is abs(Yantes-Ydepois),
        ((Dif1 = 1,Dif2 = 2);(Dif1 = 2,Dif2 = 1)))),
    ASCII is 64+Xdepois,char_code(Letra,ASCII),
((not(posicao_actual(_,Letra,Ydepois,MaxNivel)),
Accao is 0);
(posicao_actual(Peca,Letra,Ydepois,MaxNivel),
not(name(Peca,[_,Cor])),Accao is 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Verifica ̧c~ao da exist^encia ou n~ao exist^encia de pe ̧cas interm ́edias%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Movimento vertical
ver_Peca_Intermedia([Xantes,Yantes],[Xantes,Ydepois],MaxNivel):-
    not(Yantes=Ydepois),ASCII is 64+Xantes,char_code(Letra,ASCII),
    ((Ydepois>Yantes,Z is Yantes+1);(Ydepois<Yantes,Z is Yantes-1)),
    (Z=Ydepois;not(posicao_actual(_,Letra,Z,MaxNivel))),
    ver_Peca_Intermedia([Xantes,Z],[Xantes,Ydepois],MaxNivel).
%Movimento horizontal
ver_Peca_Intermedia([Xantes,Yantes],[Xdepois,Yantes],MaxNivel):-
    not(Xantes=Xdepois),((Xdepois>Xantes,Z is Xantes+1);
    (Xdepois<Xantes,Z is Xantes-1)),
ASCII is 64+Z,
char_code(Letra,ASCII),
(Z=Xdepois;not(posicao_actual(_,Letra,Yantes,MaxNivel))),
ver_Peca_Intermedia([Z,Yantes],[Xdepois,Yantes],MaxNivel).
%Movimento Diagonal
ver_Peca_Intermedia([Xantes,Yantes],[Xdepois,Ydepois],MaxNivel):-
    Dif1 is abs(Xantes-Xdepois),
    Dif2 is abs(Yantes-Ydepois),
    Dif1 = Dif2,
    ((Xdepois>Xantes,Z is Xantes+1);
    (Xdepois<Xantes,Z is Xantes-1)),
    ((Ydepois>Yantes,W is Yantes+1);
    (Ydepois<Yantes,W is Yantes-1)),
    ASCII is 64+Z,char_code(Letra,ASCII),
    ((W=Ydepois,Z=Xdepois);
    not(posicao_actual(_,Letra,W,MaxNivel))),
    ver_Peca_Intermedia([Z,W],[Xdepois,Ydepois],MaxNivel).
%Movimento final
ver_Peca_Intermedia([Xdepois,Ydepois],[Xdepois,Ydepois],_).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Fun ̧c~oes Geradoras de movimentos%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

funcao_geradora(Peca,X,Y,L,R,Nivel):-
    regra(Peca,[X,Y],[Xdepois,Ydepois],Accao,Nivel),
    Elemento=[Xdepois,Ydepois,Accao,Nivel],
    not(member(Elemento,L)),
    funcao_geradora(Peca,X,Y,[Elemento|L],R,Nivel).
funcao_geradora(_,_,_,R,R,_):-!.
funcao_geradoraM(Peca,X,Y,L,R,Nivel):-
    regra(Peca,[X,Y],[Xdepois,Ydepois],Accao,Nivel),
    Elemento=[X,Y,Xdepois,Ydepois,Accao],
    not(member(Elemento,L)),
    funcao_geradoraM(Peca,X,Y,[Elemento|L],R,Nivel).
funcao_geradoraM(_,_,_,R,R,_):-!.
funcao_geradoraAB(Peca,X,Y,L,R,Nivel,Cont):-
    regra(Peca,[X,Y],[Xdepois,Ydepois],Accao,Nivel),
    NivelN is Nivel+1,
    ElementoT=[X,Y,Xdepois,Ydepois,Accao,NivelN,_],
    not(member(ElementoT,L)),
    ContN is Cont+1,
    Elemento=[X,Y,Xdepois,Ydepois,Accao,NivelN,ContN],
    funcao_geradoraAB(Peca,X,Y,[Elemento|L],R,Nivel,ContN).
    funcao_geradoraAB(_,_,_,R,R,_,_):-!.

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %
    % Fun ̧c~oes Auxiliares%
    %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    retornar_num(X):-(
        X=1;
        X=2;
        X=3;
        X=4;
        X=5;
        X=6;
        X=7).   
max_Tab_Nivel(MaxNivel):-
    constroi_Nivel(Lista,[0]),
    max(Lista,0,MaxNivel).
constroi_Nivel(Lista,L):-
    posicao_actual(_,_,_,Nivel),
    not(member(Nivel,L)),
    constroi_Nivel(Lista,[Nivel|L]).
constroi_Nivel(L,L).
max([],Max,Max).
max([X|R],Inic,Max):-
    (Inic<X,InicN is X,max(R,InicN,Max));
    max(R,Inic,Max).
soma([],Inic,Inic).
soma([X|R],Valor,Inic):-
    InicN is Inic+X,
    soma(R,Valor,InicN).
maxValor([],_,_,Z,W,Y,T,Z,W,Y,T,A,A):-!.
maxValor([[Xantes,Yantes,Xdepois,Ydepois,Accao,V,ValorAdv]|R]
    ,Inic,Max,Z,W,Y,T,Xa,Ya,Xd,Yd,AccaoInic,AccaoN):-
    (Inic<(V-ValorAdv),InicN is (V-ValorAdv),
    maxValor(R,InicN,Max,Xantes,Yantes,Xdepois,Ydepois,Xa,Ya,Xd,Yd,Accao,AccaoN));
    maxValor(R,Inic,Max,Z,W,Y,T,Xa,Ya,Xd,Yd,AccaoInic,AccaoN).

nivela([],L,L).nivela([[]|R],L,RF):-nivela(R,L,RF),!.
nivela([[X|R]],_,[X|R]):-!.
nivela([[X|R]|RL],LI,L):-
    nivela([[X|R]],[],LX),
    nivela(RL,[],LR),
    concatena(LX,LR,L2),
    concatena(L2,LI,L),!.
concatena([],L,L).
concatena([X|R],L,[X|C]):-
    concatena(R,L,C).
escreve([]).
escreve([X|L]):-writeln(X),escreve(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Fun ̧c~ao Avaliadora do tabuleiro%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fim_Jogo(Cor,MaxNivel,Valor):-
    (name(Peca,[75,Cor]),
    cemiterio(Peca,MaxNivel),Valor is -1);
    (((Cor=66,CorN is 87);(Cor=87,CorN is 66)),
    name(Peca,[75,CorN]),
    cemiterio(Peca,MaxNivel),Valor is 2000);
    (posicao_actual(’KW’,_,_,MaxNivel),
    posicao_actual(’KB’,_,_,MaxNivel),
    (posicao_actual(Peca,_,_,MaxNivel),
    (Peca\=’KB’,Peca\=’KW’,!,fail));Valor is 0).
fim_JogoAB(Cor,MaxNivel,Valor):-
    (name(Peca,[75,Cor]),
    cemiterio(Peca,MaxNivel),
    Valor is -1);
    ((Cor=66,CorN is 87,name(Peca,[75,CorN]),
    cemiterio(Peca,MaxNivel),
    Valor is 400);
    (Cor=87,CorN is 66,name(Peca,[75,CorN]),
    cemiterio(Peca,MaxNivel),Valor is 2));
    %(Cor=87,CorN is 66,name(Peca,[75,CorN]),cemiterio(Peca,MaxNivel),Valor is 2));
    (posicao_actual(’KW’,_,_,MaxNivel),posicao_actual(’KB’,_,_,MaxNivel),
    (posicao_actual(Peca,_,_,MaxNivel),(Peca\=’KB’,Peca\=’KW’,!,fail));
    Valor is 0).
valor_tabuleiro(Cor,Valor,MaxNivel):-
    ((fim_JogoAB(Cor,MaxNivel,Valor));
    (listagem_Pecas(Cor,[],ListaValores,[],MaxNivel),
    soma(ListaValores,Valor,0))).

listagem_Pecas(Cor,L,R,LCoord,MaxNivel):-
    posicao_actual(Peca,XLetra,Y,MaxNivel),
    name(Peca,[_,Cor]),name(XLetra,[Num]),
    X is Num-64,Elemento=[X,Y],
    not(member(Elemento,LCoord)),
    pontos(Peca,Valor,X,Y,MaxNivel),
    listagem_Pecas(Cor,[Valor|L],R,[Elemento|LCoord],MaxNivel).
listagem_Pecas(_,Lista,Lista,_,_):-!.
pontos(Peca,Valor,X,Y,MaxNivel):-
    ASCII is 64+X,
    char_code(Letra,ASCII),
    posicao_actual(Peca,Letra,Y,MaxNivel),
    bonus_Defendido_Por(Peca,ValorN,X,Y,MaxNivel),
    bonus_Defendido_Por_Rainha(Peca,ValorN1,X,Y,MaxNivel),
    bonus_Por_Dist_Pos_Inicial(Peca,ValorN2,_,Y),
    bonus_Por_Dist_Centro(Peca,ValorN3,X,Y),
    bonus_Por_Dist_Rei(Peca,ValorN4,X,Y,MaxNivel),
    name(Peca,[P,_]),valor(P,ValorN5),!,
    Valor is ValorN+ValorN1+ValorN2+ValorN3+ValorN4+ValorN5.
bonus_Defendido_Por(Peca,Valor,X,Y,MaxNivel):-
    defendido_MPeca(Peca,X,Y,MaxNivel),!,
    tabela_Pontos_Defendido_MPeca(Peca,Valor).
bonus_Defendido_Por(_,0,_,_,_):-!.
bonus_Defendido_Por_Rainha(Peca,Valor,X,Y,MaxNivel):-
    peca_defendida_Rainha(Peca,X,Y,MaxNivel),
    Valor is 25,!.
bonus_Defendido_Por_Rainha(_,0,_,_,_):-!.
bonus_Por_Dist_Pos_Inicial(Peca,Valor,_,Y):-
    name(Peca,PecaArray),
    PecaArray=[80,Cor],!,
    ((Cor=87,Valor is 10*(7-Y));
    (Cor=66,Valor is 10*(Y-2))).
bonus_Por_Dist_Pos_Inicial(_,0,_,_):-!.
bonus_Por_Dist_Centro(Peca,Valor,X,Y):-
    name(Peca,PecaArray),
    ((PecaArray=[72,_],((Y<5,DistY is Y-1);
    (Y>=5,DistY is 8-Y)),((X<5,DistX is X-1);
    (X>=5,DistX is 8-X)));
    (PecaArray=[80,Cor],((Cor=87,Y>=5,DistY is 8-Y);
    (Cor=66,Y<5,DistY is Y-1)),
    ((X<5,DistX is X-1);
    (X>=5,DistX is 8-X)))),
    Valor is (DistY*5+DistX*5),!.
bonus_Por_Dist_Centro(_,0,_,_):-!.
bonus_Por_Dist_Rei(Peca,Valor,X,Y,MaxNivel):-
    name(Peca,PecaArray),
    PecaArray=[P,C],
    (P=72;P=84;P=81),
    name(Rei,[75,C]),
    posicao_actual(Rei,XReiL,YRei,MaxNivel),
    name(XReiL,[ASCII]),
    XRei is ASCII-64,
    ((Y>=YRei,DistY is Y-YRei);
    (Y<YRei,DistY is YRei-Y)),
    ((X>=XRei,DistX is X-XRei);
    (X<XRei,DistX is XRei-X)),
    (((P=72;P=84),
    Valor is 70-(DistY*2+DistX*2));
    (Valor is 140-(DistY*3+DistX*3))),!.
bonus_Por_Dist_Rei(_,0,_,_,_):-!.
tabela_Pontos_Defendido_MPeca(Peca,Valor):-
    (name(Peca,PecaArray),
    ((PecaArray=[80,_],Valor is 10);
    (PecaArray=[66,_],Valor is 25);
    (PecaArray=[84,_],Valor is 25)));
    Valor is 0.
defendido_MPeca(Peca,X,Y,MaxNivel):-
    name(Peca,PecaArray),
    ((PecaArray=[Z,66],PecaNC is 87);
    (PecaArray=[Z,87],PecaNC is 66)),
    PecaN = [Z,PecaNC],
    funcao_geradora(PecaN,X,Y,[],R,MaxNivel),!,
    not(ver_PecaDiferente(Peca,R)).
ver_PecaDiferente(Peca,[[X,Y,Accao,Nivel]|R]):-
    (Accao=1,ASCII is 64+X,char_code(Letra,ASCII),
    posicao_actual(PecaC,Letra,Y,Nivel),
    Peca=PecaC,!,fail);
    ver_PecaDiferente(Peca,R).
ver_PecaDiferente(_,[]):-!.
peca_defendida_Rainha(Peca,X,Y,MaxNivel):-
    name(Peca,PecaArray),
    (PecaArray=[66,_];PecaArray=[84,_]),
    ((PecaArray=[Z,66],PecaNC is 87);
    (PecaArray=[Z,87],PecaNC is 66)),
    PecaN = [Z,PecaNC],
    funcao_geradora(PecaN,X,Y,[],R,MaxNivel),!,
    not(ver_PecaRainha(Peca,R)).
ver_PecaRainha(Peca,[[X,Y,Accao,Nivel]|R]):-
    (Accao=1,ASCII is 64+X,char_code(Letra,ASCII),
    posicao_actual(PecaC,Letra,Y,Nivel),
    name(Peca,[_,PC]),name(PecaC,[81,PC]),
    !,fail);ver_PecaRainha(Peca,R).
ver_PecaRainha(_,[]).

