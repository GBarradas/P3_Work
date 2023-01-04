coluna('A').
coluna('B').
coluna('C').
coluna('D').
coluna('E').
coluna('F').
coluna('G').
coluna('H').
linha(X) :- between(1,8,X).
ps(X,Y,'| |') :- coluna(X), linha(Y).
ps(X,Y) :- ps(X,Y,A), white(A).
tabuleiro([
    [ps('A',1),ps('B',1),ps('C',1), ps('D',1),ps('E',1),ps('F',1),ps('G',1),ps('H',1)],
    [ps('A',2),ps('B',2),ps('C',2), ps('D',2),ps('E',2),ps('F',2),ps('G',2),ps('H',2)],
    [ps('A',3),ps('B',3),ps('C',3), ps('D',3),ps('E',3),ps('F',3),ps('G',3),ps('H',3)],
    [ps('A',4),ps('B',4),ps('C',4), ps('D',4),ps('E',4),ps('F',4),ps('G',4),ps('H',4)],
    [ps('A',5),ps('B',5),ps('C',5), ps('D',5),ps('E',5),ps('F',5),ps('G',5),ps('H',5)],
    [ps('A',6),ps('B',6),ps('C',6), ps('D',6),ps('E',6),ps('F',6),ps('G',6),ps('H',6)],
    [ps('A',7),ps('B',7),ps('C',7), ps('D',7),ps('E',7),ps('F',7),ps('G',7),ps('H',7)],
    [ps('A',8),ps('B',8),ps('C',8), ps('D',8),ps('E',8),ps('F',8),ps('G',8),ps('H',8)]
    ]).

