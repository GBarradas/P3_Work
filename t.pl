linha(1).
linha(2).
linha(3).
linha(4).
linha(5).
linha(6).
linha(7).
linha(8).
coluna('A').
coluna('B').
coluna('C').
coluna('D').
coluna('E').
coluna('F').
coluna('G').
coluna('H').
celula(X, Y) :- coluna(X), linha(Y).

tabuleiro :- [
    [celula('A', 1)| 'TP'] | [celula('B', 1)| 'CP'], [celula('C', 1)| 'BP'], [celula('D', 1)| 'KP'],
    [celula('E', 1)| 'QP'], [celula('F', 1)| 'BP'], [celula('G', 1)| 'CP'], [celula('H', 1)| 'TP'],
    [celula('A', 2)| 'PP'], [celula('B', 2)| 'PP'], [celula('C', 2)| 'PP'], [celula('D', 2)| 'PP'],
    [celula('E', 2)| 'PP'], [celula('F', 2)| 'PP'], [celula('G', 2)| 'PP'], [celula('H', 2)| 'PP'],
    [celula('A', 3)| ''], [celula('B', 3)| ''], [celula('C', 3)| ''], [celula('D', 3)| ''],
    [celula('E', 3)| ''], [celula('F', 3)| ''], [celula('G', 3)| ''], [celula('H', 3)| ''],
    [celula('A', 4)| ''], [celula('B', 4)| ''], [celula('C', 4)| ''], [celula('D', 4)| ''],
    [celula('E', 4)| ''], [celula('F', 4)| ''], [celula('G', 4)| ''], [celula('H', 4)| ''],
    [celula('A', 5)| ''], [celula('B', 5)| ''], [celula('C', 5)| ''], [celula('D', 5)| ''],
    [celula('E', 5)| ''], [celula('F', 5)| ''], [celula('G', 5)| ''], [celula('H', 5)| ''],
    [celula('A', 6)| ''], [celula('B', 6)| ''], [celula('C', 6)| ''], [celula('D', 6)| ''],
    [celula('E', 6)| ''], [celula('F', 6)| ''], [celula('G', 6)| ''], [celula('H', 6)| ''],
    [celula('A', 7)| 'PB'], [celula('B', 7)| 'PB'], [celula('C', 7)| 'PB'], [celula('D', 7)| 'PB'],
    [celula('E', 7)| 'PB'], [celula('F', 7)| 'PB'], [celula('G', 7)| 'PB'], [celula('H', 7)| 'PB'],
    [celula('A', 8)| 'TB'], [celula('B', 8)| 'CB'], [celula('C', 8)| 'BB'], [celula('D', 8)| 'KB'],
    [celula('E', 8)| 'QB'], [celula('F', 8)| 'BB'], [celula('G', 8)| 'CB'], [celula('H', 8)| 'TB']].

pos_ini('PP', celula('A', 2)).
pos_ini('PP', celula('B', 2)).
pos_ini('PP', celula('C', 2)).
pos_ini('PP', celula('D', 2)).
pos_ini('PP', celula('E', 2)).
pos_ini('PP', celula('F', 2)).
pos_ini('PP', celula('G', 2)).
pos_ini('PP', celula('H', 2)).
pos_ini('TP', celula('A', 1)).
pos_ini('CP', celula('B', 1)).
pos_ini('BP', celula('C', 1)).
pos_ini('KP', celula('D', 1)).
pos_ini('QP', celula('E', 1)).
pos_ini('BP', celula('F', 1)).
pos_ini('CP', celula('G', 1)).
pos_ini('TP', celula('H', 1)).
pos_ini('PB', celula('A', 7)).
pos_ini('PB', celula('B', 7)).
pos_ini('PB', celula('C', 7)).
pos_ini('PB', celula('D', 7)).
pos_ini('PB', celula('E', 7)).
pos_ini('PB', celula('F', 7)).
pos_ini('PB', celula('G', 7)).
pos_ini('PB', celula('H', 7)).
pos_ini('TB', celula('A', 8)).
pos_ini('CB', celula('B', 8)).
pos_ini('BB', celula('C', 8)).
pos_ini('KB', celula('D', 8)).
pos_ini('QB', celula('E', 8)).
pos_ini('BB', celula('F', 8)).
pos_ini('CB', celula('G', 8)).
pos_ini('TB', celula('H', 8)).

:-  dynamic(pos_act/3).

iniciar :- pos_ini(P, celula(X, Y)), assert(pos_act(P, celula(X, Y))), iniciar ; !.
