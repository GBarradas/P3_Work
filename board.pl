piece(white, rook,   1, 1).
piece(white, knight, 2, 1).
piece(white, bishop, 3, 1).
piece(white, queen,  4, 1).
piece(white, king,   5, 1).
piece(white, bishop, 6, 1).
piece(white, knight, 7, 1).
piece(white, rook,   8, 1).
piece(white, pawn,   X, 2) :-
  between(1, 8, X).

piece(black, rook,   1, 8).
piece(black, knight, 2, 8).
piece(black, bishop, 3, 8).
piece(black, queen,  4, 8).
piece(black, king,   5, 8).
piece(black, bishop, 6, 8).
piece(black, knight, 7, 8).
piece(black, rook,   8, 8).
piece(black, pawn,   X, 7) :-
  between('A', 'H', X).
