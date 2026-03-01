module Cap2E where

-- Exercícios do capítulo 2 do livro

-- 2.1) Gere as Listas:
listaa :: [Int]
listaa = take 7[11^n | n <- [0..]]

listab :: [Int]
listab = [x | x <- [1,2 ..40], mod x 4 /= 0]

listac :: [String]
listac = take 7["A" ++ [x] ++ "BB" | x <- ['a','b'..]]

listad :: [Int]
listad = [x | x <- [5, 8 .. 41], mod x 7 /= 0, x /= 23]

listae :: [Float]
listae = take 6[1/2^x | x <- [0..]]

listaf :: [Int]
listaf = [1, 10.. 64]

listag :: [Int]
listag = [x | x <- [2, 4..30], x `notElem` [6,14,20,26]]

listah :: [Char]
listah = [x | x <- ['@','A'..'L'], x `notElem` ['B','F','H','I','K']]

-- 2.2) Crie	uma	função	que	verifique	se	o	tamanho	de	uma
-- String	é	par	ou	não.	Use		Bool		como	retorno.
par :: String -> Bool
par x = even (length x)

-- 2.3) Escreva	uma	função	que	receba	um	vetor	de	Strings	e
-- retorne	uma	lista	com	todos	os	elementos	em	ordem	reversa.
strings :: [String] -> [String]
strings = reverse

-- 2.4)	Escreva	uma	função	que	receba	um	vetor	de	Strings	e
-- retorne	uma	lista	com	o	tamanho	de	cada	String.	As	palavras	de
-- tamanho	par	devem	ser	excluídas	da	resposta.
stringPar :: [String] -> [Int]
stringPar xs = [length s | s <- xs, odd(length s)]

-- 2.5)	Escreva	a	função		head		como	composição	de	duas	outras.
head' :: String -> Char
head' x = last(reverse x)

-- 2.6)	Faça	uma	função	que	receba	uma	String	e	retorne		True	
-- se	esta	for	um	palíndromo;	caso	contrário,		False	
palindromo :: String -> Bool
palindromo x = x == reverse x

-- Faça	uma	função	que	receba	um	inteiro	e	retorne	uma
-- tupla,	contendo:	o	dobro	deste	número	na	primeira	coordenada,	o
-- triplo	na	segunda,	o	quádruplo	na	terceira	e	o	quíntuplo	na	quarta.
tupla :: Int -> (Int, Int, Int, Int)
tupla x = (2*x, 3*x, 4*x, 5*x)