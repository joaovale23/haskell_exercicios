module Cap3E where
-- Exercícios do capítulo 3 do livro

-- 3.10)	Faça	uma	função	chamada		revNum	,	que	receba	uma
-- String		s		e	um		Int	n	.	Esta	deverá	retornar	as	n	primeiras	letras
-- em	ordem	reversa	e	o	restante	em	sua	ordem	normal.

revNum :: String -> Int -> String
revNum s n = reverse (take n s) ++ drop n s

-- 3.11)	Crie	o	tipo	de	dado		Binario		que	pode	ser		Zero		ou
-- Um	.	Faça	outro	tipo	de	dado	chamado		Funcao		que	pode	ser
-- Soma2	,		Maior	,		Menor		ou		Mult2	.	Implemente	a	função
-- aplicar		que	recebe	uma		Funcao		e	dois		Binarios	.	Seu	retorno
-- consiste	em	executar	a	operação	desejada.

data Binario = Zero | Um deriving (Show, Eq)
data Funcao = Soma2 | Maior | Menor | Mult2 deriving (Show, Eq)

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Zero Um = Um
aplicar Soma2 Um Zero = Um
aplicar Soma2 Um Um = Zero
aplicar Maior Zero Zero = Zero
aplicar Maior Zero Um = Um
aplicar Maior Um Zero = Um
aplicar Maior Um Um = Um
aplicar Menor Zero Zero = Zero
aplicar Menor Zero Um = Zero
aplicar Menor Um Zero = Zero
aplicar Menor Um Um = Um
aplicar Mult2 Zero Zero = Zero
aplicar Mult2 Zero Um = Zero
aplicar Mult2 Um Zero = Zero
aplicar Mult2 Um Um = Um

-- 3.12)	Faça	uma	função	chamada		binList	,	usando	list
-- compreeshion,	que	recebe	uma	lista	de		Binarios		(ver	exercício
-- anterior)	e	retorna	outra	lista	com	elemento	somado		Um		e
-- convertido	para		Int	

binList :: [Binario] -> [Int]
binList xs = [if x == Um then 1 else 0 | x <- xs]

-- 3.7)	Faça	uma	função	que	receba	uma		String		e	retorne
-- True		se	esta	for	um	palíndromo;	caso	contrário,		False	

isPalindromo :: String -> Bool
isPalindromo s = s == reverse s

-- 3.14)	Faça	o	novo	tipo		Valido		que	possui	dois	value
-- constructors		Sim		e		Nao	.	O	value	constructor		Sim		possui	um
-- parâmetro	(campo)		
-- String	.	Implemente	uma	função
-- isNomeValido		que	recebe	um	nome	e	retorna		Nao		caso	a
-- String		seja	vazia;	caso	contrário,		Sim	

data Valido = Sim String | Nao deriving (Show, Eq)

isNomeValido :: String -> Valido
isNomeValido "" = Nao
isNomeValido nome = Sim nome