module CombinatorParserLibrary where
infixr 3 <||>
infixl 5 <**>
infixl 5 <$$>, <$$, <**, **>, <***>, <???>
infixl 2 `opt`

-----------------------
-- Combinator Parser --
-----------------------

-- Parser: Consume una lista de símbolos de tipo s y devuelve una lista de pares 
-- estructura del parser / lista sin consumir. Lista de pares porque si la gramática
-- es ambigua, devuelve todas las formas de parseo.
-- Valor testigo: t (estructura de salida)
type Parser s t =([s] -> [(t, [s])])

-- Función que obtiene el valor final del parseo, dado el parser y la cadena de entrada.
-- Usa clase Either como respuesta, contructor Right para respuesta correcta
parse :: (t -> [(a, [Char])]) -> t -> Either [Char] a
parse p s = case p s of 
                ((res,rest):rs) -> if rest == "" then Right res else Left "Erroneous input"
                [] -> Left "Erroneous input"

-- Función que extrae el valor testigo
extract :: Either a b -> b
extract parseFunction = case parseFunction of
                            Right elem -> elem

-----------------------
-- Basic Combinators --

-- Devuelve un parser para el tipo dado, siempre exitoso
pReturn :: Eq s => a -> Parser s a
pReturn a = \inp -> [(a, inp)]

-- Parser de falla
pFail :: Eq s => Parser s a
pFail = const []

-- Dado un símbolo, de cualquier tipo, construye un parser que reconoce si el 
-- string a parsear comienza con ese símbolo
pSym :: Eq s => s -> Parser s s
pSym c = \inp -> case inp of 
                        (s:ss) -> if s == c then [(s, ss)] else []
                        _ -> []

-- Combinación de 2 parsers en uno: Aplica ambos parsers a la entrada y 
-- concatena ambas listas de salida.
(<||>) :: Eq s => Parser s a -> Parser s a -> Parser s a
p1 <||> p2 = \inp -> p1 inp ++ p2 inp

-- Combinación secuencial de 2 parsers (producto cartesiano): se le aplica 
-- p1 a la entrada, y para cada uno de los restos de su resultado se le aplica p2. 
-- Se devuelve el resto de p2 y, como testigo, la aplicación del testigo de p2 
-- al testigo de p1, que es una función
(<**>) :: Eq s => Parser s (b -> a) -> Parser s b -> Parser s a
p1 <**> p2 = \inp -> [(v1 v2, ss2)  | (v1, ss1) <- p1 inp
                                    , (v2, ss2) <- p2 ss1]

--------------------------
-- Extended Combinators --

-- Crea un parser añadiendo el valor del segundo parámetro a la lista 
-- resultante del parser del primer parámetro
opt :: Eq s => Parser s a -> a -> Parser s a
p `opt` v = p <||> pReturn v

-- Construye un parser: recibe una función y un parser, y le aplica la 
-- función al testigo del resultado del parser, devolviendo el parser final
(<$$>) :: Eq s => (b -> a) -> (Parser s b) -> Parser s a
f <$$> p = pReturn f <**> p

-- Llave faltante: indica qué valor testigo no se inluye en el resultado final,
-- ya que no es necesario computarlo
(<$$) :: Eq s => a -> Parser s b -> Parser s a
f <$$ p = const <$$> pReturn f <**> p

(<**) :: Eq s => Parser s a -> Parser s b -> Parser s a
p <** q = const <$$> p <**> q

(**>) :: Eq s => Parser s a -> Parser s b -> Parser s b
p **> q = id <$$ p <**> q

-- Análogos a los anteriores, pero aplican la función testigo de q al testigo 
-- de p para formar el testigo del parser resultante
(<***>) :: Eq s => Parser s b -> Parser s (b -> a) -> Parser s a
p <***> q = (\a f -> f a) <$$> p <**> q

(<???>) :: Eq s => Parser s a -> Parser s (a -> a) -> Parser s a
p <???> q = p <***> (q `opt` id)

-- Construye un parser que reconoce una secuencia de caracteres pasados 
-- como parámetro al inicio del string
pSyms :: Eq s => [s] -> Parser s [s]
pSyms (x:xs) = (:) <$$> pSym x <**> pSyms xs
pSyms []     = pReturn []

-- Combierten un parser de un elemento en un parser de una secuencia
-- de dichos elementos
pManyWithEmpty :: Eq s => Parser s a -> Parser s [a]
pManyWithEmpty p = (:) <$$> p <**> pManyWithEmpty p `opt` []
-- No incluye la cadena vacía vacío
pMany :: Eq s => Parser s a -> Parser s [a]
pMany p = (:) <$$> p <**> pManyWithEmpty p

-- Función que aplica todas las funciones de la lista a un elemento
applyAll :: a -> [a -> a] -> a
applyAll x (f:fs) = applyAll (f x) fs
applyAll x [] = x

-- Reconoce un parser dentro de 2 cadenas de símbolos
pPack :: Eq s => [s] -> Parser s a -> [s] -> Parser s a
pPack o p c = pSyms o **> p <** pSyms c

-- Construyen un parser para expresiones con operadores (asociación izq y 
-- derecha, cada uno). Primer parser de parámetro es para los operadores 
-- y el segundo para los operandos
pChainL :: Eq s => Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainL op p = applyAll <$$> p <**> pManyWithEmpty (flip <$$> op <**> p)

pChainR :: Eq s => Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainR op p = r where r = p <???> (flip <$$> op <**> r )

-- Otros

pAnyOf :: Eq s => [s] -> Parser s s
pAnyOf = foldr (<||>) pFail . map pSym

pFoldr :: Eq s => (a -> b -> b, b) -> ([s] -> [(a, [s])]) -> Parser s b
pFoldr alg@(op,e) p = pfm where pfm = (op <$$> p <**> pfm) `opt` e

pFoldrSep:: Eq s => (a1 -> b -> b, b) -> Parser s a -> Parser s a1 -> Parser s b
pFoldrSep alg@(op,e) sep p = (op <$$> p <**> pFoldr alg (sep **> p)) `opt` e

pFoldrPrefixed :: Eq s => (a1 -> b -> b, b) -> Parser s a -> Parser s a1 -> Parser s b
pFoldrPrefixed alg@(op,e) c p = pFoldr alg (c **> p)

pList:: Eq s => ([s] -> [(a, [s])]) -> Parser s [a]
pList p = pFoldr ((:), []) p

pListSep:: Eq s => Parser s a -> Parser s b -> Parser s [b]
pListSep s p = pFoldrSep ((:), []) s p

pListPrefixed :: Eq s => Parser s a -> Parser s b -> Parser s [b]
pListPrefixed c p = pFoldrPrefixed ((:), []) c p
