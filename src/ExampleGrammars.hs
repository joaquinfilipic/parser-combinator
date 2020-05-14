module ExampleGrammars where
import CombinatorParserLibrary

-- Reconoce parsers dentro de paréntesis
pParens :: Parser Char a -> Parser Char a
pParens p = pPack "(" p ")"

-- Parser que reconoce cantidad de paréntesis anidados que abren y cierran
pParenthesis :: Parser Char Int
pParenthesis = (max .(1+)) <$$> pParens pParenthesis <**> pParenthesis `opt` 0

-- Parser que reconoce una letra minúscula
pLetter :: Parser Char Char
pLetter = foldr (<||>) pFail (map pSym "abcdefghijklmnopqrstuvwxyz")

-- Parser que reconoce un símbolo
pSymbol :: Parser Char Char
pSymbol = foldr (<||>) pFail (map pSym "+-*/_ ()?,.")

-- Parser que reconoce un dígito
pDigit :: Parser Char Char
pDigit = foldr (<||>) pFail (map pSym "0123456789")

-- Parser que reconoce un dígito pero lo devuelve como Integer
pDigitAsInt :: Parser Char Int
pDigitAsInt = (\c -> fromEnum c - fromEnum '0') <$$> pDigit

-- Parser que reconoce números naturales
pNatural :: Parser Char Int
pNatural = foldl (\a b -> a * 10 + b) 0 <$$> pMany pDigitAsInt

-- Parser que reconoce números enteros
pInteger :: Parser Char Int
pInteger = (negate <$$ (pSyms "-") `opt` id) <**> pNatural

-- Parser para expresiones que contengan sumas y restas
pPlusMinus :: Parser Char Int
pPlusMinus = ((-) <$$ pSyms "-" <||> (+) <$$ pSyms "+") `pChainL` pInteger

-- Asocia un símbolo de operador terminal con una semántica
pOp :: Eq s => (a, [s]) -> Parser s a
pOp (sem, symbol) = sem <$$ pSyms symbol

-- Toma una lista de parsers y los combina en uno
pChoice :: Eq s => Foldable t => t (Parser s a) -> Parser s a
pChoice ps = foldr (<||>) pFail ps 

anyOp :: [(a, [Char])] -> Parser Char a
anyOp = pChoice.map pOp
addops :: Parser Char (Int -> Int -> Int)
addops = anyOp [((+), "+"), ((-), "-")]
mulops :: Parser Char (Int -> Int -> Int)
mulops = anyOp [((*), "*")]

-- Reconoce operandos. Para agregar, añadir cada operador con su semántica
-- en su respectivo nivel de precedencia
pPlusMinusTimes :: Parser Char Int
pPlusMinusTimes = foldr pChainL pInteger [addops, mulops]

----------------------

-- pExpr: Parser para reconocer y evaluar expresiones con operadores comunes, 
-- paréntesis, condicionales (if-then-else).
pExpr :: Parser Char Int
pExpr = foldr pChainL pFactor [addops, mulops] <||> pIfThenElse

pFactor = pInteger <||> pPack "(" pExpr ")"
pIfThenElse = choose    <$$ pSyms "if" <**> pBoolExpr
                        <** pSyms "then" <**> pExpr
                        <** pSyms "else" <**> pExpr

choose c t e = if c then t else e
pBoolExpr = foldr pChainR pRelExpr [orops, andops]
pRelExpr =          True <$$ pSyms "True"
            <||>    False <$$ pSyms "False"
            <||>    pExpr <***> pRelOp <**> pExpr

andops = anyOp [((&&) , "&&")]
orops = anyOp [((||) , "||")]
pRelOp = anyOp [((<=), "<="), ((>=), ">="), ((==), "=="), 
                ((/= ), "/="), ((<), "<"), ((>) , ">")]

-----------------------

-- pTree: Parser para reconocer una estructura arbórea con 
-- enteros en sus hojas
data Tree = Leaf Char
            | Bin Tree Tree
            deriving Show

pTree :: Parser Char Tree
pTree =       Leaf <$$> pDigit 
        <||>  pParens (Bin <$$> pTree <**> pTree)

