module ApplicationLayerGrammar where
import CombinatorParserLibrary
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

data DBCommand =    ADDELEMS String [String]
                | SUBSELEMS String [String]
                | DELELEM String
                | LISTDB
                | TYPE String
                | LOGEXP LogExp 
                deriving Show

data LogExp =     CONSTANT String
                | EXISTS String
                | IS String String
                | NOT LogExp
                | OR LogExp LogExp
                | AND LogExp LogExp
                | THEN LogExp LogExp
                | XOR LogExp LogExp
                | IFF LogExp LogExp
                deriving Show

-------------
-- Parsers --

-- Reconoce parsers dentro de paréntesis
pParens :: Parser Char a -> Parser Char a
pParens p = pPack "(" p ")" 

-- Parser que reconoce una letra
pLetter :: Parser Char Char
pLetter = foldr (<||>) pFail (map pSym "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

-- Parser que reconoce una palabra
pWord :: Parser Char String
pWord = pMany pLetter

pDBCommand :: Parser Char DBCommand
pDBCommand =       pElement 
            <||> TYPE <$$ pSyms "type of " <**> pWord
            <||> LOGEXP <$$> pLogExp

pElement :: Parser Char DBCommand
pElement =   ADDELEMS <$$ pSyms "define " <**> pWord <** pSyms " as " 
                <**> ((++) <$$ pSym ',') `pChainL` pMany pWord 
        <||> SUBSELEMS <$$ pSyms "undefine " <**> pWord <** pSyms " as " 
                <**> ((++) <$$ pSym ',') `pChainL` pMany pWord 
        <||> DELELEM <$$ pSyms "delete " <**> pWord
        <||> LISTDB <$$ pSyms "list"

pQuery :: Parser Char LogExp
pQuery =     EXISTS <$$ pSyms "exists " <**> pWord
        <||> IS <$$ pSyms "is " <**> pWord <** pSyms " of type " <**> pWord

pLogExp :: Parser Char LogExp
pLogExp =    CONSTANT <$$> (pSyms "True" <||> pSyms "False")
        <||> pQuery
        <||> NOT <$$ pSyms "not " <**> pLogExp
        <||> pParens (OR <$$> pLogExp <** pSyms " or " <**> pLogExp)
        <||> pParens (AND <$$> pLogExp <** pSyms " and " <**> pLogExp)
        <||> pParens (THEN <$$> pLogExp <** pSyms " then " <**> pLogExp)
        <||> pParens (XOR <$$> pLogExp <** pSyms " xor " <**> pLogExp)
        <||> pParens (IFF <$$> pLogExp <** pSyms " iff " <**> pLogExp)

---------------
-- Funciones --

-- Si el elemento no existe, lo agrega. Si existe, se fija si tiene asociado el tipo.
-- Si ya lo tiene, retorna el mismo mapa, si no, lo agrega y retorna el mapa actualizado. 
addElement :: String -> String -> M.Map String (S.Set String) -> M.Map String (S.Set String)
addElement elemName typeName map = 
    case (M.lookup elemName map) of
        Nothing -> M.insert elemName (S.fromList [typeName]) map
        Just set -> M.insert elemName (S.insert typeName set) map

addElements :: String -> [String] 
    -> M.Map String (S.Set String) -> M.Map String (S.Set String)
addElements el [] map     = map
addElements el (x:xs) map = addElements el xs (addElement el x map) 

-- Si el elemento no existe, no hace nada. Si existe, le saca el elemento asociado.
-- Si ahora el elemento queda sin elementos asociados, se borra.
substractElement :: String -> String -> M.Map String (S.Set String) -> M.Map String (S.Set String)
substractElement elemName typeName map = 
    case (M.lookup elemName map) of
        Nothing -> map
        Just set -> let newSet = S.delete typeName set 
                        in (if null newSet
                            then M.delete elemName map
                            else M.insert elemName newSet map)

substractElements :: String -> [String]
    -> M.Map String (S.Set String) -> M.Map String (S.Set String)
substractElements el [] map     = map
substractElements el (x:xs) map = substractElements el xs (substractElement el x map)

-- Elimina forzadamente un elemento con sus asociaciones
deleteElement :: String -> M.Map String (S.Set String) -> M.Map String (S.Set String)
deleteElement elemName map = M.delete elemName map

evalLogExp :: LogExp -> M.Map String (S.Set String) -> Bool
evalLogExp (CONSTANT s) map     = if s == "True" then True else False
evalLogExp (EXISTS el) map      = M.member el map
evalLogExp (IS el typ) map      = if el == typ then True else isElementOfType el typ map
evalLogExp (NOT exp) map        = not (evalLogExp exp map)
evalLogExp (OR exp1 exp2) map   = (evalLogExp exp1 map) || (evalLogExp exp2 map)
evalLogExp (AND exp1 exp2) map  = (evalLogExp exp1 map) && (evalLogExp exp2 map)
evalLogExp (THEN exp1 exp2) map =   if (evalLogExp exp1 map) 
                                    then (evalLogExp exp2 map) 
                                    else True
evalLogExp (XOR exp1 exp2) map  = do 
                                    let resExp1 = evalLogExp exp1 map
                                        resExp2 = evalLogExp exp2 map
                                    if resExp1 && resExp2
                                    then False
                                    else if not resExp1 && not resExp2
                                    then False
                                    else True
evalLogExp (IFF exp1 exp2) map = not (evalLogExp (XOR exp1 exp2) map)

-- Se fija recursivamente si el elemento es de cierto tipo. Puede serlo
-- por transitividad. 
-- NOTA: En este esquema, queda en el usuario no formar loops
isElementOfType :: String -> String -> M.Map String (S.Set String) -> Bool
isElementOfType el typ map =    if not (M.member el map)
                                then False
                                else if S.member typ (map M.! el) 
                                then True
                                else do 
                                    let list = S.toList (map M.! el)
                                    case list of
                                        [] -> False
                                        l ->   isElementOfTypeL l typ map

isElementOfTypeL :: [String] -> String -> M.Map String (S.Set String) -> Bool
isElementOfTypeL list typ map = case list of
                                [] -> False
                                (x:xs) ->   if isElementOfType x typ map
                                            then True
                                            else isElementOfTypeL xs typ map



                        