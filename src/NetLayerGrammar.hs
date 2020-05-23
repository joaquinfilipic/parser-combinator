module NetLayerGrammar where
import CombinatorParserLibrary

--------------------
-- Tipos de datos --

data Packet = P {
    packetPosition :: Point2D, 
    packetDirection :: Direction 
} deriving (Eq, Show)

data Direction = North | East | South | West deriving (Eq, Show)

data Instruction =   ListI [Instruction]
                    | Loop Int Instruction
                    | RightC Int
                    | LeftC Int
                    | ForwardC Int
                    | Translate
                    | Proc String
                    | ProcDef Procedure
                    deriving (Eq, Show)

data Procedure = PR String Instruction deriving (Eq, Show)

data Routers = R Point2D [Point2D] Point2D deriving (Eq, Show)

data Point2D = P2D Int Int deriving (Eq, Show)

data Net = N {
    netRouters :: Routers,
    netPacket :: Packet,
    netProcedures :: [Procedure]
} deriving (Eq, Show)

--------------
-- Parsers --

-- Reconoce parsers dentro de paréntesis
pParens :: Parser Char a -> Parser Char a
pParens p = pPack "(" p ")"

-- Reconoce parsers dentro de llaves
pBraces :: Parser Char a -> Parser Char a
pBraces p = pPack "{" p "}"

-- Parser que reconoce una letra
pLetter :: Parser Char Char
pLetter = foldr (<||>) pFail (map pSym "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

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

-- Reconoce los comandos simples
pCommand :: Parser Char Instruction
pCommand =        LeftC <$$> pNatural <** pSyms "Left"
            <||>  LeftC 1 <$$ pSyms "Left"
            <||>  RightC <$$> pNatural <** pSyms "Right"
            <||>  RightC 1 <$$ pSyms "Right"
            <||>  ForwardC <$$> pNatural <** pSyms "Front"
            <||>  ForwardC 1 <$$ pSyms "Front"

-- Test parser: Reconoce comandos simples separados por un +
pCommandsTest :: Parser Char [Instruction]
pCommandsTest = ((++) <$$ pSym '+') `pChainL` pMany pCommand 

-- Reconoce el patrón del loop
pLoop :: Parser Char Instruction
pLoop = Loop <$$> pNatural <** pSym '*' <**> pParens (pInstruction)

-- Reconoce el comando de envío de un mensaje
pTranslate :: Parser Char Instruction
pTranslate = Translate <$$ pSyms "Translate" 

-- Reconoce la definición de un procedimiento
pProcedureDef :: Parser Char Procedure
pProcedureDef = PR  <$$> pMany pLetter <** pSym '=' 
                    <**> pParens (pInstruction)

-- Reconoce la utilización de un procedimiento
pProcedure :: Parser Char Instruction
pProcedure = Proc <$$> (pMany pLetter)

-- Parser que reconoce separadores de instrucciones
pInstSeparator :: Parser Char Char
pInstSeparator = foldr (<||>) pFail (map pSym "+\n") 

-- Parser para las instrucciones.
-- Agrupa en una lista todas las instrucciones, sean concatenación de
-- comandos simples, loops o definición y uso de procedimientos
pInstruction :: Parser Char Instruction
pInstruction =  ListI <$$> (((++) <$$ (pInstSeparator) )
                    `pChainL` 
                  pMany (        pCommand 
                            <||> pTranslate  
                            <||> pLoop
                            <||> ProcDef <$$> pProcedureDef
                            <||> pProcedure))
                <||>  pLoop

-- Reconoce un punto 2D
pPoint2D :: Parser Char Point2D
pPoint2D = P2D <$$> pInteger <** pSyms "," <**> pInteger

-- EXTRA: Parser para los routers.
-- Reconoce el punto de inicio, una serie de puntos intermedios 
-- y el punto final.
-- Ex: pLevelMap "I22,3 3,2 7,8 5,3 F9,2"
pRouters :: Parser Char Routers
pRouters = R  <$$ pSyms "I" <**> pPoint2D 
              <** pSyms " " <**> pMany (pPoint2D <** pSyms " ")
              <** pSyms "F" <**> pPoint2D

-----------------------------
-- Funciones de evaluación --

evalInstruction :: Instruction -> Net -> Net
evalInstruction (RightC n)         = applyManyTimes n turnRight
evalInstruction (LeftC n)          = applyManyTimes n turnLeft
evalInstruction (ForwardC n)       = applyManyTimes n moveForward
evalInstruction (ListI list)       = applyToList list evalInstruction
evalInstruction (Loop n inst)      = applyLoop n inst evalInstruction
evalInstruction (Translate)        = translate
evalInstruction (Proc s)           = \n -> evalInstruction (replaceProcedure s n) n
evalInstruction (ProcDef p)        = evalProcedureDef p

turnRight :: Net -> Net
turnRight n@(N _ (P p North) _)   = n { netPacket = (P p East)}
turnRight n@(N _ (P p East) _)    = n { netPacket = (P p South)}
turnRight n@(N _ (P p South) _)   = n { netPacket = (P p West)}
turnRight n@(N _ (P p West) _)    = n { netPacket = (P p North)}

turnLeft :: Net -> Net
turnLeft n@(N _ (P p North) _)    = n { netPacket = (P p West)}
turnLeft n@(N _ (P p East) _)     = n { netPacket = (P p North)}
turnLeft n@(N _ (P p South) _)    = n { netPacket = (P p East)}
turnLeft n@(N _ (P p West) _)     = n { netPacket = (P p South)}

moveForward :: Net -> Net
moveForward (N h (P p North) pr) 
    = N h (P (P2D (getX p) (checkMove ((getY p)-1))) North) pr
moveForward (N h (P p East) pr)  
    = N h (P (P2D (checkMove ((getX p)+1)) (getY p)) East) pr
moveForward (N h (P p South) pr) 
    = N h (P (P2D (getX p) (checkMove((getY p)+1))) South) pr
moveForward (N h (P p West) pr)  
    = N h (P (P2D (checkMove((getX p)-1)) (getY p)) West) pr

-- Saca el router por donde pasó paquete de la lista de pendientes
translate :: Net -> Net
translate (N (R ps pl pd) (P p d) pr) = 
    N   (R ps (substractIfPresent pl (p)) pd) 
        (P p d) 
        pr

replaceProcedure :: String -> Net -> Instruction
replaceProcedure s (N h p []) = ListI []
replaceProcedure s (N h p ((PR name mov):rest)) 
    = if s == name then mov else replaceProcedure s (N h p rest)

applyManyTimes :: Int -> (a -> a) -> a -> a
applyManyTimes 0 f x = x 
applyManyTimes n f x = applyManyTimes (n-1) f (f x)

applyToList :: [b] -> (b -> a -> a) -> a -> a 
applyToList []     f x  = x
applyToList (s:ss) f x  =  applyToList ss f (f s x)

applyLoop :: Int -> b -> (b -> a -> a) -> a -> a
applyLoop 0 m f x = x
applyLoop n m f x = applyLoop (n-1) m f (f m x)

getX :: Point2D -> Int
getX (P2D x y) = x

getY :: Point2D -> Int
getY (P2D x y) = y

checkMove :: Int -> Int
checkMove n = if n < 0 then 0 else (if n > 11 then 11 else n) 

substractIfPresent :: Eq a => [a] -> a -> [a]
substractIfPresent list element = filter (\e -> e /= element) list 

evalProcedureDef :: Procedure -> Net -> Net
evalProcedureDef pr (N h pkt pl) = N h pkt (pl++[pr])

-- Crea la red, en estado inicial, con los routers parseados
createNet :: Routers -> Net
createNet (R ps pl pd) = 
    N (R ps pl pd) (P (P2D (getX ps) (getY ps) ) North) []

-- Determina si el envío sobre la red se completó exitosamente. Para ello, 
-- se tuvo que haber pasado por todos los routers indicadas en la red y haber
-- terminado en el router inicial (a modo de ACK)
netCompleted :: Net -> Bool      
netCompleted (N (R ps pl pd) (P pos dir) pr) = (pl == [] && pd == pos)
