module Main where
import CombinatorParserLibrary
import NetLayerGrammar
import ApplicationLayerGrammar
import System.Console.ANSI
import System.IO
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

main = do
    clearScreen
    setCursorPosition 0 0
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "MAIN GUI"
    putStrLn ""
    putStrLn "Presione '1' para interactuar en la capa de red"
    putStrLn "Presione '2' para interactuar en la capa de aplicación"
    putStrLn "Presione '3' para restaurar archivos"
    putStrLn "Presione 'q' para salir"
    hSetEcho stdin True
    setSGR [SetColor Foreground Vivid White]
    mainOption <- getLine
    case mainOption of
        "1" -> netLayerMain
        "2" -> appLayerMain
        "3" -> do
                    dist <- readFile "../files/routersDistributions/rd1.txt"
                    writeFile "../files/routersDistributions/current.txt" dist
                    writeFile "../files/appLayerInstructions.txt" ""
                    bkpMsg <- readFile "../files/netLayerMessageBackup.txt"
                    writeFile "../files/netLayerMessage.txt" bkpMsg
                    main
        "q" -> return ()
        _   -> main

------------------
-- Net Layer UI --

netLayerMain = do
    clearScreen
    setCursorPosition 0 0
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "NET LAYER"
    putStrLn ""
    putStrLn "Presione '1' para seleccionar las posibles distribuciones de la red"
    putStrLn "Presione '2' para mostrar la distribución de la red"
    putStrLn "Presione '3' para entrar al modo interactivo del lenguaje"
    putStrLn "Presione 'q' para salir"
    hSetEcho stdin True
    setSGR [SetColor Foreground Vivid White]
    netLayerOption <- getLine
    case netLayerOption of
        "1" ->  do
                    setSGR [SetColor Foreground Vivid Blue]
                    putStrLn ""
                    putStrLn "Ingrese una opción de distribución de routers (1 -default-, 2 o 3):"
                    setSGR [SetColor Foreground Vivid White]
                    rdist <- getLine
                    case rdist of
                        "2" -> do 
                                    dist <- readFile "../files/routersDistributions/rd2.txt"
                                    writeFile "../files/routersDistributions/current.txt" dist
                        "3" -> do 
                                    dist <- readFile "../files/routersDistributions/rd3.txt"
                                    writeFile "../files/routersDistributions/current.txt" dist
                        _   -> netLayerMain
                    netLayerMain
        "2" ->  do
                    routers <- readFile "../files/routersDistributions/current.txt"
                    let net = createNet (extract (parse pRouters routers)) 
                    drawNet net
                    putStrLn "Defined procedures:"
                    printProcedures net
                    putStrLn ""
                    putStrLn "Presione cualquier tecla para volver:"
                    setSGR [SetColor Foreground Vivid White]
                    getLine         
                    netLayerMain
        "3" ->  do 
                    routers <- readFile "../files/routersDistributions/current.txt"
                    let net = createNet (extract (parse pRouters routers)) 
                    readAndRunInstruction net
        "q" -> main
        _   -> netLayerMain
        
readAndRunInstruction net = do
    drawNet net
    putStrLn "Defined procedures:"
    printProcedures net
    putStrLn ""
    putStrLn "Inserte instrucción, o 'q' para salir:"
    setSGR [SetColor Foreground Vivid White]
    i <- getLine
    if i /= "q"
    then runInstructions net i readAndRunInstruction
    else netLayerMain

-- Recibe las instrucciones a ejecutar y una función que es llamada en caso de
-- que no se complete el envío del paquete.
runInstructions net inst func = do
    let parseRes = parse pInstruction inst
    case parseRes of
            Left s  -> do -- Erroneous input
                        putStrLn s
                        func net     
            Right ins -> do   
                            let newNet = evalInstruction ins net
                            if netCompleted newNet
                            then do
                                drawNet newNet
                                putStrLn "Mensaje entregado al host destino"
                                deliverMessage
                                getLine
                                main
                            else func newNet

drawNet :: Net -> IO ()
drawNet (N (R ps pl pd) (P ppos pdes) _) = do
    clearScreen
    setSGR [SetColor Foreground Vivid Blue] 
    drawColsDivisions 1
    drawRowsDivisions 0
    setSGR [SetColor Foreground Vivid Green]
    drawRouters pl
    setSGR [SetColor Foreground Vivid Yellow]
    drawHosts ps pd
    setCursorPosition (getY(ppos)*2 + 1) (getX(ppos)*2 + 1)
    if elem ppos pl 
        then setSGR [SetColor Foreground Vivid Red]
        else setSGR [SetColor Foreground Vivid White]
    drawPacket pdes
    setCursorPosition 27 0
    setSGR [SetColor Foreground Vivid White]

drawRowsDivisions :: Int -> IO ()
drawRowsDivisions 26 = return ()
drawRowsDivisions n  = do
    setCursorPosition n 0
    putStr "+-+-+-+-+-+-+-+-+-+-+-+-+"
    drawRowsDivisions (n+2)

drawColsDivisions :: Int -> IO ()
drawColsDivisions 25 = return ()
drawColsDivisions n  = do
    setCursorPosition n 0
    putStrLn "| | | | | | | | | | | | |"
    drawColsDivisions (n+2)

drawRouters :: [Point2D] -> IO ()
drawRouters []     = return ()
drawRouters (h:hs) = do 
    setCursorPosition (getY(h)*2 + 1) (getX(h)*2 + 1)
    putChar 'R'
    drawRouters hs

drawHosts :: Point2D -> Point2D -> IO ()
drawHosts ps pd = do
    setCursorPosition (getY(ps)*2 + 1) (getX(ps)*2 + 1)
    putChar 'S'
    setCursorPosition (getY(pd)*2 + 1) (getX(pd)*2 + 1)
    putChar 'D'

-- ▲▶▼◀ ˄˃˅˂
drawPacket :: Direction -> IO ()
drawPacket d = case d of
                North -> putStr "▲"
                East  -> putStr "▶"
                South -> putStr "▼"
                West  -> putStr "◀"

printProcedures:: Net -> IO ()
printProcedures (N _ _ procList) = print procList

deliverMessage :: IO ()
deliverMessage = do
    msg <- readFile "../files/netLayerMessage.txt"
    writeFile "../files/appLayerInstructions.txt" msg

--------------------------
-- Application Layer UI --

appLayerMain = do
    clearScreen
    setCursorPosition 0 0
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn "APPLICATION LAYER"
    putStrLn ""
    putStrLn "Presione '1' para entrar al modo interactivo cargando la base de datos desde un archivo"
    putStrLn "Presione '2' para entrar al modo interactivo con la base de datos vacía"
    putStrLn "Presione 'q' para salir"
    hSetEcho stdin True
    setSGR [SetColor Foreground Vivid White]
    appLayerOption <- getLine
    case appLayerOption of
        "1" ->  do
                    file <- readFile "../files/appLayerInstructions.txt"
                    let dataBase = readFileInsertions M.empty (lines file)
                    clearScreen
                    setCursorPosition 0 0
                    putStrLn ""
                    readAndRunAppInstruction dataBase
        "2" -> do
            clearScreen
            setCursorPosition 0 0
            putStrLn ""
            readAndRunAppInstruction M.empty
        "q" -> main
        _   -> appLayerMain

readAndRunAppInstruction db = do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn ""
    putStrLn "Inserte instrucción, o 'q' para salir"
    setSGR [SetColor Foreground Vivid White]
    i <- getLine
    clearScreen
    setCursorPosition 0 0
    if i /= "q"
    then runAppInstructions i db
    else appLayerMain

runAppInstructions i db = do
    let parseRes = parse pDBCommand i    
    case parseRes of
        Left s -> do
                    putStrLn s
                    readAndRunAppInstruction db
        Right ins -> case extract parseRes of
                        ADDELEMS e list -> do
                            let newDB = addElements e list db
                            readAndRunAppInstruction newDB
                        SUBSELEMS e list -> do
                            let newDB = substractElements e list db
                            readAndRunAppInstruction newDB
                        DELELEM e -> do
                            let newDB = deleteElement e db
                            readAndRunAppInstruction newDB
                        LISTDB -> do
                            listDB (M.toList db)
                            readAndRunAppInstruction db
                        TYPE s -> do
                            if M.member s db then print $ db M.! s else putStrLn ""
                            readAndRunAppInstruction db
                        LOGEXP logExp -> do
                            print $ evalLogExp logExp db
                            readAndRunAppInstruction db

readFileInsertions :: M.Map String (S.Set String) -> [String] -> M.Map String (S.Set String)
readFileInsertions db insList = do
    case insList of
        [] -> db
        (i:ii) -> do  
                    let parseRes = parse pDBCommand i    
                    case parseRes of
                        Left s -> readFileInsertions db ii
                        Right ins -> case extract parseRes of
                                        ADDELEMS e types -> readFileInsertions (addElements e types db) ii
                                        _ -> readFileInsertions db ii

listDB :: Show a => [a] -> IO ()
listDB [] = putStrLn ""
listDB (x:xs) = do 
                    print x
                    listDB xs