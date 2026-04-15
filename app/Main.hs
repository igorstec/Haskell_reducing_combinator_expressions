module Main where

import qualified Data.Map as Map
import System.Environment (getArgs)

import Language.Haskell.Syntax
import Language.Haskell.Parser
--import Data.Type.Coercion (trans)

infixl 9 :$

-- Opis danych w jaki sposób chcemy miec dane naszego programu w formie definicji funkcji DEF zawarte w liscie o nazwie
-- Prog, sama funkcja ma miec nazwe stringa liste argumentów [Pat] i ciao funckji Expr które jest drzewem binarnym
data Def = Def Name [Pat] Expr deriving (Show)
data Expr = Var Name | Expr :$ Expr deriving (Show)
type Pat = Name
type Name = String

newtype Prog = Prog {progDefs :: [Def]} deriving (Show)

-- Ta funckja zamienia liste Prog którą musimy stworzyc z danych na mape definicji funkcji
type DefMap = Map.Map Name Def

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) = Map.fromList [ (name, d) | d@(Def name _ _) <- defs ]

-- Funkcja która zamienia skladnie podzbioru haskela na string w formie ktora nas interesuje
-- To bedzie dlugir ja mam stringa kombinatorw chce je dac na hsmodule to ParseResult HSModule itd. az [Def]
fromHsString :: String -> Prog
fromHsString = Prog . fromParseResult . parseModule

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult x = case x of
    ParseOk hsModule -> fromHsModule hsModule
    ParseFailed errloc err -> error $ "Blad parsowania: " ++ err ++ " w miejscu" ++ show errloc ++ "\n"


--Tu dostaje juz duzy skomplikowany HsModule i musze go przerobic na liste Def, czyli na nasz wewnetrzny format
-- Nasz Prog to opakowanie na liste Def, a Def to nazwa funkcji, lista argumentów i ciało funkcji, czyli Expr
-- zakładamy ze naazwy funkcji sa unikalne, a argumenty sa niepowtarzajace sie. w jednej definijcii funkcji nie moze byc powtarzajacych sie argumentow, ale w roznych definicjach juz tak
--HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]	 

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ decls) = concatMap transDecl decls

-- Nasza funkcja pomocnicza:
transDecl :: HsDecl -> [Def]
transDecl (HsFunBind matches) = map transMatch matches
transDecl (HsPatBind _ (HsPVar nazwa) rhs _) = [Def (transName nazwa) [] (transRhs rhs)] 
transDecl _                   = []  -- Ignorujemy wszystko inne! Zwracamy pustą listę.


--HsMatch SrcLoc HsName [HsPat] HsRhs [HsDecl]	 
--data Def = Def Name [Pat] Expr
transMatch :: HsMatch -> Def
transMatch (HsMatch _ name pats rhs _) = Def (transName name) (map transPat pats) (transRhs rhs)

transName :: HsName -> Name
transName (HsIdent nazwa)  = nazwa
transName (HsSymbol nazwa) = nazwa

transPat :: HsPat -> Pat
transPat (HsPVar nazwa) = transName nazwa
transPat _ = error "Oczekiwano zwyklej zmiennej jako argumentu!"

transRhs :: HsRhs -> Expr
transRhs (HsUnGuardedRhs expr) = transExp expr
transRhs _ = error "Nie przyjmujemy definicji z guardsami."

transExp :: HsExp -> Expr
transExp (HsApp lewe prawe) = transExp lewe :$ transExp prawe
transExp (HsVar (UnQual (HsIdent nazwa))) = Var nazwa
transExp (HsParen srodek) = transExp srodek
transExp (HsCon (UnQual (HsIdent nazwa))) = Var nazwa
transExp _ = error "Skladnia nieobslugiwana w tym jezyku!"


-- subst :: (Name, Expr) -> Expr -> Expr
-- subst (n, e) (Var x) | x == n    = e
-- subst _ (Var x)                 = Var x


-- Ścieżka redukcji
rpath :: Expr -> [Expr]
rpath e = e : maybe [] rpath (rstep e)

-- Pojedyńcze przejście
rstep :: Expr -> Maybe Expr
rstep (Var "i" :$ x) = Just x
rstep _              = Nothing

prettyDef :: Def -> String
prettyDef (Def name args body) =
    name ++ " " ++ unwords args ++ " = " ++ prettyExpr body

prettyDefnoDecl :: Def -> String
prettyDefnoDecl (Def _ _ body) =
    prettyExpr body

prettyExpr :: Expr -> String
prettyExpr = go False
  where
    go _ (Var name)       = name
    go p (a :$ b) = paren p (go False a ++ " " ++ go True b)
    paren True  s = "(" ++ s ++ ")"
    paren False s = s

-- Wypisuje listę wyrażeń, każde w osobnej linii
printExprs :: [Expr] -> IO ()
printExprs = mapM_ (putStrLn . prettyExpr)

-- rstep :: Expr -> Maybe Expr
-- rstep (I :$ x)                = Just x
-- rstep ((K :$ x) :$ _)         = Just x
-- rstep (((S :$ x) :$ y) :$ z)  = Just ((x :$ z) :$ (y :$ z))
-- rstep (((B :$ x) :$ y) :$ z)  = Just (x :$ (y :$ z))
-- rstep (a :$ b) = case rstep a of
--     Just a' -> Just (a' :$ b)
--     Nothing -> case rstep b of
--         Just b' -> Just (a :$ b')
--         Nothing -> Nothing
-- rstep _ = Nothing

-- Sciezka refukcji

printPath :: Expr -> IO ()
printPath = printExprs . take 30 . rpath


-- GŁÓWNA FUNKCJA PROGRAMU
main :: IO ()
main = do
    args <- getArgs
    case args of
        [plik] -> do
            -- Wczytujemy zawartość pliku do zmiennej typu String
            kodZrodlowy <- readFile plik
            
            -- Tutaj w przyszłości połączysz to z parserem, np.:
            let definicje = fromHsString kodZrodlowy

            putStrLn "--- Zbudowano strukturę Prog ---"
            print definicje

            let mapa = buildDefMap definicje
            putStrLn "Zbudowano mapę!"


            
            -- Na razie tylko potwierdzamy, że plik się wczytał:
            putStrLn $ "Udalo sie wczytac plik: " ++ plik
            putStrLn "Oto jego zawartosc:"
            putStrLn kodZrodlowy

            putStrLn "--- Sciezka redukcji brzmi: ---"
            let myexpr = mapa Map.! "main"  -- Przykładowo bierzemy definicję funkcji "main"
            putStrLn "--- Najpierw main: ---"
            putStrLn $ prettyDefnoDecl myexpr

            
        _ -> 
            putStrLn "Blad argumentow."