module Main where

import Data.List (nub)
import qualified Data.Map as Map
import Language.Haskell.Parser
import Language.Haskell.Syntax
import System.Environment (getArgs)

infixl 9 :$

-- Opis danych w jaki sposób chcemy miec dane naszego programu w formie definicji funkcji DEF zawarte w liscie o nazwie
-- Prog, sama funkcja ma miec nazwe stringa liste argumentów [Pat] i ciłao funckji Expr które jest drzewem binarnym
data Def = Def Name [Pat] Expr deriving (Show)

data Expr = Var Name | Expr :$ Expr deriving (Show)

type Pat = Name

type Name = String

newtype Prog = Prog {progDefs :: [Def]} deriving (Show)

-- Ta funckcja zamienia liste Prog na mape definicji funkcji
type DefMap = Map.Map Name Def

buildDefMap :: Prog -> DefMap
buildDefMap (Prog defs) = Map.fromList [(name, d) | d@(Def name _ _) <- defs]

-- Funkcja która zamienia skladnie podzbioru haskela w forme która nas intersuje Prog
fromHsString :: String -> Prog
fromHsString = validateProg . Prog . fromParseResult . parseModule

fromParseResult :: ParseResult HsModule -> [Def]
fromParseResult x = case x of
  ParseOk hsModule -> fromHsModule hsModule
  ParseFailed errloc err -> error $ "Blad parsowania: " ++ err ++ " w miejscu" ++ show errloc ++ "\n"

-- Tu mamy juz duzy skomplikowany HsModule i trzeba go przerobic na liste Def, czyli na nasz wewnetrzny format
-- HsModule SrcLoc Module (Maybe [HsExportSpec]) [HsImportDecl] [HsDecl]

fromHsModule :: HsModule -> [Def]
fromHsModule (HsModule _ _ _ _ decls) = concatMap transDecl decls

transDecl :: HsDecl -> [Def]
transDecl (HsFunBind matches) = map transMatch matches
transDecl (HsPatBind _ (HsPVar nazwa) rhs _) = [Def (transName nazwa) [] (transRhs rhs)]
transDecl _ = []

-- HsMatch SrcLoc HsName [HsPat] HsRhs [HsDecl]

transMatch :: HsMatch -> Def
transMatch (HsMatch _ name pats rhs _) = Def (transName name) (map transPat pats) (transRhs rhs)

transName :: HsName -> Name
transName (HsIdent nazwa) = nazwa
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

flattenApp :: Expr -> (Expr, [Expr])
flattenApp expr = go expr []
  where
    go (a :$ b) acc = go a (b : acc)
    go rest acc = (rest, acc)

-- Podstawia jedno konkretne wyrażenie w miejsce wybranej zmiennej
subst :: (Name, Expr) -> Expr -> Expr
subst (target, newExpr) (Var name)
  | name == target = newExpr
  | otherwise = Var name
subst sub (left :$ right) =
  subst sub left :$ subst sub right

-- Przemianowuje argumenty funkcji na bezpieczne (dodając znak '#'),
-- aby uniknąć problemu kolizji i przechwytywania zmiennych.
alphaRename :: [Name] -> Expr -> ([Name], Expr)
alphaRename [] body = ([], body)
alphaRename (p : ps) body =
  let newName = "#" ++ p
      newBody = subst (p, Var newName) body
      (restParams, finalBody) = alphaRename ps newBody
   in (newName : restParams, finalBody)

-- Pojedynczy krok redukcji
rstep :: DefMap -> Expr -> Maybe Expr
rstep defMap expr =
  let (headExpr, args) = flattenApp expr
   in case headExpr of
        -- Jeśli głowa to zmienna, szukamy jej w słowniku:
        Var name -> case Map.lookup name defMap of
          Just (Def _ params body) ->
            if length args >= length params
              then
                let (safeParams, safeBody) = alphaRename params body
                    -- Wycinamy tylko te argumenty, które funkcja faktycznie "zje"
                    usedArgs = take (length params) args
                    restArgs = drop (length params) args

                    -- Łączymy w pary: parametr -> argument, np. [("#x", K), ("#y", I)]
                    substPairs = zip safeParams usedArgs

                    reducedBody = foldl (\currBody pair -> subst pair currBody) safeBody substPairs

                    finalResult = foldl (:$) reducedBody restArgs
                 in Just finalResult
              else
                searchElsewhere expr
          Nothing ->
            -- To sie nie zdarzy, bo wszystkie zmienne powinny byc zdefiniowane, ale na wszelki wypadek
            searchElsewhere expr
        _ ->
          -- Głowa nie jest zmienna wiec szukamy dalej w drzewie
          searchElsewhere expr
  where
    -- Funkcja wędrująca po drzewie w poszukiwaniu miejsca do redukcji
    searchElsewhere (a :$ b) = case rstep defMap a of
      Just a' -> Just (a' :$ b) -- Najpierw próbujemy zredukować lewą stronę
      Nothing -> case rstep defMap b of
        Just b' -> Just (a :$ b')
        Nothing -> Nothing -- Obie strony to postać normalna
    searchElsewhere _ = Nothing -- Same zmienne i liście

rpath :: DefMap -> Expr -> [Expr]
rpath mapa e = e : maybe [] (rpath mapa) (rstep mapa e)

printPath :: DefMap -> Expr -> IO ()
printPath mapa = mapM_ (putStrLn . prettyExpr) . take 30 . rpath mapa

prettyExpr :: Expr -> String
prettyExpr = go False
  where
    go _ (Var name) = name
    go p (a :$ b) = paren p (go False a ++ " " ++ go True b)
    paren True s = "(" ++ s ++ ")"
    paren False s = s

-- Główna funkcja walidująca cały program
validateProg :: Prog -> Prog
validateProg (Prog defs) =
  Prog (checkMain . checkUniqueArgs . checkUniqueDefs $ defs)

checkUniqueDefs :: [Def] -> [Def]
checkUniqueDefs defs =
  let nazwy = map (\(Def nazwa _ _) -> nazwa) defs

      findDup [] = Nothing
      findDup (x : xs) = if x `elem` xs then Just x else findDup xs
   in case findDup nazwy of
        Just dup -> error $ "Blad Poprawnosci"
        Nothing -> defs

checkUniqueArgs :: [Def] -> [Def]
checkUniqueArgs defs =
  -- Wyłapujemy te definicje, w których liczba unikalnych argumentów (nub)
  -- jest inna niż całkowita liczba argumentów
  let badDefs = filter (\(Def _ argumenty _) -> length (nub argumenty) /= length argumenty) defs
   in case badDefs of
        (Def nazwa _ _ : _) -> error $ "Blad Poprawnosci: Kombinator"
        [] -> defs

-- Sprawdza, czy istnieje 'main' i czy nie ma argumentów
checkMain :: [Def] -> [Def]
checkMain defs =
  let mainDefs = filter (\(Def nazwa _ _) -> nazwa == "main") defs
   in case mainDefs of
        [] -> error "Blad Poprawnosci: Program nie zawiera definicji 'main'!"
        [Def _ argumenty _] ->
          if null argumenty
            then defs
            else error "Blad Poprawnosci"
        _ -> defs -- Przypadek wielu 'main' zostanie i tak wyłapany przez checkUniqueDefs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [plik] -> do
      -- Wczytujemy zawartość pliku do zmiennej typu String
      kodZrodlowy <- readFile plik

      let definitions = fromHsString kodZrodlowy

      let mapa = buildDefMap definitions

      let (Def _ _ myexpr) = mapa Map.! "main"

      printPath mapa myexpr
    _ ->
      putStrLn "Blad argumentow."