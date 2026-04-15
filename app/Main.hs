module Main where

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
fromHsString = Prog . fromParseResult . parseModule

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

rozbij :: Expr -> (Expr, [Expr])
rozbij expr = go expr []
  where
    go (a :$ b) acc = go a (b : acc)
    go reszta acc = (reszta, acc)

-- Podstawia jedno konkretne wyrażenie w miejsce wybranej zmiennej
subst :: (Name, Expr) -> Expr -> Expr
subst (cel, noweWyrazenie) (Var nazwa)
  | nazwa == cel = noweWyrazenie
  | otherwise = Var nazwa
subst podstawienie (lewe :$ prawe) =
  subst podstawienie lewe :$ subst podstawienie prawe

-- Przemianowuje argumenty funkcji na bezpieczne (dodając znak '#'),
-- aby uniknąć problemu kolizji i przechwytywania zmiennych.
alphaRename :: [Name] -> Expr -> ([Name], Expr)
alphaRename [] cialo = ([], cialo)
alphaRename (p : ps) cialo =
  let nowaNazwa = "#" ++ p
      noweCialo = subst (p, Var nowaNazwa) cialo
      (resztaParametrow, ostateczneCialo) = alphaRename ps noweCialo
   in (nowaNazwa : resztaParametrow, ostateczneCialo)

-- Pojedynczy krok redukcji
rstep :: DefMap -> Expr -> Maybe Expr
rstep mapa expr =
  let (glowa, argumenty) = rozbij expr
   in case glowa of
        -- Jeśli głowa to zmienna, szukamy jej w słowniku:
        Var nazwa -> case Map.lookup nazwa mapa of
          Just (Def _ parametry cialo) ->
            if length argumenty >= length parametry
              then
                let (bezpieczneParametry, bezpieczneCialo) = alphaRename parametry cialo
                    -- Wycinamy tylko te argumenty, które funkcja faktycznie "zje"
                    uzyteArgumenty = take (length parametry) argumenty
                    resztaArgumentow = drop (length parametry) argumenty

                    -- Łączymy w pary: parametr -> argument, np. [("#x", K), ("#y", I)]
                    paryDoPodstawienia = zip bezpieczneParametry uzyteArgumenty

                    zredukowaneCialo = foldl (\aktCialo para -> subst para aktCialo) bezpieczneCialo paryDoPodstawienia

                    ostatecznyWynik = foldl (:$) zredukowaneCialo resztaArgumentow
                 in Just ostatecznyWynik
              else
                szukajGdzieIndziej expr
          Nothing ->
            -- To sie nie zdarzy, bo wszystkie zmienne powinny byc zdefiniowane, ale na wszelki wypadek
            szukajGdzieIndziej expr
        _ ->
          -- Głowa nie jest zmienna wiec szukamy dalej w drzewie
          szukajGdzieIndziej expr
  where
    -- Funkcja wędrująca po drzewie w poszukiwaniu miejsca do redukcji
    szukajGdzieIndziej (a :$ b) = case rstep mapa a of
      Just a' -> Just (a' :$ b) -- Najpierw próbujemy zredukować lewą stronę
      Nothing -> case rstep mapa b of
        Just b' -> Just (a :$ b')
        Nothing -> Nothing -- Obie strony to postać normalna
    szukajGdzieIndziej _ = Nothing -- Same zmienne i liście

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

-- GŁÓWNA FUNKCJA PROGRAMU
main :: IO ()
main = do
  args <- getArgs
  case args of
    [plik] -> do
      -- Wczytujemy zawartość pliku do zmiennej typu String
      kodZrodlowy <- readFile plik

      let definicje = fromHsString kodZrodlowy

      let mapa = buildDefMap definicje

      let (Def _ _ myexpr) = mapa Map.! "main"

      printPath mapa myexpr
    _ ->
      putStrLn "Blad argumentow."