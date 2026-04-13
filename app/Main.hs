module Main where
    import qualified Data.Map as Map

    infixl 9 :$

    --Opis danych w jaki sposób chcemy miec dane naszego programu w formie definicji funkcji DEF zawarte w liscie o nazwie
    -- Prog, sama funkcja ma miec nazwe stringa liste argumentów [Pat] i ciao funckji Expr które jest drzewem binarnym
    data Def = Def Name [Pat] Expr
    data Expr = Var Name | Expr :$ Expr
    type Pat = Name
    type Name = String

    newtype Prog = Prog {progDefs :: [Def]}

    --ta funckja zamienia liste Prog którą mósimy stworzyc z danych na mape definicji funkcji
    type DefMap = Map.Map Name Def

    buildDefMap :: Prog -> DefMap
    buildDefMap (Prog defs) = Map.fromList [ (name, d) | d@(Def name _ _) <- defs ]

    --funckja która zamienia skladnie podzbioru haskela na string w formie ktora nas interesuje
    fromHsString :: String -> [Def]
    fromHsString x = []

    -- z parseREsult Hsmodule na Prog
    fromParseResult :: ParseResult HsModule -> [Def]

    --z modułu na tal na prawde Prog
    fromHsModule :: HsModule -> [Def]



    -- Sciezka refukcji
    rpath :: Expr -> [Expr]
    rpath e = e : maybe [] rpath (rstep e)

    rstep :: Expr -> Maybe Expr
    rstep (I :$ x)                = Just x

