import Text.Parsec
import System.Environment
import Data.List

type Potpolja = String
type Sadrzaj = String
type Tupl = (Potpolja, Sadrzaj)

data Parsiranje = Parsiranje {getGlavnoPolje :: Int,
                              getIndikator :: String,
                              getListu :: [Tupl],
                              getListaPotPolja :: [PotParsiranje]}

data PotParsiranje = PotParsiranje {getPotPoljePolja :: String,
                                    getGlPolje :: Int,
                                    getInd :: String,
                                    getTupl :: [Tupl]} deriving Eq



toString :: [Parsiranje] -> String
toString ps = unlines . fmap show $ ps

toStringPotP :: [PotParsiranje] -> String
toStringPotP ps = unwords . fmap show $ ps

showGp :: Int -> String
showGp n 
    | n < 10 = "00" ++ show n
    | n < 100 = "0" ++ show n
    | otherwise = show n


instance Show Parsiranje where
    show (Parsiranje glavnoPolje indikator tupl listaPotPolja) = (showGp glavnoPolje) ++ " " ++ (read (show indikator)) ++ " " ++ (showListHelper tupl) ++ (showL listaPotPolja)
            where showListHelper [] = ""
                  showListHelper (x:xs) = "[" ++ (read (show (fst x))) ++ "]" ++ (read (show (snd x))) ++ "  " ++ showListHelper xs
                  showL [] = ""
                  showL l = (toStringPotP l)

instance Show PotParsiranje where
    show (PotParsiranje potPolje glavnoPolje indikator tuplPotpolja) = "[" ++ (read (show potPolje)) ++ "]  {{" ++      (showGp glavnoPolje) ++ " " ++ (read (show indikator)) ++ "  " ++ (showTuplove tuplPotpolja) ++ "}}  "
            where showTuplove [] = ""
                  showTuplove (x:xs) = (read (show (fst x))) ++ " " ++ (read (show (snd x))) ++ " " ++ showTuplove xs

-- JSON
class ToJSON a where
  toJSON :: a -> String
--

instance ToJSON Parsiranje where
    toJSON (Parsiranje glavnoPolje indikator lista listaPotPolja) = "{\n" ++ (showGlavnoPolje (show glavnoPolje)) ++ (showInd indikator) ++ (showList lista) ++ (parsiranjaUJSONPot listaPotPolja) ++ "\n}"
            where showGlavnoPolje "" = ""
                  showGlavnoPolje br = "\t \"glavnoPolje\": " ++ (show br) ++ ",\n"
                  showInd "" = ""
                  showInd ind = "\t \"indikator\": " ++ (show ind) ++ ",\n"
                  showList [] = ""
                  showList l = "\t \"potpolja\": [" ++ listHlpr l ++ "\t\n\t  ]"
                    where listHlpr [] = ""
                          listHlpr (x:xs) = "\n\t\t" ++ (help x) ++ listHlpr xs
                            where help tupl = "\"potpolje\": " ++ (show (fst tupl)) ++ ", \n\t\t\"sadrzaj\":" ++ (show (snd tupl))

instance ToJSON PotParsiranje where
    toJSON (PotParsiranje potPolje glavnoPoljePotpolja indikatorPotpolja tuplPotpolja) = (showPotpolje potPolje) ++ (showGlavnoPolje (show glavnoPoljePotpolja)) ++ (showIndik indikatorPotpolja) ++ (showTupl tuplPotpolja) ++ "\n\t ]"
            where toJSON [] = ""
                  showPotpolje "" = ""
                  showPotpolje polje = "\t \"potparsiranja\": [\n\t\t\t\"potpolje\" :" ++ (show polje) ++ ",\n"
                  showGlavnoPolje "" = ""
                  showGlavnoPolje br = "\t\t\t\"glavnoPoljePotpolja\": " ++ (show br) ++ ",\n"
                  showIndik "" = ""
                  showIndik ind = "\t\t\t\"indikatorPotpolja\": " ++ (show ind) ++ ",\n"
                  showTupl [] = ""
                  showTupl t = "\t\t\t\"listaTuplova\" : [" ++ tuplHlpr t ++ "\n\t\t\t]"
                        where tuplHlpr [] = ""
                              tuplHlpr (x:xs) = "\n\t\t\t\t" ++ (hlp x) ++ tuplHlpr xs
                                    where hlp tupl = "\"potpoljePolja\": " ++ (show (fst tupl)) ++ ",\n\t\t\t\t\"sadrzajPotpolja\":" ++ (show (snd tupl))

---  Dodatno odradjeno --- 

izmeniIndikator :: [Parsiranje] -> Int -> String -> [Parsiranje]
izmeniIndikator ps gp ind = fmap tmp ps
    where
        tmp :: Parsiranje -> Parsiranje
        tmp p
          | getGlavnoPolje p == gp = izmeniIndikator' p ind
          | otherwise = p

izmeniIndikator' :: Parsiranje -> String -> Parsiranje
izmeniIndikator' p ind = Parsiranje { 
    getGlavnoPolje = getGlavnoPolje p,
    getIndikator = ind,
    getListu = getListu p,
    getListaPotPolja = getListaPotPolja p }

--- Dodavanje i brisanje za polja 

proveraPolja :: [Parsiranje]  -> [Parsiranje]
proveraPolja par = [p | p <- par, (getListu p) /= []  || (dodatnaProvera (getListaPotPolja p)) /= [] ]

dodatnaProvera :: [PotParsiranje] -> [PotParsiranje]
dodatnaProvera pp = [p | p <- pp, (getTupl p) /= [] ]
 

dodajPotpolje :: [Parsiranje] -> Int -> String -> String -> [Parsiranje]
dodajPotpolje ps gp pp sad = fmap dodaj ps
    where
        dodaj :: Parsiranje -> Parsiranje
        dodaj p
            | getGlavnoPolje p == gp = dodajPotpolje' p pp sad
            | otherwise = p


dodajPotpolje' :: Parsiranje -> String -> String -> Parsiranje
dodajPotpolje' p pp sad = Parsiranje {
    getGlavnoPolje = getGlavnoPolje p,
    getIndikator = getIndikator p,
    getListu = dodajNoviElement (getListu p) pp sad,
    getListaPotPolja = getListaPotPolja p }


dodajNoviElement :: [Tupl] -> String -> String -> [Tupl]
dodajNoviElement l pp sad = (pp, sad):l


obrisiPotpolje :: [Parsiranje] -> Int -> String -> [Parsiranje]
obrisiPotpolje par gp potpoljeZaBrisanje = proveraPolja $ fmap obrisi par
    where
        obrisi :: Parsiranje -> Parsiranje
        obrisi p
            | getGlavnoPolje p == gp = nadjiTupl p potpoljeZaBrisanje
            | otherwise = p


nadjiTupl :: Parsiranje -> String -> Parsiranje
nadjiTupl par str = Parsiranje {
    getGlavnoPolje = getGlavnoPolje par,
    getIndikator = getIndikator par,
    getListu = ukloniT (getListu par) str,
    getListaPotPolja = getListaPotPolja par }

ukloniT :: [Tupl] -> String -> [Tupl]
ukloniT t s = filter (\x -> x /= ("", "")) $ fmap sveElem t
    where
        sveElem :: Tupl -> Tupl
        sveElem tupl
            | fst tupl == s = ("", "")
            | otherwise = tupl


--- Dodavanje i brisanje za sekundarna potpolja

dodavanjeUSekundarnaPolja :: [Parsiranje] -> Int -> Int -> String -> String -> [Parsiranje]
dodavanjeUSekundarnaPolja ps gp gpPotpolja sp sad = fmap dodaj ps
    where
        dodaj :: Parsiranje -> Parsiranje
        dodaj p
            | getGlavnoPolje p == gp = dodavanjeUSekundarnaPolja' p gpPotpolja sp sad
            | otherwise = p



dodavanjeUSekundarnaPolja'  :: Parsiranje -> Int -> String -> String -> Parsiranje
dodavanjeUSekundarnaPolja' p gp pp sad = Parsiranje {
    getGlavnoPolje = getGlavnoPolje p,
    getIndikator = getIndikator p,
    getListu = getListu p,
    getListaPotPolja = ulazUPotparsiranje (getListaPotPolja p) gp pp sad }


ulazUPotparsiranje :: [PotParsiranje] -> Int -> String -> String -> [PotParsiranje]
ulazUPotparsiranje ps gp pp sad = fmap prolaz ps
    where
        prolaz :: PotParsiranje -> PotParsiranje
        prolaz p
            | getGlPolje p == gp = prolazKrozPotParsiranje p pp sad
            | otherwise = p

prolazKrozPotParsiranje :: PotParsiranje -> String -> String -> PotParsiranje
prolazKrozPotParsiranje pp potpolje sadrzaj = PotParsiranje {
    getPotPoljePolja = getPotPoljePolja pp,
    getGlPolje = getGlPolje pp,
    getInd = getInd pp,
    getTupl = dodajSek (getTupl pp) potpolje sadrzaj }


dodajSek :: [Tupl] -> String -> String -> [Tupl]
dodajSek tupls potpolje sadrzaj = (potpolje, sadrzaj):tupls


brisanjeUSekundarnimPoljima :: [Parsiranje] -> Int -> Int -> String -> [Parsiranje]
brisanjeUSekundarnimPoljima ps gp sekGp pp = proveraPolja $ fmap prolazakKrozParsiranja ps
    where
        prolazakKrozParsiranja :: Parsiranje -> Parsiranje
        prolazakKrozParsiranja p 
            | getGlavnoPolje p == gp = prolazakKrozParsiranje p sekGp pp
            | otherwise = p


prolazakKrozParsiranje :: Parsiranje -> Int -> String -> Parsiranje
prolazakKrozParsiranje p sekGp sad = Parsiranje {
    getGlavnoPolje = getGlavnoPolje p,
    getIndikator = getIndikator p,
    getListu = getListu p,
    getListaPotPolja = ulazUPotparsiranje2 (getListaPotPolja p) sekGp sad }


ulazUPotparsiranje2 :: [PotParsiranje] -> Int -> String -> [PotParsiranje]
ulazUPotparsiranje2 pp gp sad = fmap prolaz2 pp
    where 
        prolaz2 :: PotParsiranje -> PotParsiranje
        prolaz2 p
            | getGlPolje p == gp = prolazKrozPotParsiranje2 p sad
            | otherwise = p


prolazKrozPotParsiranje2 :: PotParsiranje -> String -> PotParsiranje
prolazKrozPotParsiranje2 pp potpolje = PotParsiranje {
    getPotPoljePolja = getPotPoljePolja pp,
    getGlPolje = getGlPolje pp,
    getInd = getInd pp,
    getTupl = ukloniSekPolje (getTupl pp) potpolje }

ukloniSekPolje :: [Tupl] -> String -> [Tupl]
ukloniSekPolje tupls pp = filter (\x -> x /= ("", "")) $ fmap sveElem tupls
    where
        sveElem :: Tupl -> Tupl
        sveElem tupl
            | fst tupl == pp = ("", "")
            | otherwise = tupl

promenaSadrzajaPotpolja :: [Parsiranje] -> Int -> String  -> String -> [Parsiranje]
promenaSadrzajaPotpolja ps gp pp noviSadrzaj = fmap prolazak ps
    where 
        prolazak :: Parsiranje -> Parsiranje
        prolazak p 
            | getGlavnoPolje p == gp = promenaSadrzaja p pp noviSadrzaj
            | otherwise = p

promenaSadrzaja :: Parsiranje -> String -> String -> Parsiranje
promenaSadrzaja ps pp noviSadrzaj = Parsiranje {
    getGlavnoPolje = getGlavnoPolje ps,
    getIndikator = getIndikator ps,
    getListu = promenaSad (getListu ps) pp noviSadrzaj,
    getListaPotPolja = getListaPotPolja ps }

promenaSad :: [Tupl] -> String -> String -> [Tupl]
promenaSad tupl potpolje sadrzaj = fmap nadjiPotpolje tupl
    where
        nadjiPotpolje :: Tupl -> Tupl
        nadjiPotpolje t 
            | fst t == potpolje = (potpolje, sadrzaj)
            | otherwise = t

promenaSadrzajaSekundarnogPolja :: [Parsiranje] -> Int -> Int -> String -> String -> [Parsiranje]
promenaSadrzajaSekundarnogPolja ps gp sgp pp noviSadrzaj = fmap prolazakSekundarnihPolja ps
    where
        prolazakSekundarnihPolja :: Parsiranje -> Parsiranje
        prolazakSekundarnihPolja p
            | getGlavnoPolje p == gp = promenaSadrzajaSekundarnog p sgp pp noviSadrzaj
            | otherwise = p

promenaSadrzajaSekundarnog :: Parsiranje -> Int -> String -> String -> Parsiranje
promenaSadrzajaSekundarnog ps sgp pp sadrzaj = Parsiranje {
    getGlavnoPolje = getGlavnoPolje ps,
    getIndikator = getIndikator ps,
    getListu = getListu ps,
    getListaPotPolja = ulazakUPotparsiranje (getListaPotPolja ps) sgp pp sadrzaj }

ulazakUPotparsiranje :: [PotParsiranje] -> Int -> String -> String -> [PotParsiranje]
ulazakUPotparsiranje ps gp pp sad = fmap prolazSek ps
    where
        prolazSek :: PotParsiranje -> PotParsiranje
        prolazSek p
            | getGlPolje p == gp = promenaSadrzajaSek p pp sad
            | otherwise = p


promenaSadrzajaSek :: PotParsiranje -> String -> String -> PotParsiranje
promenaSadrzajaSek ps pp noviSadrzaj = PotParsiranje {
    getPotPoljePolja = getPotPoljePolja ps,
    getGlPolje = getGlPolje ps,
    getInd = getInd ps,
    getTupl = promenaSadrzajaS (getTupl ps) pp noviSadrzaj }


--- Pretrazivanje elemenata kod PodPolja


pretrazivanjeElemenataPotPolja :: [Parsiranje] -> Int -> String -> [Parsiranje]
pretrazivanjeElemenataPotPolja ps gp pp = proveraPolja $ fmap pretraga ps
    where
        pretraga :: Parsiranje -> Parsiranje
        pretraga p
            | getGlavnoPolje p == gp = pretrazivanjeElemenata p pp 
            | otherwise = praznoParsiranje p

praznoParsiranje :: Parsiranje -> Parsiranje
praznoParsiranje ps = Parsiranje {
    getGlavnoPolje = getGlavnoPolje ps,
    getIndikator = getIndikator ps,
    getListu = praznuListu (getListu ps),
    getListaPotPolja = praznaListaPotParsiranja (getListaPotPolja ps) }

praznuListu :: [Tupl] -> [Tupl]
praznuListu tupls = filter (\x -> x /= ("", "")) $ fmap vratiPrazno tupls
    where
        vratiPrazno :: Tupl -> Tupl
        vratiPrazno t = ("", "")


praznaListaPotParsiranja :: [PotParsiranje] -> [PotParsiranje]
praznaListaPotParsiranja ps = fmap praznoPotParsiranje ps
    where
        praznoPotParsiranje :: PotParsiranje -> PotParsiranje
        praznoPotParsiranje ps = PotParsiranje {
            getPotPoljePolja = getPotPoljePolja ps,
            getGlPolje = getGlPolje ps,
            getInd = getInd ps,
            getTupl = praznuListu (getTupl ps) }



pretrazivanjeElemenata :: Parsiranje -> String -> Parsiranje
pretrazivanjeElemenata ps pp = Parsiranje {
    getGlavnoPolje = getGlavnoPolje ps,
    getIndikator = getIndikator ps,
    getListu = pretragaKrozListu (getListu ps) pp,
    getListaPotPolja = getListaPotPolja ps }


pretragaKrozListu :: [Tupl] -> String -> [Tupl]
pretragaKrozListu tupl pp = filter (\x -> x /= ("", "")) $ fmap prolaz3 tupl
    where
        prolaz3 :: Tupl -> Tupl
        prolaz3 t
            | fst t == pp = t 
            | otherwise = ("", "")


promenaSadrzajaS :: [Tupl] -> String -> String -> [Tupl]
promenaSadrzajaS tupl pp sadrzaj = fmap krozProlaz tupl
    where
        krozProlaz :: Tupl -> Tupl
        krozProlaz t
            | fst t == pp = (pp, sadrzaj)
            | otherwise = t



--- Pretrazivanje elemenata kod SekundarnaPolja

pretrazivanjeElemenataSekundarnihPolja :: [Parsiranje] -> Int -> String -> Int -> String -> [Parsiranje]
pretrazivanjeElemenataSekundarnihPolja ps gp pp sgp spp = proveraPolja $ fmap nadji ps
    where
        nadji :: Parsiranje -> Parsiranje
        nadji p
            | getGlavnoPolje p == gp = pretrazivanjeElemenataSekundarnih p pp sgp spp
            | otherwise = praznoParsiranje p


pretrazivanjeElemenataSekundarnih :: Parsiranje -> String -> Int -> String -> Parsiranje
pretrazivanjeElemenataSekundarnih ps pp sgp spp = Parsiranje {
    getGlavnoPolje = getGlavnoPolje ps,
    getIndikator = getIndikator ps,
    getListu = praznuListu (getListu ps),
    getListaPotPolja = pretragaSekundarnih (getListaPotPolja ps) pp sgp spp }


pretragaSekundarnih :: [PotParsiranje] -> String -> Int -> String -> [PotParsiranje]
pretragaSekundarnih pps pp sgp spp = fmap prolazak pps
    where
        prolazak :: PotParsiranje -> PotParsiranje
        prolazak p 
            | (getPotPoljePolja p == pp) && (getGlPolje p == sgp) = prolazakSekunarnihPolja p sgp spp
            | otherwise = praznoSekundarnoParsiranje p


praznoSekundarnoParsiranje :: PotParsiranje -> PotParsiranje
praznoSekundarnoParsiranje ps = PotParsiranje {
    getPotPoljePolja = getPotPoljePolja ps,
    getGlPolje = getGlPolje ps,
    getInd = getInd ps,
    getTupl = praznuListu (getTupl ps) }

prolazakSekunarnihPolja :: PotParsiranje -> Int -> String -> PotParsiranje
prolazakSekunarnihPolja ps sgp spp = PotParsiranje {
    getPotPoljePolja = getPotPoljePolja ps,
    getGlPolje = getGlPolje ps,
    getInd = getInd ps,
    getTupl = pretragaKrozListu (getTupl ps) spp }


------------------------------------
glavnoPolje :: Parsec String () Int
glavnoPolje = do spaces
                 num <- many1 digit
                 char ' '
                 return $ read num

indikator :: Parsec String () String
indikator = do spaces
               ind <- many1 (oneOf "0123456789#")
               char ' '
               return $ ind


potpolja :: Parsec String () String
potpolja = do spaces
              char '['
              slovo <- many1 $ try(letter) <|> digit
              char ']'
              spaces
              notFollowedBy (string "{{")
              return $ slovo

potpolja2 :: Parsec String () String
potpolja2 = do spaces
               char '['
               slovo <- many1 $ try(letter) <|> digit
               char ']'
               spaces
               string "{{"
               return $ slovo


sadrzaj :: Parsec String () String
sadrzaj = do spaces 
             sad <- manyTill anyChar $ try (string "  ")
             return $ sad

sadrzajSaNavodnicima :: Parsec String () String
sadrzajSaNavodnicima = do spaces
                          string "\""
                          sad <- manyTill anyChar $ try (string "\"")
                          spaces
                          return sad


svaListaPotpodpolja :: Parsec String () [Tupl]
svaListaPotpodpolja = do pp1 <- try (listaPotpodpolja2) <|> listaPotpodpolja
                         pp2 <- many $ try (listaPotpodpolja2) <|> listaPotpodpolja
                         return (pp1:pp2)


listaPotpodpolja :: Parsec String () Tupl
listaPotpodpolja = do spaces
                      slovo <- many1 letter
                      spaces
                      sad <- manyTill anyChar $ try (char ' ')
                      spaces
                      return (slovo, sad)

listaPotpodpolja2 :: Parsec String () Tupl
listaPotpodpolja2 = do spaces
                       slovo <- many1 letter    
                       spaces   
                       string "\""
                       sad <- manyTill anyChar $ try (string "\"")
                       spaces
                       return (slovo, sad)

listaPotpolja :: Parsec String () Tupl
listaPotpolja = do po <- potpolja
                   sa <- try sadrzajSaNavodnicima <|> sadrzaj 
                   return (po, sa)

listaSvega :: Parsec String () [Tupl]
listaSvega = do l1 <- listaPotpolja
                l2 <- many $ try (listaPotpolja)
                return (l1:l2)


listaSvegaPot :: Parsec String () PotParsiranje
listaSvegaPot = do spaces
                   polje <- potpolja2
                   spaces
                   podpodP <- many1 digit
                   char ' '
                   ind <- many1 $ oneOf ("0123456789#")
                   string "  "
                   pp <- svaListaPotpodpolja
                   spaces
                   string "}}"
                   return $ PotParsiranje polje (read podpodP) ind pp

listaSvegaPotpolja :: Parsec String () [PotParsiranje]
listaSvegaPotpolja = do l1 <- listaSvegaPot
                        l2 <- many $ try (listaSvegaPot)
                        return (l1:l2)

parsiranje1 :: Parsec String () Parsiranje
parsiranje1 = do gp <- glavnoPolje
                 id <- indikator
                 li <- (try listaSvega) <|> return []
                 pa <- (try listaSvegaPotpolja) <|> return []
                 return $ Parsiranje gp id li pa

parsiranje :: Parsec String () [Parsiranje]
parsiranje = do p1 <- try(parsiranje1)
                p2 <- many $ try(parsiranje1)
                return (p1:p2)


-- :run main "test1.txt" "bb.txt"
main :: IO ()
main = do (input:output:[]) <- getArgs
          content <- readFile input
          let ps = parse parsiranje input content
          case ps of
            Left err -> print err
            Right par -> rekurzivnoUnosenje par output

rekurzivnoUnosenje :: [Parsiranje] -> String -> IO ()
rekurzivnoUnosenje par output = do putStrLn "Unesi komandu"
                                   linija <- fmap words getLine
                                   if null linija 
                                       then return ()
                                       else do komanda <- odradiKomandu par linija
                                               let file = toString $ komanda
                                               writeFile output file
                                               rekurzivnoUnosenje komanda output


odradiKomandu :: [Parsiranje] -> [String] -> IO [Parsiranje]
odradiKomandu ps ["dodajPotpolje", gp, pp, sad] = do
    let gp' = read gp :: Int
        ps' = dodajPotpolje ps gp' pp sad
    return ps'
odradiKomandu ps ["dodavanjeUSekundarnaPolja", gp, sgp, pp, sad] = do
    let gp' = read gp :: Int
        sgp' = read sgp :: Int
        ps' = dodavanjeUSekundarnaPolja ps gp' sgp' pp sad
    return ps'
odradiKomandu ps ["obrisiPotpolje", gp, pp] = do
    let gp' = read gp :: Int
        ps' = obrisiPotpolje ps gp' pp
    return ps'
odradiKomandu ps ["brisanjeUSekundarnimPoljima", gp, sgp, pp] = do
    let gp' = read gp :: Int
        sgp' = read sgp :: Int
        ps' = brisanjeUSekundarnimPoljima ps gp' sgp' pp
    return ps'
odradiKomandu ps ["promenaSadrzajaPotpolja", gp, pp, noviSadrzaj] = do
    let gp' = read gp :: Int
        ps' = promenaSadrzajaPotpolja ps gp' pp noviSadrzaj
    return ps'
odradiKomandu ps ["promenaSadrzajaSekundarnogPolja", gp, sgp, pp, noviSadrzaj] = do
    let gp' = read gp :: Int
        sgp' = read sgp :: Int
        ps' = promenaSadrzajaSekundarnogPolja ps gp' sgp' pp noviSadrzaj
    return ps'
odradiKomandu ps ["pretrazivanjeElemenataPotPolja", gp, pp] = do
    let gp' = read gp :: Int
        ps' = pretrazivanjeElemenataPotPolja ps gp' pp
    return ps'
odradiKomandu ps ["pretrazivanjeElemenataSekundarnihPolja", gp, pp, sgp, spp] = do
    let gp' = read gp :: Int
        sgp' = read sgp :: Int
        ps' = pretrazivanjeElemenataSekundarnihPolja ps gp' pp sgp' spp
    return ps'
odradiKomandu ps ["sacuvajUJSON", outputFile] = do 
    let json = parsiranjaUJSON ps
        file = outputFile ++ ".txt"
    writeFile file json
    return ps
odradiKomandu ps cmd = do
    print $ "Nepoznata komanda: " ++ (show cmd)
    return ps

parsiranjaUJSON :: [Parsiranje] -> String
parsiranjaUJSON ps =  (intercalate ",\n" . fmap toJSON $ ps)  

parsiranjaUJSONPot :: [PotParsiranje] -> String
parsiranjaUJSONPot ps =  (intercalate ",\n" . fmap toJSON $ ps)  
