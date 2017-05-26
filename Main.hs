import Data.List
import Data.Ord (comparing)
import Data.Char (ord)

english = 
     [ 0.08167, 0.01492, 0.02782, 0.04253, 
       0.12702, 0.02228, 0.02015, 0.06094, 
       0.06966, 0.00153, 0.00772, 0.04025, 
       0.02406, 0.06749, 0.07507, 0.01929, 
       0.00095, 0.05987, 0.06327, 0.09056, 
       0.02758, 0.00978, 0.02360, 0.00150, 
       0.01974, 0.00074 ] 

crypt = "\
    \MOMUD EKAPV TQEFM OEVHP AJMII CDCTI FGYAG JSPXY ALUYM NSMYH\
    \VUXJE LEPXJ FXGCM JHKDZ RYICU HYPUS PGIGM OIYHF WHTCQ KMLRD\
    \ITLXZ LJFVQ GHOLW CUHLO MDSOE KTALU VYLNZ RFGBX PHVGA LWQIS\
    \FGRPH JOOFW GUBYI LAPLA LCAFA AMKLG CETDW VOELJ IKGJB XPHVG\
    \ALWQC SNWBU BYHCU HKOCE XJEYK BQKVY KIIEH GRLGH XEOLW AWFOJ\
    \ILOVV RHPKD WIHKN ATUHN VRYAQ DIVHX FHRZV QWMWV LGSHN NLVZS\
    \JLAKI FHXUF XJLXM TBLQV RXXHR FZXGV LRAJI EXPRV OSMNP KEPDT\
    \LPRWM JAZPK LQUZA ALGZX GVLKL GJTUI ITDSU REZXJ ERXZS HMPST\
    \MTEOE PAPJH SMFNB YVQUZ AALGA YDNMP AQOWT UHDBV TSMUE UIMVH\
    \QGVRW AEFSP EMPVE PKXZY WLKJA GWALT VYYOB YIXOK IHPDS EVLEV\
    \RVSGB JOGYW FHKBL GLXYA MVKIS KIEHY IMAPX UOISK PVAGN MZHPW\
    \TTZPV XFCCD TUHJH WLAPF YULTB UXJLN SIJVV YOVDJ SOLXG TGRVO\
    \SFRII CTMKO JFCQF KTINQ BWVHG TENLH HOGCS PSFPV GJOKM SIFPR\
    \ZPAAS ATPTZ FTPPD PORRF TAXZP KALQA WMIUD BWNCT LEFKO ZQDLX\
    \BUXJL ASIMR PNMBF ZCYLV WAPVF QRHZV ZGZEF KBYIO OFXYE VOWGB\
    \BXVCB XBAWG LQKCM ICRRX MACUO IKHQU AJEGL OIJHH XPVZW JEWBA\
    \FWAML ZZRXJ EKAHV FASMU LVVUT TGK\
    \"

breakup :: Int -> [a] -> [[a]]
breakup _ [] = []
breakup n as = 
    let (h, r) = splitAt n as
    in h:breakup n r

distribute :: Int -> [a] -> [[a]]
distribute n as = transpose $ breakup n as

coincProb :: String -> Float
coincProb str = 
    let alphabet = nub $ sort str
        ccs = fmap (\ch -> (length.filter (==ch)) str) alphabet
        strln = length str
        d = fromIntegral $ strln * (strln - 1)
        n = fromIntegral $ sum $ fmap (\cc -> cc * (cc-1)) ccs
    in n / d

mean :: [Float] -> Float
mean as = sum as / fromIntegral (length as)
 

coincIndex :: Int -> String -> Float
coincIndex n as =  
    -- The correlation increases artificially for smaller
    -- pieces/longer keys, so weigh against them a little
    let offsetCheat = fromIntegral n / 3000.0
    in mean (fmap coincProb (distribute n as)) - offsetCheat

-- find decoding offset that results in best match 
-- between actual char frequencies and expected frequencies
getKeyChar :: [Float] -> String -> Char
getKeyChar expected caesar =
    let actual = fmap (\ch -> (fromIntegral.length.filter (==ch)) caesar) ['A'..'Z']
        dots = fmap (\ltr -> (ltr, sum $ zipWith (*) expected (drop (ord ltr - ord 'A') (cycle actual))) ) ['A'..'Z']
    in fst $ maximumBy (comparing snd) dots

-- Add two upper case letters using modulo arithmetic
alphaSum :: Char -> Char -> Char
alphaSum a b = 
   cycle ['A'..'Z'] !! (26 + (ord b - ord a))

-- given a key and cipher text encoded with that key, return the plaintext
decode :: String -> String -> String
decode key = zipWith alphaSum (cycle key) 

main = do
    let cr = filter (/=' ') crypt
        -- Assume that if there are less than 20 characters
        -- per column, the key's too long to guess
        cIndices = fmap (\n -> (n, coincIndex n cr)) [1..length cr `div` 20]
        keyLen = fst $ maximumBy (comparing snd) cIndices
        ceasars = distribute keyLen cr
        key = fmap (getKeyChar english) ceasars
    mapM_ putStrLn [key, decode key cr]
