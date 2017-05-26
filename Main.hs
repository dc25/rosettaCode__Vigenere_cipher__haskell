import Data.List
import Data.Ord (comparing)
import Data.Char (ord)

englishFrequencies = 
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

average :: [Float] -> Float
average as = sum as / fromIntegral (length as)

-- Count the occurrence of a char in a string.
countChar :: String -> Char -> Int
countChar s c = length $ filter (==c) s

-- Break a string up into substrings of n chars.
breakup :: Int -> [a] -> [[a]]
breakup _ [] = []
breakup n as = 
    let (h, r) = splitAt n as
    in h:breakup n r

-- Dole out elements of a string over a n element distribution.
distribute :: [a] -> Int -> [[a]]
distribute as n = transpose $ breakup n as

-- The probability that members of a pair of characters taken randomly
-- from a given string are equal.
coincProb :: String -> Float
coincProb str = 
    let alphabet = nub $ sort str
        ccs = fmap (countChar str) alphabet
        strln = length str
        d = fromIntegral $ strln * (strln - 1)
        n = fromIntegral $ sum $ fmap (\cc -> cc * (cc-1)) ccs
    in n / d

-- Use the average probablity of coincidence for all the members of
-- a distribution to rate the distribution - the higher the better.
-- The correlation increases artificially for smaller
-- pieces/longer keys, so weigh against them a little
rate :: [String] -> Float
rate d =  average (fmap coincProb d) - fromIntegral (length d) / 3000.0 

-- find decoding offset that results in best match 
-- between actual char frequencies and expected frequencies
getKeyChar :: [Float] -> String -> Char
getKeyChar expectedFrequencies encodedString =
    let actualFrequencies = fmap (fromIntegral . countChar encodedString) ['A'..'Z']
        dots = fmap (\ltr -> (ltr, sum $ zipWith (*) expectedFrequencies (drop (ord ltr - ord 'A') (cycle actualFrequencies))) ) ['A'..'Z']
    in fst $ maximumBy (comparing snd) dots

-- Add two upper case letters using modulo arithmetic
alphaSum :: Char -> Char -> Char
alphaSum a b = cycle ['A'..'Z'] !! (26 + (ord b - ord a))

-- given a key and cipher text encoded with that key, return the plaintext
decode :: String -> String -> String
decode key = zipWith alphaSum (cycle key) 

main = do
    let cr = filter (/=' ') crypt
        -- Assume that if there are less than 20 characters
        -- per column, the key's too long to guess
        distributions = fmap (distribute cr) [1..length cr `div` 20]
        ratedDistributions = fmap (\d -> (d, rate d)) distributions
        bestDistribution = fst $ maximumBy (comparing snd) ratedDistributions
        key = fmap (getKeyChar englishFrequencies) bestDistribution
    mapM_ putStrLn [key, decode key cr]
