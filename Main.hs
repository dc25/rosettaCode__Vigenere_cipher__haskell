import Data.List(transpose, nub, sort, maximumBy)
import Data.Ord (comparing)
import Data.Char (ord)

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
coincidence :: String -> Float
coincidence str = 
    let charCounts = fmap (countChar str) (nub $ sort str)
        strln = length str
        d = fromIntegral $ strln * (strln - 1)
        n = fromIntegral $ sum $ fmap (\cc -> cc * (cc-1)) charCounts
    in n / d

-- Use the average probablity of coincidence for all the members of
-- a distribution to rate the distribution - the higher the better.
-- The correlation increases artificially for smaller
-- pieces/longer keys, so weigh against them a little
rate :: [String] -> Float
rate d =  average (fmap coincidence d) - fromIntegral (length d) / 3000.0 

-- Multiply elements of lists together and add up the results.
dot :: [Float] -> [Float] -> Float
dot v0 v1 = sum $ zipWith (*) v0 v1

-- Given two lists of floats, rotate one of them by the number of
-- characters indicated by letter and then 'dot' them together.
rotateAndDot :: [Float] -> [Float] -> Char -> Float
rotateAndDot v0 v1 letter = dot v0 (drop (ord letter - ord 'A') (cycle v1))  

-- Find decoding offset that results in best match 
-- between actual char frequencies and expected frequencies.
getKeyChar :: [Float] -> String -> Char
getKeyChar expected sample =
    let actual = fmap (fromIntegral . countChar sample) ['A'..'Z']
    in maximumBy (comparing $ rotateAndDot expected actual) ['A'..'Z']

main = do
    let cr = filter (/=' ') crypt
        -- Assume that if there are less than 20 characters
        -- per column, the key's too long to guess
        distributions = fmap (distribute cr) [1..length cr `div` 20]
        bestDistribution = maximumBy (comparing rate) distributions
        key = fmap (getKeyChar englishFrequencies) bestDistribution
        alphaSum a b = cycle ['A'..'Z'] !! (26 + (ord b - ord a))
    mapM_ putStrLn ["Key: " ++ key, "Decrypted Text: " ++ zipWith alphaSum (cycle key) cr]

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

