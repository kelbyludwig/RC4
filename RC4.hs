import Data.List
import Data.Char (ord)
import Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Data.Word8 as W8
import qualified Data.ByteString.Base16 as H

data PRGA = PRGA { i :: Int
                 , j :: Int
                 , s :: [W8.Word8]
                 , x :: B.ByteString
                 }

ksa :: B.ByteString -> [W8.Word8]
ksa key = ksa' key 0 [W8._nul..] 0

ksa' :: B.ByteString -> Int -> [W8.Word8] -> Int -> [W8.Word8] 
ksa' key j s 256 = s
ksa' key j s i   = ksa' key j' (swap i j' s) (i+1)
                    where unkey  = B.unpack key
                          keylen = B.length key
                          j'     = (j + (fromIntegral (s !! i) :: Int) + 
                                   (fromIntegral (unkey !! (i `mod` keylen)) :: Int)) `mod` 256

prga :: [W8.Word8] -> Int -> B.ByteString
prga st l = prga' state l
              where state = PRGA { i = 0, j = 0, s = st, x = B.empty }

prga' :: PRGA -> Int -> B.ByteString
prga' (PRGA _ _ _ x) 0   = x
prga' (PRGA i j s x) len = prga' (PRGA i' j' s' x') (len-1)
                            where i' = (i + 1) `mod` 256
                                  j' = (j + (fromIntegral (s !! i'))) `mod` 256
                                  s' = swap i' j' s
                                  k  = s' !! (((fromIntegral (s' !! i')) + (fromIntegral (s' !! j'))) `mod` 256)
                                  x' = B.snoc x k

--https://mail.haskell.org/pipermail/haskell-cafe/2005-May/009859.html
swap :: Int -> Int -> [W8.Word8] -> [W8.Word8]
swap i j xs | i == j    = xs
swap i j xs | otherwise = initial ++ (xs !! y) : middle ++ (xs !! x) : end 
    where [x,y]   = sort [i,j]
          initial = take x xs
          middle  = take (y-x-1) (drop (x+1) xs)
          end     = drop (y+1) xs

main :: IO ()
main = do
        let packer = B.pack . map (fromIntegral . ord)
        let key    = packer "Secret"
        let mes    = packer "Attack at dawn"
        let exkey  = ksa key
        let keystm = prga exkey (B.length mes)
        putStrLn $ show $ (H.encode $ keystm)
        let cipher = (B.pack . B.zipWith xor keystm) mes
        putStrLn $ show $ (H.encode $ cipher)
