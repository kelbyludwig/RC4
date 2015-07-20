import Data.List
import Data.Char (ord)
import qualified Data.ByteString as B
import qualified Data.Word8 as W8
import qualified Data.ByteString.Base16 as H

ksa :: B.ByteString -> B.ByteString
ksa key = ksa' key 0 [W8._nul..] 0

ksa' :: B.ByteString -> Int -> [W8.Word8] -> Int -> B.ByteString
ksa' key j s 256 = B.pack s
ksa' key j s i   = ksa' key j' (swap i j' s) (i+1)
                    where unkey  = B.unpack key
                          keylen = B.length key
                          j'     = (j + (fromIntegral (s !! i) :: Int) + 
                                   (fromIntegral (unkey !! (i `mod` keylen)) :: Int)) `mod` 256

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
        putStrLn $ show key
        putStrLn $ show $ (H.encode $ ksa key, B.length $ ksa key)
