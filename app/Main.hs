module Main where
import System.Environment
import qualified Data.ByteString.Lazy as BS
import Coder ( compress )
import Decoder ( decompress )

main = do
   [mode, inpFile, outFile] <- getArgs
   inp <- BS.readFile inpFile
   let inpUnp = BS.unpack inp
   let otputStr=[]
   let otputStr
         | mode == "compress" = compress inpUnp
         | mode == "decompress" = decompress inpUnp
         | otherwise = error "Wrong mode"
   let out = BS.pack otputStr
   BS.writeFile outFile out
   putStrLn "Complete :)"