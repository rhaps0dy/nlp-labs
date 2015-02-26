import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T

countWords :: [T.Text] -> M.Map T.Text (M.Map T.Text Int)
countWords [] = M.empty
countWords (word:t:rest) =
    let w = T.toLower word
        wcount = countWords rest
        inmap = fromMaybe M.empty (M.lookup w wcount)
        curcount = fromMaybe 0 (M.lookup t inmap)
        inmap' = M.insert t (curcount+1) inmap
    in  M.insert w inmap' wcount

formatEntry :: (T.Text, Int) -> T.Text
formatEntry (cat, n) = T.concat [T.pack "\t", cat, T.pack "\t", T.pack (show n), T.pack "\n"]

formatCount :: M.Map T.Text (M.Map T.Text Int) -> T.Text
formatCount wcount =
    let wcountList = M.toList wcount
        wcountExtended = map (\(w, m) -> (w, M.toList m)) wcountList
        formatWord (w, entries) = T.concat $ w:(intersperse w $ map formatEntry entries)
    in  T.concat . map formatWord $ wcountExtended
        
main = do
     inp <- B.readFile "corpus.txt"
     let ws = T.words . decodeLatin1 $ inp
         count = countWords ws
         out = encodeUtf8 . formatCount $ count
     B.writeFile "lexic.txt" out