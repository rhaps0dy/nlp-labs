import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Applicative ((<$>))

unknown = T.pack "Unknown"

classifyWords :: M.Map T.Text T.Text -> [T.Text] -> [(T.Text, T.Text)]
classifyWords m = map (\w -> (w, fromMaybe unknown $ M.lookup w m))

readMap :: [T.Text] -> M.Map T.Text (M.Map T.Text Int)
readMap [] = M.empty
readMap (word:cat:textn:rest) =
    let wcount = readMap rest
        n = read (T.unpack textn) :: Int
        inmap = fromMaybe M.empty (M.lookup word wcount)
        inmap' = M.insert cat n inmap
    in  M.insert word inmap' wcount

processMap :: M.Map T.Text (M.Map T.Text Int) -> M.Map T.Text T.Text
processMap = M.map (fst . processMap)
    where processMap = foldl (\r1@(_, n1) r2@(_, n2) -> if n1 > n2 then r1 else r2) (unknown, 0) . M.toList

-- different, length
diff :: Eq a => [a] -> [a] -> (Int, Int)
diff [] [] = (0, 0)
diff (x:xs) (y:ys) =
    let (wrong, total) = diff xs ys
        addwrong = if x /= y then 1 else 0
    in  (addwrong + wrong, 1 + total)

group2 :: [a] -> [(a, a)]
group2 [] = []
group2 (x:y:rest) = (x, y):(group2 rest)

filetowords fp = (T.words . decodeLatin1) <$> B.readFile fp



main = do
    inp <- filetowords "test_2.txt"
    out <- filetowords "gold_standard_2.txt"
    wmap <- processMap . readMap <$> filetowords "lexic.txt"
    let classified = classifyWords wmap inp
        standard = group2 out
--    B.writeFile ("test_" ++ n ++ "_classified.txt"
    print $ diff classified standard